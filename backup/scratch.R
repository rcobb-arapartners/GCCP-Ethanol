library(tidyverse)
library(data.table)
library(lubridate)
library(janitor)
library(rvest)
library(DBI)
library(lpSolve)
library(scales)
library(broom)
library(gtsummary)
library(gt)
library(grid)
library(ggExtra)
library(rJava)
library(RJDBC)
library(zoo)
library(glue)
theme_set(theme_bw())
########################################################
# Build function for API calls for historical corn and ethanol forwards
########################################################

build_marketview_daily_forward_data_on_corn_months <- function(
    base_symbols   = c("GCU","ZC"),
    calendar_years = c(2007:2026),
    month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
    min_date       = "2000-01-01",
    max_date       = Sys.Date(),
    as_of_date = '2026-02-06') {
  
  ethanol_month_lookup <- c(
    F = 1,
    G = 2,
    H = 3,
    J = 4,
    K = 5,
    M = 6,
    N = 7,
    Q = 8,
    U = 9,
    V = 10,
    X = 11,
    Z = 12
  )
  
  
  
  
  
  endpoint <- "https://webservice.gvsi.com/api/v3/getdaily"
  fields_encoded <- "symbol%2Cdescription%2Cupdatetype%2Clastupdatetime%2Cclose%2Chigh%2Clow%2Copen%2Copeninterest%2Ctradedatetimeutc%2Cvolume%2Cweight"
  output_piece  <- "output=csv&includeheaders=true"
  
  min_date_enc <- URLencode(format(as.Date(min_date), "%m/%d/%Y"), reserved = TRUE)
  max_date_enc <- URLencode(format(as.Date(max_date), "%m/%d/%Y"), reserved = TRUE)
  
  year_to_code <- function(y) substr(as.character(y), 3, 4)
  
  # Build ONE big symbols list: /GCUF25,...,/ZCZ26 etc.
  contracts <- expand_grid(
    base_symbol   = base_symbols,
    calendar_year = calendar_years,
    month_code    = month_codes
  ) |>
    mutate(
      year_code = year_to_code(calendar_year),
      contract_symbol_raw = paste0("/", base_symbol, month_code, year_code)
    )
  
  symbols_raw <- paste(unique(contracts$contract_symbol_raw), collapse = ",")
  symbols_enc <- URLencode(symbols_raw, reserved = TRUE)
  
  url <- paste0(
    endpoint,
    "?symbols=", symbols_enc,
    "&fields=", fields_encoded,
    "&", output_piece,
    "&startdate=", min_date_enc,
    "&enddate=", max_date_enc,
    "&username=", Sys.getenv("ara_marketview_api_username"),
    "&password=", Sys.getenv("ara_marketview_api_password")
  )
  
  #print(url)
  
  full_data_raw <- read_csv(url, show_col_types = FALSE)
  
  full_data <- full_data_raw |>
    mutate(symbol_norm = ifelse(startsWith(symbol, "/"), symbol, paste0("/", symbol))) |>
    left_join(
      contracts |>
        distinct(contract_symbol_raw, base_symbol, calendar_year, year_code, month_code) |>
        mutate(symbol_norm = contract_symbol_raw) |>
        select(-contract_symbol_raw),
      by = "symbol_norm"
    ) |>
    select(-symbol_norm) |>
    mutate(
      DATE_ID = as.Date(mdy_hms(tradedatetimeutc)),
      # month code -> month number
      MONTH_NUM = case_when(
        month_code == "F" ~ 1L,
        month_code == "G" ~ 2L,
        month_code == "H" ~ 3L,
        month_code == "J" ~ 4L,
        month_code == "K" ~ 5L,
        month_code == "M" ~ 6L,
        month_code == "N" ~ 7L,
        month_code == "Q" ~ 8L,
        month_code == "U" ~ 9L,
        month_code == "V" ~ 10L,
        month_code == "X" ~ 11L,
        month_code == "Z" ~ 12L,
        TRUE ~ NA_integer_
      ),
      EXPIRY_CEILING_DATE = ceiling_date(ymd(sprintf("%d-%02d-01", calendar_year, MONTH_NUM)), unit = "month") - days(1)
    ) |>
    filter(DATE_ID <= EXPIRY_CEILING_DATE) |>
    select(-MONTH_NUM, -EXPIRY_CEILING_DATE) |> 
    rename_all(toupper) |> 
    select(-c(UPDATETYPE,TRADEDATETIMEUTC,LASTUPDATETIME)) |> 
    mutate(
      DATE_ID = as.Date(DATE_ID),
      YEAR_FULL = 2000 + as.numeric(YEAR_CODE),
      MONTH_NUMBER = ethanol_month_lookup[MONTH_CODE],
      CONTRACT_DATE_ID = as.Date(paste(YEAR_FULL, MONTH_NUMBER, "01", sep = "-")),
      TRADING_MONTH = floor_date(DATE_ID, "months")
    ) |>
    select(SYMBOL,DESCRIPTION,BASE_SYMBOL,CALENDAR_YEAR,YEAR_CODE,MONTH_CODE,DATE_ID,everything()) |> 
    mutate(EXPIRED = if_else(as.Date(as_of_date) > ceiling_date(CONTRACT_DATE_ID,'months') - days(1),1,0)) 
  
  
  return(full_data)
  
  
}
########################################################
# Call Ethanol for all months
########################################################
ethanol_marketview_daily_forward_curves <- build_marketview_daily_forward_data_on_corn_months(base_symbols   = c("GCU"),
                                                                                              calendar_years = c(2007:2026),
                                                                                              month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                                                                              min_date       = "2000-01-01",
                                                                                              max_date       = Sys.Date(),
                                                                                              as_of_date = '2026-02-06')

########################################################
# Call Corn for only select months
########################################################
corn_marketview_daily_forward_curves <- build_marketview_daily_forward_data_on_corn_months(base_symbols   = c("ZC"),
                                                                                           calendar_years = c(2007:2026),
                                                                                           month_codes    = c("H","K","N","U","Z"),
                                                                                           min_date       = "2000-01-01",
                                                                                           max_date       = Sys.Date(),
                                                                                           as_of_date = '2026-02-06')

########################################################
# Generate Liquidations OFF Corn Months
########################################################
generate_off_month_prices <- function(marketview_ethanol_df = ethanol_marketview_daily_forward_curves,
                                      marketview_corn_df = corn_marketview_daily_forward_curves,
                                      as_of_date = '2026-02-06',
                                      yield = 2.9) {
  off_month_ethanol_prices <- marketview_ethanol_df |> 
    filter(DATE_ID <= as_of_date) |> 
    group_by(DESCRIPTION) |>
    mutate(
      PRIOR_MONTH_START = floor_date(CONTRACT_DATE_ID %m-% months(1), unit = "month"),
      PRIOR_MONTH_END   = CONTRACT_DATE_ID - days(1)
    ) |>
    mutate(
      LAST_PRIOR_DATE = suppressWarnings(
        max(DATE_ID[DATE_ID >= PRIOR_MONTH_START & DATE_ID <= PRIOR_MONTH_END], na.rm = TRUE)
      ),
      HAS_PRIOR_MONTH = is.finite(LAST_PRIOR_DATE),
      keep_row = if_else(
        MONTH_CODE %in% c("H","K","N","U","Z") & HAS_PRIOR_MONTH,
        DATE_ID == LAST_PRIOR_DATE,
        TRUE)
    ) |>
    ungroup() |>
    filter(keep_row) |>
    select(DESCRIPTION, YEAR_CODE, MONTH_CODE, DATE_ID, CONTRACT_DATE_ID, CLOSE,EXPIRED) |> 
    group_by(DESCRIPTION) |> 
    filter(DATE_ID == max(DATE_ID)) |> 
    ungroup() |> 
    rename(ETHANOL_PRICE_OFF_MONTH = CLOSE)
  
  
  #first, do expired contracts
  ethanol_off_df_expired <- off_month_ethanol_prices |> 
    filter(EXPIRED == 1)
  full_relevant_corn_off_prices_expired <- data.frame()
  for(d in unique(ethanol_off_df_expired$DESCRIPTION)) {
    print(d)
    sub_ethanol_off_df_expired <- ethanol_off_df_expired |>
      filter(DESCRIPTION == d)
    ethanol_year_code <- sub_ethanol_off_df_expired |>
      pull(YEAR_CODE)
    ethanol_month_code <- sub_ethanol_off_df_expired |>
      pull(MONTH_CODE)
    marketview_corn_df_filtered <- marketview_corn_df |> 
      filter(DATE_ID <= as_of_date)
    
    
    if(ethanol_month_code == 'F') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'January') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'G') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'February') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'H') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'February') |> 
        mutate(RELEVANT_CORN_PRICE = last(CLOSE),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'J') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'K',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'April') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'K') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'K',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'April') |> 
        mutate(RELEVANT_CORN_PRICE = last(CLOSE),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'M') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'N',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'June') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'N') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'N',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'June') |> 
        mutate(RELEVANT_CORN_PRICE = last(CLOSE),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'Q') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'U',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'August') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'U') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'U',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'August') |> 
        mutate(RELEVANT_CORN_PRICE = last(CLOSE),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'V') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'October') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'X') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'November') |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        mutate(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    if(ethanol_month_code == 'Z') {
      sub_marketview_corn_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z',
               YEAR_CODE == ethanol_year_code) |> 
        filter(months(TRADING_MONTH) == 'November') |> 
        mutate(RELEVANT_CORN_PRICE = last(CLOSE),
               ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION,RELEVANT_CORN_PRICE) |> 
        distinct()
    }
    
    full_relevant_corn_off_prices_expired <- full_relevant_corn_off_prices_expired |> 
      bind_rows(sub_marketview_corn_expired_df_filtered)
    
  }
  
  #now, do non-expired contracts
  
  
  ethanol_off_df_non_expired <- off_month_ethanol_prices |> 
    filter(EXPIRED == 0)
  full_relevant_corn_off_prices_non_expired <- data.frame()
  for(d in unique(ethanol_off_df_non_expired$DESCRIPTION)) {
    print(d)
    sub_ethanol_off_df_non_expired <- ethanol_off_df_non_expired |>
      filter(DESCRIPTION == d)
    ethanol_year_code <- sub_ethanol_off_df_non_expired |>
      pull(YEAR_CODE)
    ethanol_month_code <- sub_ethanol_off_df_non_expired |>
      pull(MONTH_CODE)
    
    marketview_corn_df_filtered <- marketview_corn_df |> 
      filter(DATE_ID <= as_of_date,
             YEAR_CODE %in% ethanol_off_df_non_expired$YEAR_CODE,
             MONTH_CODE %in% ethanol_off_df_non_expired$MONTH_CODE) |> 
      group_by(DESCRIPTION) |> 
      filter(DATE_ID == max(DATE_ID)) |> 
      ungroup()
    
    if(ethanol_month_code == 'F') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'G') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'H') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'H') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'J') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'K') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'K') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'K') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'M') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'N') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'N') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'N') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'Q') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'U') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'U') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'U') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'V') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'X') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    if(ethanol_month_code == 'Z') {
      sub_marketview_corn_non_expired_df_filtered <- marketview_corn_df_filtered |> 
        filter(MONTH_CODE == 'Z') |> 
        mutate(ETHANOL_DESCRIPTION = d) |> 
        select(ETHANOL_DESCRIPTION, RELEVANT_CORN_PRICE = CLOSE) 
    }
    
    full_relevant_corn_off_prices_non_expired <- full_relevant_corn_off_prices_non_expired |> 
      bind_rows(sub_marketview_corn_non_expired_df_filtered)
  }
  
  full_relevant_corn_off_prices <- full_relevant_corn_off_prices_expired |> 
    bind_rows(full_relevant_corn_off_prices_non_expired)
  
  
  off_month_prices <- off_month_ethanol_prices |> 
    inner_join(full_relevant_corn_off_prices,by = c('DESCRIPTION' = 'ETHANOL_DESCRIPTION')) |> 
    mutate(CRUSH_SPREAD = ETHANOL_PRICE_OFF_MONTH - (RELEVANT_CORN_PRICE/100/yield)) |> 
    rename(RELEVANT_CORN_PRICE_OFF_MONTH = RELEVANT_CORN_PRICE,
           CRUSH_SPREAD_OFF_MONTH = CRUSH_SPREAD)
  
  
  return(off_month_prices)
  
  
}
off_month_prices <- generate_off_month_prices(as_of_date = '2026-02-06')

########################################################
# Generate Liquidations ON Corn Months
########################################################
generate_on_month_prices <- function(input_ethanol_df = ethanol_marketview_daily_forward_curves,
                                     input_corn_df    = corn_marketview_daily_forward_curves,
                                     as_of_date       = '2026-02-06',
                                     yield = 2.9) {
  
  as_of_date <- as.Date(as_of_date)
  
  ethanol_df <- input_ethanol_df |>
    rename(CONTRACT_YEAR = CALENDAR_YEAR) |>
    mutate(
      CONTRACT_MONTH = MONTH_CODE,
      
      CONTRACT_MONTH_NAME = case_when(
        CONTRACT_MONTH == "F" ~ "January",
        CONTRACT_MONTH == "G" ~ "February",
        CONTRACT_MONTH == "H" ~ "March",
        CONTRACT_MONTH == "J" ~ "April",
        CONTRACT_MONTH == "K" ~ "May",
        CONTRACT_MONTH == "M" ~ "June",
        CONTRACT_MONTH == "N" ~ "July",
        CONTRACT_MONTH == "Q" ~ "August",
        CONTRACT_MONTH == "U" ~ "September",
        CONTRACT_MONTH == "V" ~ "October",
        CONTRACT_MONTH == "X" ~ "November",
        CONTRACT_MONTH == "Z" ~ "December",
        TRUE ~ NA_character_
      ),
      
      # ethanol -> corn month alignment (corrected)
      RELEVANT_CORN_CONTRACT_MONTH = case_when(
        CONTRACT_MONTH == "Z" ~ "H",
        CONTRACT_MONTH == "X" ~ "Z",
        CONTRACT_MONTH == "V" ~ "Z",
        CONTRACT_MONTH == "U" ~ "Z",
        CONTRACT_MONTH == "Q" ~ "U",
        CONTRACT_MONTH == "N" ~ "U",
        CONTRACT_MONTH == "M" ~ "N",
        CONTRACT_MONTH == "K" ~ "N",
        CONTRACT_MONTH == "J" ~ "K",
        CONTRACT_MONTH == "H" ~ "K",
        CONTRACT_MONTH == "G" ~ "H",
        CONTRACT_MONTH == "F" ~ "H",
        TRUE ~ NA_character_
      ),
      
      # year crossover: Dec ethanol -> next year's Mar corn
      RELEVANT_CORN_CONTRACT_YEAR =
        if_else(CONTRACT_MONTH == "Z", CONTRACT_YEAR + 1L, CONTRACT_YEAR),
      
      # ethanol expiry ceiling (month-end proxy)
      ETHANOL_MONTH_NUM = case_when(
        CONTRACT_MONTH == "F" ~ 1L,
        CONTRACT_MONTH == "G" ~ 2L,
        CONTRACT_MONTH == "H" ~ 3L,
        CONTRACT_MONTH == "J" ~ 4L,
        CONTRACT_MONTH == "K" ~ 5L,
        CONTRACT_MONTH == "M" ~ 6L,
        CONTRACT_MONTH == "N" ~ 7L,
        CONTRACT_MONTH == "Q" ~ 8L,
        CONTRACT_MONTH == "U" ~ 9L,
        CONTRACT_MONTH == "V" ~ 10L,
        CONTRACT_MONTH == "X" ~ 11L,
        CONTRACT_MONTH == "Z" ~ 12L,
        TRUE ~ NA_integer_
      ),
      ETHANOL_EXPIRY_CEILING_DATE =
        ceiling_date(ymd(sprintf("%d-%02d-01", CONTRACT_YEAR, ETHANOL_MONTH_NUM)), "month") - days(1),
      
      # Expired flag (as-of date compared to expiry date)
      EXPIRED_CONTRACT = as_of_date > ETHANOL_EXPIRY_CEILING_DATE
    ) |>
    select(-ETHANOL_MONTH_NUM) 
  
  
  corn_join <- input_corn_df |>
    rename(CONTRACT_YEAR = CALENDAR_YEAR) |>
    select(
      DATE_ID,
      CONTRACT_YEAR,
      MONTH_CODE,
      CORN_DESCRIPTION = DESCRIPTION,
      CORN_CLOSE = CLOSE
    ) 
  
  on_corn_months_crush_df <- ethanol_df |>
    left_join(
      corn_join,
      by = c(
        "DATE_ID" = "DATE_ID",
        "RELEVANT_CORN_CONTRACT_YEAR" = "CONTRACT_YEAR",
        "RELEVANT_CORN_CONTRACT_MONTH" = "MONTH_CODE"
      )
    ) |>
    rename(
      ETHANOL_DESCRIPTION = DESCRIPTION,
      ETHANOL_CLOSE = CLOSE
    ) |>
    select(
      DATE_ID,
      CONTRACT_YEAR,
      CONTRACT_MONTH,
      CONTRACT_MONTH_NAME,
      RELEVANT_CORN_CONTRACT_YEAR,
      RELEVANT_CORN_CONTRACT_MONTH,
      ETHANOL_DESCRIPTION,
      CORN_DESCRIPTION,
      ETHANOL_EXPIRY_CEILING_DATE,
      EXPIRED_CONTRACT,
      ETHANOL_CLOSE,
      CORN_CLOSE
    ) |> 
    na.omit() |> 
    select(DATE_ID,EXPIRED_CONTRACT,CORN_DESCRIPTION,ETHANOL_DESCRIPTION,CONTRACT_MONTH_NAME,CONTRACT_YEAR,RELEVANT_CORN_CONTRACT_MONTH,RELEVANT_CORN_CONTRACT_YEAR,ETHANOL_CLOSE,CORN_CLOSE) |> 
    mutate(TRADING_MONTH = floor_date(DATE_ID,'months')) |> 
    group_by(CORN_DESCRIPTION,ETHANOL_DESCRIPTION,TRADING_MONTH) |> 
    mutate(ROLLING_AVERAGE_CORN_CLOSE = cummean(CORN_CLOSE)) |> 
    ungroup() |> 
    group_by(CORN_DESCRIPTION,ETHANOL_DESCRIPTION) |> 
    filter(DATE_ID == max(DATE_ID)) |> 
    mutate(
      CRUSH_SPREAD = ETHANOL_CLOSE - (ROLLING_AVERAGE_CORN_CLOSE/100/yield),
      IMPLIED_CRUSH_SPREAD = ETHANOL_CLOSE - (CORN_CLOSE/100/yield),
      CRUSH_SPREAD_USED = if_else(EXPIRED_CONTRACT, CRUSH_SPREAD,IMPLIED_CRUSH_SPREAD)
    ) |> 
    ungroup() |> 
    select(DESCRIPTION = ETHANOL_DESCRIPTION,
           RELEVANT_CORN_PRICE_ON_MONTH = CORN_CLOSE,
           ETHANOL_PRICE_ON_MONTH = ETHANOL_CLOSE,
           CRUSH_SPREAD_ON_MONTH = CRUSH_SPREAD_USED)
}

on_month_prices <- generate_on_month_prices(as_of_date = '2026-02-06')


prices_full <- off_month_prices |> 
  full_join(on_month_prices)





































