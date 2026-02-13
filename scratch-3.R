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
    filter(DATE_ID < CONTRACT_DATE_ID) |> 
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

#ethanol OFF
generate_off_month_ethanol_prices <- function(marketview_ethanol_df = ethanol_marketview_daily_forward_curves,
                                              as_of_date = '2026-02-06') {
  ethanol_off <- marketview_ethanol_df |> 
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
  return(ethanol_off)
}
off_month_ethanol_prices <- generate_off_month_ethanol_prices()
#corn off
generate_off_month_corn_prices <- function(marketview_corn_df = corn_marketview_daily_forward_curves,
                                           ethanol_off_df = off_month_ethanol_prices,
                                           as_of_date = '2026-02-06') {
  #first, do expired contracts
  ethanol_off_df_expired <- ethanol_off_df |> 
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

  
  ethanol_off_df_non_expired <- ethanol_off_df |> 
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
  return(full_relevant_corn_off_prices)
  
}
off_month_corn_prices <- generate_off_month_corn_prices()

off_month_prices <- off_month_ethanol_prices |> 
  inner_join(off_month_corn_prices,by = c('DESCRIPTION' = 'ETHANOL_DESCRIPTION'))


write_csv(off_month_prices,'for_kevin_off_month.csv')




































