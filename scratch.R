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

build_marketview_daily_forward_data <- function(
    base_symbols   = c("GCU","ZC"),
    calendar_years = c(2007:2026),
    month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
    min_date       = "2000-01-01",
    max_date       = Sys.Date()) {
  
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
    select(SYMBOL,DESCRIPTION,BASE_SYMBOL,CALENDAR_YEAR,YEAR_CODE,MONTH_CODE,DATE_ID,everything())
  
  return(full_data)
  

}
########################################################
# Call Ethanol for all months
########################################################
ethanol_marketview_daily_forward_curves <- build_marketview_daily_forward_data(base_symbols   = c("GCU"),
                                                                               calendar_years = c(2007:2026),
                                                                               month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                                                               min_date       = "2000-01-01",
                                                                               max_date       = '2026-01-16')
########################################################
# Call Corn for only select months
########################################################
corn_marketview_daily_forward_curves <- build_marketview_daily_forward_data(base_symbols   = c("ZC"),
                                                                            calendar_years = c(2007:2026),
                                                                            month_codes    = c("H","K","N","U","Z"),
                                                                            min_date       = "2000-01-01",
                                                                            max_date       = '2026-01-16')
########################################################
# Generate the input dataframe to calculate the crush spread
########################################################
generate_crush_input_df <- function(input_ethanol_df = ethanol_marketview_daily_forward_curves,
                                    input_corn_df    = corn_marketview_daily_forward_curves,
                                    as_of_date       = Sys.Date()) {
  
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
      RELEVANT_CORN_CONTRACT_YEAR = if_else(CONTRACT_MONTH == "Z", CONTRACT_YEAR + 1L, CONTRACT_YEAR),
      
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
      
      # Correct expiration flag: compare expiry to an as-of date, not the row DATE_ID
      ETHANOL_NOT_EXPIRED = as_of_date <= ETHANOL_EXPIRY_CEILING_DATE
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
  
  ethanol_df |>
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
      ETHANOL_NOT_EXPIRED,
      ETHANOL_CLOSE,
      CORN_CLOSE
    )
}


crush_input <- generate_crush_input_df() |> 
  na.omit() |> 
  select(DATE_ID,ETHANOL_NOT_EXPIRED,CORN_DESCRIPTION,ETHANOL_DESCRIPTION,CONTRACT_MONTH_NAME,CONTRACT_YEAR,RELEVANT_CORN_CONTRACT_MONTH,RELEVANT_CORN_CONTRACT_YEAR,ETHANOL_CLOSE,CORN_CLOSE) |> 
  mutate(TRADING_MONTH = floor_date(DATE_ID,'months')) |> 
  group_by(CORN_DESCRIPTION,ETHANOL_DESCRIPTION,TRADING_MONTH) |> 
  mutate(ROLLING_AVERAGE_CORN_CLOSE = cummean(CORN_CLOSE)) |> 
  ungroup() |> 
  group_by(CORN_DESCRIPTION,ETHANOL_DESCRIPTION) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  mutate(
    CRUSH_SPREAD = ETHANOL_CLOSE - (ROLLING_AVERAGE_CORN_CLOSE/100/2.9),
    IMPLIED_CRUSH_SPREAD = ETHANOL_CLOSE - (CORN_CLOSE/100/2.9),
    CRUSH_SPREAD_USED = if_else(ETHANOL_NOT_EXPIRED, IMPLIED_CRUSH_SPREAD, CRUSH_SPREAD)
  ) |> 
  ungroup() |> 
  select(CONTRACT_YEAR,CONTRACT_MONTH_NAME,CRUSH_SPREAD) |> 
  pivot_wider(id_cols = CONTRACT_YEAR,
              names_from = CONTRACT_MONTH_NAME,
              values_from = CRUSH_SPREAD) |> 
  select(CONTRACT_YEAR,January, February, March, April, May, June, July, August, September, October, November, December)
  





