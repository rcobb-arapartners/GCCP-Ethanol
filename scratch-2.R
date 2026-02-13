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
    max_date       = Sys.Date()) {
  
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
    filter(DATE_ID < CONTRACT_DATE_ID)
  
  return(full_data)
  
  
}
########################################################
# Call Ethanol for all months
########################################################
ethanol_marketview_daily_forward_curves <- build_marketview_daily_forward_data_on_corn_months(base_symbols   = c("GCU"),
                                                                                              calendar_years = c(2007:2026),
                                                                                              month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                                                                              min_date       = "2000-01-01",
                                                                                              max_date       = Sys.Date())

########################################################
# Call Corn for only select months
########################################################
corn_marketview_daily_forward_curves <- build_marketview_daily_forward_data_on_corn_months(base_symbols   = c("ZC"),
                                                                                           calendar_years = c(2007:2026),
                                                                                           month_codes    = c("H","K","N","U","Z"),
                                                                                           min_date       = "2000-01-01",
                                                                                           max_date       = Sys.Date())




########################################################
# Generate Liquidations OFF Corn Months
# scratch scripting
########################################################
as_of_date = as.Date('2026-02-06')
#ethanol OFF
scratch_ethanol_off <- ethanol_marketview_daily_forward_curves |> 
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
  select(DESCRIPTION, YEAR_CODE, MONTH_CODE, DATE_ID, CONTRACT_DATE_ID, CLOSE) |> 
  group_by(DESCRIPTION) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  ungroup() |>
  arrange(desc(CONTRACT_DATE_ID))

#corn OFF
scratch_corn_off <- corn_marketview_daily_forward_curves |> 
  filter(DATE_ID <= as_of_date)
#march 
jan_mar <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'March',
         months(TRADING_MONTH) == "January") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |> 
  mutate(X = 1)

feb_mar <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'March',
         months(TRADING_MONTH) == "February") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |> mutate(X = 2)

mar_mar <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'March',
         months(TRADING_MONTH) == "February") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE = CLOSE) |> 
  mutate(TRADING_MONTH = CONTRACT_DATE_ID) |> 
  mutate(X = 3)

#may
apr_may <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'May',
         months(TRADING_MONTH) == "April") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |> 
  mutate(X = 4)

may_may <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'May',
         months(TRADING_MONTH) == "April") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE = CLOSE) |>
  mutate(TRADING_MONTH = CONTRACT_DATE_ID) |> 
  mutate(X = 5)

#july
jun_jul <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'July',
         months(TRADING_MONTH) == "June") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |> 
  mutate(X = 6)


jul_jul <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'July',
         months(TRADING_MONTH) == "June") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE = CLOSE) |>
  mutate(TRADING_MONTH = CONTRACT_DATE_ID) |> 
  mutate(X = 7)

#september
aug_sep <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'September',
         months(TRADING_MONTH) == "August") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |> 
  mutate(X = 8)

sep_sep <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'September',
         months(TRADING_MONTH) == "August") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE = CLOSE) |>
  mutate(TRADING_MONTH = CONTRACT_DATE_ID) |> 
  mutate(X = 9)

#december

oct_dec <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'December',
         months(TRADING_MONTH) == "October") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |>
  mutate(X = 10)

nov_dec <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'December',
         months(TRADING_MONTH) == "November") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
  group_by(TRADING_MONTH,CONTRACT_DATE_ID) |> 
  summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T)) |>
  mutate(X = 11)

dec_dec <- scratch_corn_off |> 
  filter(months(CONTRACT_DATE_ID) == 'December',
         months(TRADING_MONTH) == "November") |> 
  group_by(CONTRACT_DATE_ID) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE = CLOSE) |>
  mutate(TRADING_MONTH = CONTRACT_DATE_ID) |> 
  mutate(X = 12)


corn_off_all <- bind_rows(
  jan_mar,
  feb_mar,
  mar_mar,
  apr_may,
  may_may,
  jun_jul,
  jul_jul,
  aug_sep,
  sep_sep,
  oct_dec,
  nov_dec,
  dec_dec) |> 
  rename(RC_RELEVANT_CORN_PRICE = RELEVANT_CORN_PRICE) |> 
  arrange(desc(TRADING_MONTH))






# corn_off_all_CHAT <- scratch_corn_off |>
#   filter(CONTRACT_DATE_ID < as_of_date) |>
#   mutate(
#     contract_month = months(CONTRACT_DATE_ID),
#     trading_month  = months(TRADING_MONTH)
#   ) |>
#   mutate(
#     mode = case_when(
#       # last rules
#       contract_month == "March"     & trading_month == "February"  ~ "last",
#       contract_month == "May"       & trading_month == "April"     ~ "last",
#       contract_month == "July"      & trading_month == "June"      ~ "last",
#       contract_month == "September" & trading_month == "August"    ~ "last",
#       contract_month == "December"  & trading_month == "November"  ~ "last",
#       
#       # avg rules
#       contract_month == "March"     & trading_month %in% c("January","February") ~ "avg",
#       contract_month == "May"       & trading_month %in% c("April")             ~ "avg",
#       contract_month == "July"      & trading_month %in% c("June")              ~ "avg",
#       contract_month == "September" & trading_month %in% c("August")            ~ "avg",
#       contract_month == "December"  & trading_month %in% c("October","November")~ "avg",
#       
#       TRUE ~ NA_character_
#     )
#   ) |>
#   filter(!is.na(mode)) |>
#   
#   # replicate your "TRADING_MONTH == max(TRADING_MONTH)" trimming for avg mode
#   group_by(CONTRACT_DATE_ID, mode) |>
#   filter(
#     if_else(mode == "avg", TRADING_MONTH == max(TRADING_MONTH), TRUE)
#   ) |>
#   ungroup() |>
#   
#   group_by(TRADING_MONTH, CONTRACT_DATE_ID, mode) |>
#   summarise(
#     RELEVANT_CORN_PRICE = if_else(
#       first(mode) == "avg",
#       mean(CLOSE, na.rm = TRUE),
#       {
#         last_day <- max(DATE_ID, na.rm = TRUE)
#         mean(CLOSE[DATE_ID == last_day], na.rm = TRUE)
#       }
#     ),
#     .groups = "drop"
#   ) |>
#   select(TRADING_MONTH, CONTRACT_DATE_ID, RELEVANT_CORN_PRICE) |>
#   arrange(CONTRACT_DATE_ID, TRADING_MONTH)









