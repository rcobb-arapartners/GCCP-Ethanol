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
# Rob to-do: build a function that calculates the continious futures contract (example: GCU<0>)
########################################################


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
# Generate the input dataframe to calculate the crush spread for on months
########################################################
generate_crush_on_corn_months_input_df <- function(input_ethanol_df = ethanol_marketview_daily_forward_curves,
                                                   input_corn_df    = corn_marketview_daily_forward_curves,
                                                   as_of_date       = '2026-02-06') {
  
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
      EXPIRED_CONTRACT,
      ETHANOL_CLOSE,
      CORN_CLOSE
    )
}
crush_on_corn_months_input_df <- generate_crush_on_corn_months_input_df(as_of_date = '2026-02-06')
########################################################
# Generate Liquidations ON Corn Months
########################################################
generate_liquidations_on_corn_months <- function(yield = 2.9,
                                                 on_corn_months_crush_df = crush_on_corn_months_input_df) {
  liquidation_on_table <- on_corn_months_crush_df |> 
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
    select(CONTRACT_YEAR,CONTRACT_MONTH_NAME,CRUSH_SPREAD_USED) |> 
    pivot_wider(id_cols = CONTRACT_YEAR,
                names_from = CONTRACT_MONTH_NAME,
                values_from = CRUSH_SPREAD_USED) |> 
    select(CONTRACT_YEAR,January, February, March, April, May, June, July, August, September, October, November, December) |> 
    rowwise() |> 
    mutate(CalYear = mean(c(January, February, March, April, May, June, July, August, September, October, November, December),na.rm = T))
  
  return(liquidation_on_table)
}
liquidations_on_corn_months <- generate_liquidations_on_corn_months()
liquidations_on_corn_months

########################################################
# Generate Liquidations OFF Corn Months
# scratch scripting
########################################################
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


ethanol_roll_months <- c("H","K","N","U","Z")  # Mar, May, Jul, Sep, Dec

scratch_ethanol_off <- ethanol_marketview_daily_forward_curves |> 
  filter(DATE_ID <= as.Date("2026-02-06")) |>  #!!! parameterize this. 
  mutate(
    year_full = 2000 + as.numeric(YEAR_CODE),
    month_num = ethanol_month_lookup[MONTH_CODE],
    CONTRACT_DATE_ID = as.Date(paste(year_full, month_num, "01", sep = "-")),
    prior_month_start = floor_date(CONTRACT_DATE_ID %m-% months(1), unit = "month"),
    prior_month_end   = CONTRACT_DATE_ID - days(1)
  ) |>
  group_by(DESCRIPTION) |>
  mutate(
    last_prior_date = suppressWarnings(
      max(DATE_ID[DATE_ID >= prior_month_start & DATE_ID <= prior_month_end], na.rm = TRUE)
    ),
    has_prior_month = is.finite(last_prior_date),
    keep_row = if_else(
      MONTH_CODE %in% ethanol_roll_months & has_prior_month,
      DATE_ID == last_prior_date,
      TRUE
    )
  ) |>
  ungroup() |>
  filter(keep_row) |>
  select(DESCRIPTION, YEAR_CODE, MONTH_CODE, DATE_ID, CONTRACT_DATE_ID, CLOSE) |> 
  group_by(DESCRIPTION) |> 
  filter(DATE_ID == max(DATE_ID)) |> 
  ungroup() |>
  arrange(desc(CONTRACT_DATE_ID))


for(d in unique(corn_marketview_daily_forward_curves$DESCRIPTION)) {
     print(d)
  
    corn_off_sub_df <- corn_marketview_daily_forward_curves |>
      filter(DATE_ID <= as.Date("2026-02-06")) |>
      filter(DESCRIPTION == d) |>
      mutate(
        DATE_ID = as.Date(DATE_ID),
        year_full = 2000 + as.numeric(YEAR_CODE),
        month_num = ethanol_month_lookup[MONTH_CODE],
        CONTRACT_DATE_ID = as.Date(paste(year_full, month_num, "01", sep = "-")),
        TRADING_MONTH = floor_date(DATE_ID, "months"),
        TRADING_MONTH_NAME = months(TRADING_MONTH)
      )
    
    
    if(months(unique(corn_off_sub_df$CONTRACT_DATE_ID)) == "March") {
      corn_off_sub_df_avg_months <- corn_off_sub_df |> 
        filter(TRADING_MONTH_NAME %in% c("January","February")) |> 
        group_by(TRADING_MONTH_NAME) |> 
        filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
        ungroup() |> 
        group_by(CONTRACT_DATE_ID,TRADING_MONTH) |> 
        summarise(RELEVANT_CORN_PRICE = mean(CLOSE,na.rm = T))
    }
    
    
    
    
    
    
    
}








# for(d in unique(corn_marketview_daily_forward_curves$DESCRIPTION)) {
#   print(d)
#   corn_off_sub_df <- corn_marketview_daily_forward_curves |>
#     filter(DATE_ID <= as.Date("2026-02-06")) |>
#     filter(DESCRIPTION == d) |>
#     mutate(
#       DATE_ID = as.Date(DATE_ID),
#       year_full = 2000 + as.numeric(YEAR_CODE),
#       month_num = ethanol_month_lookup[MONTH_CODE],
#       CONTRACT_DATE_ID = as.Date(paste(year_full, month_num, "01", sep = "-")),
#       TRADING_MONTH = floor_date(DATE_ID, "months")
#     ) |>
#     group_by(TRADING_MONTH) |>
#     filter(
#       # March contract: allow only Jan/Feb trading days
#       (months(CONTRACT_DATE_ID) == "March"     & months(DATE_ID) %in% c("January", "February")) |
#         
#         # May contract: allow only April trading days
#         (months(CONTRACT_DATE_ID) == "May"       & months(DATE_ID) %in% c("April")) |
#         
#         # July contract: allow only June trading days
#         (months(CONTRACT_DATE_ID) == "July"      & months(DATE_ID) %in% c("June")) |
#         
#         # September contract: allow only August trading days
#         (months(CONTRACT_DATE_ID) == "September" & months(DATE_ID) %in% c("August")) |
#         
#         # December contract: allow only Oct/Nov trading days
#         (months(CONTRACT_DATE_ID) == "December"  & months(DATE_ID) %in% c("October", "November")) |
#         
#         # All other contract months: no extra filtering
#         !(months(CONTRACT_DATE_ID) %in% c("March","May","July","September","December"))
#     ) |> 
#     mutate(CORN_TRADING_MONTH_NAME = months(TRADING_MONTH)) |> 
#     ungroup() |> 
#     group_by(CONTRACT_DATE_ID,CORN_TRADING_MONTH_NAME) |> 
#     filter(TRADING_MONTH == max(TRADING_MONTH)) |> 
#     ungroup() |> 
#     group_by(CONTRACT_DATE_ID,CORN_TRADING_MONTH_NAME) |> 
#     summarise(RELEVANT_CORN_CLOSE_PRICE = mean(CLOSE)) |> 
#     bind_rows(corn_marketview_daily_forward_curves |> 
#                 filter(DATE_ID <= '2026-02-06') |> 
#                 filter(DESCRIPTION == d) |> 
#                 mutate(TRADING_MONTH = floor_date(DATE_ID,'months')) |> 
#                 filter(TRADING_MONTH != max(TRADING_MONTH)) |> 
#                 filter(DATE_ID == max(DATE_ID)) |> 
#                 mutate(
#                   year_full = 2000 + as.numeric(YEAR_CODE),
#                   month_num = ethanol_month_lookup[MONTH_CODE],
#                   CONTRACT_DATE_ID = as.Date(paste(year_full, month_num, "01", sep = "-")),
#                   CORN_TRADING_MONTH_NAME = months(TRADING_MONTH)
#                 ) |> 
#                 select(CONTRACT_DATE_ID,CORN_TRADING_MONTH_NAME,CLOSE) |> 
#                 rename(RELEVANT_CORN_CLOSE_PRICE = CLOSE))
#   
# }








########################################################
# scratch visualizations
########################################################



crush_on_corn_months_input_df |> 
  mutate(TRADING_MONTH = floor_date(DATE_ID,'months')) |> 
  group_by(CORN_DESCRIPTION,ETHANOL_DESCRIPTION,TRADING_MONTH) |> 
  mutate(ROLLING_AVERAGE_CORN_CLOSE = cummean(CORN_CLOSE)) |> 
  ungroup() |> 
  mutate(
    CRUSH_SPREAD = ETHANOL_CLOSE - (ROLLING_AVERAGE_CORN_CLOSE/100/2.9),
    IMPLIED_CRUSH_SPREAD = ETHANOL_CLOSE - (CORN_CLOSE/100/2.9),
    CRUSH_SPREAD_USED = if_else(EXPIRED_CONTRACT, CRUSH_SPREAD,IMPLIED_CRUSH_SPREAD)
  ) |> 
  filter(ETHANOL_DESCRIPTION == 'Sep 25 NYMEX Chicago Ethanol (Platts) Futures Electronic') |> 
  ggplot(aes(x = DATE_ID)) + 
  geom_line(aes(y = CRUSH_SPREAD_USED)) 





