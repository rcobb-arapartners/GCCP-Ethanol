# 02-DataBuild.R
# Simplified data fetching and processing

# ------------------------------------------------------------
# Single API fetch function (NO auto-execution)
# ------------------------------------------------------------
fetch_marketview_data <- function(base_symbols, 
                                  calendar_years = 2007:2026,
                                  month_codes,
                                  min_date = "2000-01-01",
                                  max_date = Sys.Date()) {
  
  # Build contract symbols
  contracts <- expand_grid(
    base_symbol = base_symbols,
    year = calendar_years,
    month = month_codes
  ) |>
    mutate(
      year_code = substr(as.character(year), 3, 4),
      symbol = paste0("/", base_symbol, month, year_code)
    )
  
  # Build URL (match original format exactly)
  symbols_string <- paste(contracts$symbol, collapse = ",")
  url <- paste0(
    "https://webservice.gvsi.com/api/v3/getdaily",
    "?symbols=", URLencode(symbols_string, reserved = TRUE),
    "&fields=symbol%2Cdescription%2Cupdatetype%2Clastupdatetime%2Cclose%2Chigh%2Clow%2Copen%2Copeninterest%2Ctradedatetimeutc%2Cvolume%2Cweight",
    "&output=csv&includeheaders=true",
    "&startdate=", URLencode(format(as.Date(min_date), "%m/%d/%Y"), reserved = TRUE),
    "&enddate=", URLencode(format(as.Date(max_date), "%m/%d/%Y"), reserved = TRUE),
    "&username=", Sys.getenv("ara_marketview_api_username"),
    "&password=", Sys.getenv("ara_marketview_api_password")
  )
  
  print(paste("Fetching", base_symbols, "with", nrow(contracts), "contracts..."))
  
  # Fetch and clean
  read_csv(url, show_col_types = FALSE) |>
    mutate(
      symbol = if_else(startsWith(symbol, "/"), symbol, paste0("/", symbol)),
      date = as.Date(mdy_hms(tradedatetimeutc))
    ) |>
    left_join(contracts, by = "symbol") |>
    mutate(
      month_num = MONTH_CODES[month],
      expiry_date = ceiling_date(ymd(sprintf("%d-%02d-01", year, month_num)), "month") - days(1)
    ) |>
    filter(date <= expiry_date) |>
    select(date, year, month, description, close)
}

# ------------------------------------------------------------
# Build crush spread input dataframe
# ------------------------------------------------------------
build_crush_df <- function(ethanol_df, corn_df, as_of_date = Sys.Date()) {
  
  as_of_date <- as.Date(as_of_date)
  
  ethanol_df |>
    rename(
      eth_desc = description,
      eth_close = close,
      eth_year = year,
      eth_month = month
    ) |>
    mutate(
      month_name = MONTH_NAMES[eth_month],
      corn_month = ETHANOL_TO_CORN_MONTH[eth_month],
      corn_year = if_else(eth_month == "Z", eth_year + 1L, eth_year),
      month_num = MONTH_CODES[eth_month],
      expiry_date = ceiling_date(ymd(sprintf("%d-%02d-01", eth_year, month_num)), "month") - days(1),
      expired = as_of_date > expiry_date
    ) |>
    left_join(
      corn_df |>
        rename(corn_desc = description, corn_close = close) |>
        select(date, year, month, corn_desc, corn_close),
      by = c("date", "corn_year" = "year", "corn_month" = "month")
    ) |>
    select(date, eth_year, eth_month, month_name, corn_year, corn_month,
           eth_desc, corn_desc, expiry_date, expired, eth_close, corn_close)
}

# ------------------------------------------------------------
# Calculate liquidations table - FIXED to use as_of_date
# ------------------------------------------------------------
calc_liquidations <- function(crush_df, yield = 2.9, as_of_date = Sys.Date()) {
  
  as_of_date <- as.Date(as_of_date)
  
  # CRITICAL FIX: Filter to as_of_date, not max(date)
  result <- crush_df |>
    na.omit() |>
    mutate(trading_month = floor_date(date, "months")) |>
    group_by(corn_desc, eth_desc, trading_month) |>
    mutate(rolling_avg_corn = cummean(corn_close)) |>
    ungroup() |>
    # Filter to the as_of_date for each contract
    group_by(corn_desc, eth_desc) |>
    filter(date <= as_of_date) |>
    filter(date == max(date)) |>
    mutate(
      crush_spread = eth_close - (rolling_avg_corn / 100 / yield),
      implied_spread = eth_close - (corn_close / 100 / yield),
      # Non-expired uses IMPLIED, Expired uses CRUSH
      spread_used = if_else(expired, crush_spread, implied_spread)
    ) |>
    ungroup()
  
  result |>
    select(eth_year, month_name, spread_used) |>
    pivot_wider(
      id_cols = eth_year,
      names_from = month_name,
      values_from = spread_used
    ) |>
    select(eth_year, January, February, March, April, May, June,
           July, August, September, October, November, December) |>
    rowwise() |>
    mutate(
      CalYear = mean(c(January, February, March, April, May, June,
                       July, August, September, October, November, December),
                     na.rm = TRUE)
    ) |>
    ungroup()
}