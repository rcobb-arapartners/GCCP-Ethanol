# 03-VisualBuild.R

# ------------------------------------------------------------
# Helper: Excel PERCENTRANK.INC equivalent
# Matches Excel's exact formula: interpolates when x falls between
# two adjacent sorted values.
# Arguments:
#   historical  - numeric vector of historical values (NOT including x)
#   x           - the value to rank
# Returns a value in [0, 1]
# ------------------------------------------------------------
excel_percentrank <- function(historical, x) {
  vals <- sort(na.omit(historical))
  n    <- length(vals)
  if (n == 0) return(NA_real_)
  
  # Excel PERCENTRANK(array, x):
  #   - array is the historical values only (x is NOT added to the pool)
  #   - if x < min(array): return 0
  #   - if x > max(array): return 1
  #   - if x exactly matches a value: (rank - 1) / (n - 1), rank = 1-based position
  #   - if x falls between vals[i] and vals[i+1]: linear interpolation
  #     result = (i-1)/(n-1) + (x - vals[i])/(vals[i+1] - vals[i]) * (1/(n-1))
  
  if (x <= vals[1])  return(0)
  if (x >= vals[n])  return(1)
  
  # Find position via interpolation (handles exact matches and between-values)
  i <- findInterval(x, vals)   # largest index where vals[i] <= x
  
  if (isTRUE(all.equal(vals[i], x))) {
    # Exact match
    (i - 1) / (n - 1)
  } else {
    # Interpolate between vals[i] and vals[i+1]
    (i - 1) / (n - 1) + (x - vals[i]) / (vals[i + 1] - vals[i]) * (1 / (n - 1))
  }
}

# ------------------------------------------------------------
# Append stats rows to the wide year x month data frame.
# Now includes:
#   [year rows]
#   __gap1__   <- thin separator
#   Mean
#   Fwd%       <- Forward percentile row
#   __gap2__   <- thin separator
#   P-10, P-25, P-50, P-75, P-90
#   __gap3__   <- thin separator
#   Week Ago Value, Week Ago Percentile, Δ Week, Month Ago Value, Month Ago Percentile, Δ Month
#
# Stats now exclude forward months (only include months <= as_of_date)
# ------------------------------------------------------------
append_stats_rows <- function(wide_df, as_of_date, max_year) {
  
  value_cols <- names(wide_df)[names(wide_df) != "YEAR"]
  
  # Determine which year/month combinations are forward
  month_to_num <- c(
    January = 1, February = 2, March = 3, April = 4,
    May = 5, June = 6, July = 7, August = 8,
    September = 9, October = 10, November = 11, December = 12
  )
  
  as_of_date <- as.Date(as_of_date)
  as_of_year <- as.integer(format(as_of_date, "%Y"))
  as_of_month <- as.integer(format(as_of_date, "%m"))
  
  make_blank <- function(label) {
    row <- tibble(YEAR = label)
    for (col in value_cols) row[[col]] <- NA_real_
    row
  }
  
  make_stat_row <- function(label, fn) {
    row <- tibble(YEAR = label)
    for (col in value_cols) {
      if (col == "CalYear") {
        # CalYear: average all non-forward months across all years
        all_values <- c()
        for (year_val in wide_df$YEAR) {
          year_int <- as.integer(year_val)
          for (month_col in names(month_to_num)) {
            if (month_col %in% names(wide_df)) {
              month_num <- month_to_num[month_col]
              # Include if year < as_of_year, or year == as_of_year and month <= as_of_month
              if (year_int < as_of_year || (year_int == as_of_year && month_num <= as_of_month)) {
                val <- wide_df %>% filter(YEAR == year_val) %>% pull(!!month_col)
                if (length(val) > 0 && !is.na(val)) {
                  all_values <- c(all_values, val)
                }
              }
            }
          }
        }
        row[[col]] <- fn(all_values)
      } else if (col %in% names(month_to_num)) {
        # Regular month column: only include years where this month is not forward
        month_num <- month_to_num[col]
        values <- c()
        for (year_val in wide_df$YEAR) {
          year_int <- as.integer(year_val)
          # Include if year < as_of_year, or year == as_of_year and month <= as_of_month
          if (year_int < as_of_year || (year_int == as_of_year && month_num <= as_of_month)) {
            val <- wide_df %>% filter(YEAR == year_val) %>% pull(!!col)
            if (length(val) > 0 && !is.na(val)) {
              values <- c(values, val)
            }
          }
        }
        row[[col]] <- fn(values)
      } else {
        row[[col]] <- NA_real_
      }
    }
    row
  }
  
  # Make forward percentile row using PERCENTRANK method
  make_forward_percentile_row <- function() {
    row <- tibble(YEAR = "Fwd%")
    
    # Get the forward year row
    forward_row <- wide_df %>% filter(YEAR == as.character(max_year))
    
    for (col in value_cols) {
      if (col %in% names(month_to_num)) {
        month_num <- month_to_num[col]
        
        # Only calculate for forward months (month > as_of_month for max_year)
        if (as.integer(max_year) == as_of_year && month_num > as_of_month) {
          # Get forward value
          forward_val <- if (nrow(forward_row) > 0 && col %in% names(forward_row)) {
            forward_row[[col]]
          } else {
            NA_real_
          }
          
          # Get all historical values for this month (excluding forward year)
          historical_values <- wide_df %>%
            mutate(YEAR_INT = as.integer(YEAR)) %>%
            filter(YEAR_INT < max_year) %>%
            pull(!!col) %>%
            na.omit()
          
          # Calculate percentile using Excel PERCENTRANK.INC method
          # Excel formula: count values < x, then interpolate for ties
          # rank = (number of values strictly less than x) + fraction for ties
          if (!is.na(forward_val) && length(historical_values) > 0) {
            row[[col]] <- excel_percentrank(historical_values, forward_val)
          } else {
            row[[col]] <- NA_real_
          }
        } else if (as.integer(max_year) > as_of_year) {
          # All months in future year are forward
          forward_val <- if (nrow(forward_row) > 0 && col %in% names(forward_row)) {
            forward_row[[col]]
          } else {
            NA_real_
          }
          
          # Get all historical values for this month
          historical_values <- wide_df %>%
            mutate(YEAR_INT = as.integer(YEAR)) %>%
            filter(YEAR_INT < max_year) %>%
            pull(!!col) %>%
            na.omit()
          
          # Calculate percentile using Excel PERCENTRANK.INC method
          if (!is.na(forward_val) && length(historical_values) > 0) {
            row[[col]] <- excel_percentrank(historical_values, forward_val)
          } else {
            row[[col]] <- NA_real_
          }
        } else {
          row[[col]] <- NA_real_
        }
      } else if (col == "CalYear") {
        # CalYear percentile for forward year
        if (as.integer(max_year) >= as_of_year) {
          forward_val <- if (nrow(forward_row) > 0) forward_row[[col]] else NA_real_
          
          historical_calyear_values <- wide_df %>%
            mutate(YEAR_INT = as.integer(YEAR)) %>%
            filter(YEAR_INT < max_year) %>%
            pull(CalYear) %>%
            na.omit()
          
          # Calculate percentile using Excel PERCENTRANK.INC method
          if (!is.na(forward_val) && length(historical_calyear_values) > 0) {
            row[[col]] <- excel_percentrank(historical_calyear_values, forward_val)
          } else {
            row[[col]] <- NA_real_
          }
        } else {
          row[[col]] <- NA_real_
        }
      } else {
        row[[col]] <- NA_real_
      }
    }
    row
  }
  
  bind_rows(
    wide_df,
    make_blank("__gap1__"),
    make_stat_row("Mean", function(x) mean(x, na.rm = TRUE)),
    make_forward_percentile_row(),
    make_blank("__gap2__"),
    make_stat_row("P-10", function(x) quantile(x, 0.10, na.rm = TRUE)),
    make_stat_row("P-25", function(x) quantile(x, 0.25, na.rm = TRUE)),
    make_stat_row("P-50", function(x) quantile(x, 0.50, na.rm = TRUE)),
    make_stat_row("P-75", function(x) quantile(x, 0.75, na.rm = TRUE)),
    make_stat_row("P-90", function(x) quantile(x, 0.90, na.rm = TRUE))
  )
}

# ------------------------------------------------------------
# Build comparison rows (Week Ago, Month Ago, Deltas, Percentiles)
# Only for the max_year selected
# ------------------------------------------------------------
build_comparison_rows <- function(wide_df, as_of_date, max_year, 
                                  ethanol_df, corn_df, yield, spread_type) {
  
  as_of_date <- as.Date(as_of_date)
  
  # Get week ago and month ago dates
  week_ago_date <- as_of_date - 7
  # Month ago: same day, one month back
  month_ago_date <- as_of_date %m-% months(1)
  
  # Determine forward months
  month_to_num <- c(
    January = 1, February = 2, March = 3, April = 4,
    May = 5, June = 6, July = 7, August = 8,
    September = 9, October = 10, November = 11, December = 12
  )
  as_of_month <- as.integer(format(as_of_date, "%m"))
  as_of_year <- as.integer(format(as_of_date, "%Y"))
  
  # Try to build comparison data, fall back to empty if it fails
  tryCatch({
    # Filter ethanol and corn to only include data up to week ago date
    ethanol_week <- ethanol_df %>%
      filter(DATE_ID <= week_ago_date) %>%
      mutate(EXPIRED = if_else(week_ago_date > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    
    corn_week <- corn_df %>%
      filter(DATE_ID <= week_ago_date) %>%
      mutate(EXPIRED = if_else(week_ago_date > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    
    # Filter ethanol and corn to only include data up to month ago date
    ethanol_month <- ethanol_df %>%
      filter(DATE_ID <= month_ago_date) %>%
      mutate(EXPIRED = if_else(month_ago_date > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    
    corn_month <- corn_df %>%
      filter(DATE_ID <= month_ago_date) %>%
      mutate(EXPIRED = if_else(month_ago_date > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    
    # Rebuild prices_full with filtered data
    prices_week <- build_prices_full(ethanol_week, corn_week, week_ago_date, yield)
    prices_month <- build_prices_full(ethanol_month, corn_month, month_ago_date, yield)
    
    # Shape to wide
    spread_col <- if (spread_type == "ON") "CRUSH_SPREAD_ON_MONTH" else "CRUSH_SPREAD_OFF_MONTH"
    wide_week <- shape_for_table(prices_week, spread_col)
    wide_month <- shape_for_table(prices_month, spread_col)
    
    # Get current values for max_year
    current_row <- wide_df %>% filter(YEAR == as.character(max_year))
    week_row <- wide_week %>% filter(YEAR == as.character(max_year))
    month_row <- wide_month %>% filter(YEAR == as.character(max_year))
    
    # Build comparison rows
    value_cols <- names(wide_df)[names(wide_df) != "YEAR"]
    
    week_ago_row <- tibble(YEAR = "Week Ago Value")
    week_ago_pct_row <- tibble(YEAR = "Week Ago Percentile")
    delta_week_row <- tibble(YEAR = "Δ Week")
    month_ago_row <- tibble(YEAR = "Month Ago Value")
    month_ago_pct_row <- tibble(YEAR = "Month Ago Percentile")
    delta_month_row <- tibble(YEAR = "Δ Month")
    
    for (col in value_cols) {
      if (col %in% names(month_to_num)) {
        month_num <- month_to_num[col]
        # Only show comparisons for forward months - check length first
        if (length(month_num) > 0 && !is.null(month_num) && !is.na(month_num) && month_num > as_of_month) {
          curr_val <- if (nrow(current_row) > 0 && col %in% names(current_row)) { v <- current_row[[col]]; if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
          week_val <- if (nrow(week_row)    > 0 && col %in% names(week_row))    { v <- week_row[[col]];    if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
          month_val <- if (nrow(month_row)  > 0 && col %in% names(month_row))   { v <- month_row[[col]];   if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
          
          # Calculate percentiles for week ago and month ago values using Excel PERCENTRANK method
          # Get all historical values for this month (excluding forward years)
          historical_values <- wide_df %>%
            filter(!(YEAR %in% c("Mean", "Fwd%", "P-10", "P-25", "P-50", "P-75", "P-90"))) %>%
            mutate(YEAR = as.integer(YEAR)) %>%
            filter(YEAR < as_of_year | (YEAR == as_of_year & month_num <= as_of_month)) %>%
            pull(!!col) %>%
            na.omit()
          
          week_pct <- if (!is.na(week_val) && length(historical_values) > 0) {
            excel_percentrank(historical_values, week_val)
          } else {
            NA_real_
          }
          
          month_pct <- if (!is.na(month_val) && length(historical_values) > 0) {
            excel_percentrank(historical_values, month_val)
          } else {
            NA_real_
          }
          
          week_ago_row[[col]] <- week_val
          week_ago_pct_row[[col]] <- week_pct
          delta_week_row[[col]] <- if (!is.na(curr_val) && !is.na(week_val)) curr_val - week_val else NA_real_
          month_ago_row[[col]] <- month_val
          month_ago_pct_row[[col]] <- month_pct
          delta_month_row[[col]] <- if (!is.na(curr_val) && !is.na(month_val)) curr_val - month_val else NA_real_
        } else {
          week_ago_row[[col]] <- NA_real_
          week_ago_pct_row[[col]] <- NA_real_
          delta_week_row[[col]] <- NA_real_
          month_ago_row[[col]] <- NA_real_
          month_ago_pct_row[[col]] <- NA_real_
          delta_month_row[[col]] <- NA_real_
        }
      } else if (col == "CalYear") {
        # CalYear: aggregate forward months only
        curr_vals <- c()
        week_vals <- c()
        month_vals <- c()
        
        for (month_col in names(month_to_num)) {
          if (month_col %in% names(current_row)) {
            month_num <- month_to_num[month_col]
            if (length(month_num) > 0 && !is.null(month_num) && !is.na(month_num) && month_num > as_of_month) {
              cv <- if (nrow(current_row) > 0 && month_col %in% names(current_row)) { v <- current_row[[month_col]]; if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
              wv <- if (nrow(week_row)    > 0 && month_col %in% names(week_row))    { v <- week_row[[month_col]];    if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
              mv <- if (nrow(month_row)   > 0 && month_col %in% names(month_row))   { v <- month_row[[month_col]];   if (length(v) == 1 && !is.na(v)) v else NA_real_ } else NA_real_
              
              if (!is.na(cv)) curr_vals <- c(curr_vals, cv)
              if (!is.na(wv)) week_vals <- c(week_vals, wv)
              if (!is.na(mv)) month_vals <- c(month_vals, mv)
            }
          }
        }
        
        curr_avg <- if (length(curr_vals) > 0) mean(curr_vals) else NA_real_
        week_avg <- if (length(week_vals) > 0) mean(week_vals) else NA_real_
        month_avg <- if (length(month_vals) > 0) mean(month_vals) else NA_real_
        
        # Calculate percentile for CalYear (all historical CalYear values) using Excel PERCENTRANK method
        historical_calyear_values <- wide_df %>%
          filter(!(YEAR %in% c("Mean", "Fwd%", "P-10", "P-25", "P-50", "P-75", "P-90"))) %>%
          mutate(YEAR = as.integer(YEAR)) %>%
          filter(YEAR < as_of_year) %>%
          pull(CalYear) %>%
          na.omit()
        
        week_calyear_pct <- if (!is.na(week_avg) && length(historical_calyear_values) > 0) {
          excel_percentrank(historical_calyear_values, week_avg)
        } else {
          NA_real_
        }
        
        month_calyear_pct <- if (!is.na(month_avg) && length(historical_calyear_values) > 0) {
          excel_percentrank(historical_calyear_values, month_avg)
        } else {
          NA_real_
        }
        
        week_ago_row[[col]] <- week_avg
        week_ago_pct_row[[col]] <- week_calyear_pct
        delta_week_row[[col]] <- if (!is.na(curr_avg) && !is.na(week_avg)) curr_avg - week_avg else NA_real_
        month_ago_row[[col]] <- month_avg
        month_ago_pct_row[[col]] <- month_calyear_pct
        delta_month_row[[col]] <- if (!is.na(curr_avg) && !is.na(month_avg)) curr_avg - month_avg else NA_real_
      } else {
        week_ago_row[[col]] <- NA_real_
        week_ago_pct_row[[col]] <- NA_real_
        delta_week_row[[col]] <- NA_real_
        month_ago_row[[col]] <- NA_real_
        month_ago_pct_row[[col]] <- NA_real_
        delta_month_row[[col]] <- NA_real_
      }
    }
    
    bind_rows(
      week_ago_row,
      week_ago_pct_row,
      delta_week_row,
      month_ago_row,
      month_ago_pct_row,
      delta_month_row
    )
    
  }, error = function(e) {
    message("Warning: Could not build comparison data: ", e$message)
    # Return empty comparison rows
    value_cols <- names(wide_df)[names(wide_df) != "YEAR"]
    
    week_ago_row <- tibble(YEAR = "Week Ago Value")
    week_ago_pct_row <- tibble(YEAR = "Week Ago Percentile")
    delta_week_row <- tibble(YEAR = "Δ Week")
    month_ago_row <- tibble(YEAR = "Month Ago Value")
    month_ago_pct_row <- tibble(YEAR = "Month Ago Percentile")
    delta_month_row <- tibble(YEAR = "Δ Month")
    
    for (col in value_cols) {
      week_ago_row[[col]] <- NA_real_
      week_ago_pct_row[[col]] <- NA_real_
      delta_week_row[[col]] <- NA_real_
      month_ago_row[[col]] <- NA_real_
      month_ago_pct_row[[col]] <- NA_real_
      delta_month_row[[col]] <- NA_real_
    }
    
    bind_rows(
      week_ago_row,
      week_ago_pct_row,
      delta_week_row,
      month_ago_row,
      month_ago_pct_row,
      delta_month_row
    )
  })
}

# ------------------------------------------------------------
# GT table builder
# Now includes forward cell borders, comparison rows, Fwd% row, and 3 decimals for stats
# ------------------------------------------------------------
build_crush_table <- function(wide_df, title = "Crush Spreads", subtitle = "", 
                              decimals = 3, as_of_date, max_year,
                              ethanol_df, corn_df, yield, spread_type,
                              dark_mode = FALSE) {
  
  color_scale <- scales::col_numeric(
    palette = c("#d73027","#f46d43","#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63","#1a9850"),
    domain  = NULL
  )
  
  value_cols  <- names(wide_df)[names(wide_df) != "YEAR"]
  stat_labels <- c("Mean", "Fwd%", "P-10", "P-25", "P-50", "P-75", "P-90")
  gap_labels  <- c("__gap1__", "__gap2__", "__gap3__")
  comparison_labels <- c("Week Ago Value", "Week Ago Percentile", "Δ Week", 
                         "Month Ago Value", "Month Ago Percentile", "Δ Month")
  
  # Build stats rows
  full_df <- append_stats_rows(wide_df, as_of_date, max_year)
  
  # Add gap before comparisons
  gap3 <- tibble(YEAR = "__gap3__")
  for (col in value_cols) gap3[[col]] <- NA_real_
  full_df <- bind_rows(full_df, gap3)
  
  # Add comparison rows
  comparison_df <- build_comparison_rows(wide_df, as_of_date, max_year,
                                         ethanol_df, corn_df, yield, spread_type)
  full_df <- bind_rows(full_df, comparison_df)
  
  # Determine forward cells for borders
  month_to_num <- c(
    January = 1, February = 2, March = 3, April = 4,
    May = 5, June = 6, July = 7, August = 8,
    September = 9, October = 10, November = 11, December = 12
  )
  as_of_date <- as.Date(as_of_date)
  as_of_year <- as.integer(format(as_of_date, "%Y"))
  as_of_month <- as.integer(format(as_of_date, "%m"))
  
  # Pre-compute row indices
  n <- nrow(full_df)
  year_rows <- which(!(full_df$YEAR %in% c(gap_labels, stat_labels, comparison_labels)))
  gap_rows <- which(full_df$YEAR %in% gap_labels)
  stat_rows <- which(full_df$YEAR %in% stat_labels)
  mean_row <- which(full_df$YEAR == "Mean")
  fwd_pct_row <- which(full_df$YEAR == "Fwd%")
  pct_rows <- which(full_df$YEAR %in% c("P-10","P-25","P-50","P-75","P-90"))
  p50_row <- which(full_df$YEAR == "P-50")
  comparison_rows <- which(full_df$YEAR %in% comparison_labels)
  week_ago_row <- which(full_df$YEAR == "Week Ago Value")
  week_ago_pct_row <- which(full_df$YEAR == "Week Ago Percentile")
  delta_week_row <- which(full_df$YEAR == "Δ Week")
  month_ago_row <- which(full_df$YEAR == "Month Ago Value")
  month_ago_pct_row <- which(full_df$YEAR == "Month Ago Percentile")
  delta_month_row <- which(full_df$YEAR == "Δ Month")
  
  # Find forward rows (max_year row)
  forward_year_rows <- which(full_df$YEAR == as.character(max_year))
  
  # ---- Base table ----
  tbl <- full_df |>
    gt(rowname_col = "YEAR") |>
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = md(subtitle)
    ) |>
    cols_align(align = "center", columns = everything()) |>
    fmt_number(columns = all_of(value_cols), rows = year_rows, decimals = decimals) |>
    fmt_number(columns = all_of(value_cols), rows = c(mean_row, pct_rows), decimals = 3) |>
    fmt_percent(columns = all_of(value_cols), rows = fwd_pct_row, decimals = 0) |>
    fmt_number(columns = all_of(value_cols), 
               rows = c(week_ago_row, delta_week_row, month_ago_row, delta_month_row), 
               decimals = 3) |>
    fmt_percent(columns = all_of(value_cols), 
                rows = c(week_ago_pct_row, month_ago_pct_row), 
                decimals = 0) |>
    sub_missing(columns = all_of(value_cols), rows = gap_rows, missing_text = "") |>
    sub_missing(columns = all_of(value_cols), rows = comparison_rows, missing_text = "-") |>
    sub_missing(columns = all_of(value_cols), rows = fwd_pct_row, missing_text = "-") |>
    text_transform(
      locations = cells_stub(rows = gap_rows),
      fn = function(x) ""
    )
  
  # ---- Color scale on year data rows only (not forward year) ----
  non_forward_year_rows <- year_rows[!(year_rows %in% forward_year_rows)]
  for (col in value_cols) {
    tbl <- tbl |>
      data_color(columns = all_of(col), rows = non_forward_year_rows, fn = color_scale)
  }
  
  # ---- Color scale on forward year rows (they'll get border on top) ----
  if (length(forward_year_rows) > 0) {
    for (col in value_cols) {
      tbl <- tbl |>
        data_color(columns = all_of(col), rows = forward_year_rows, fn = color_scale)
    }
  }
  
  # ---- Border forward cells (dodger blue) ----
  forward_border_color <- if (dark_mode) "#4169E1" else "#1E90FF"  # Dodger blue
  
  if (length(forward_year_rows) > 0) {
    for (col in value_cols) {
      if (col %in% names(month_to_num)) {
        month_num <- month_to_num[col]
        # Border if month >= as_of_month
        if (month_num >= as_of_month) {
          tbl <- tbl |>
            tab_style(
              style = cell_borders(sides = "all", 
                                   color = forward_border_color, 
                                   weight = px(2)),
              locations = cells_body(columns = all_of(col), rows = forward_year_rows)
            )
        }
      } else if (col == "CalYear") {
        # Border CalYear for forward year
        tbl <- tbl |>
          tab_style(
            style = cell_borders(sides = "all", 
                                 color = forward_border_color, 
                                 weight = px(2)),
            locations = cells_body(columns = all_of(col), rows = forward_year_rows)
          )
      }
    }
  }
  
  # ---- Theme options ----
  if (dark_mode) {
    tbl <- tbl |>
      tab_options(
        table.font.size                = px(12),
        table.background.color         = "#222222",
        heading.background.color       = "#222222",
        column_labels.background.color = "#333333",
        stub.background.color          = "#2a2a2a",
        row.striping.background_color  = "#2a2a2a",
        data_row.padding               = px(5),
        column_labels.font.weight      = "bold",
        heading.title.font.size        = px(16),
        heading.subtitle.font.size     = px(11),
        table.border.top.style         = "solid",
        table.border.top.width         = px(2),
        table.border.top.color         = "#18BC9C"
      ) |>
      tab_style(style = cell_text(color = "#ffffff", weight = "bold", size = px(11)),
                locations = cells_column_labels(everything())) |>
      tab_style(style = cell_text(color = "#ffffff"),
                locations = cells_body()) |>
      tab_style(style = cell_text(color = "#ffffff", weight = "bold"),
                locations = cells_stub())
  } else {
    tbl <- tbl |>
      tab_options(
        table.font.size                = px(12),
        data_row.padding               = px(5),
        column_labels.font.weight      = "bold",
        heading.title.font.size        = px(16),
        heading.subtitle.font.size     = px(11),
        table.border.top.style         = "solid",
        table.border.top.width         = px(2),
        table.border.top.color         = "#2C3E50"
      ) |>
      tab_style(style = cell_text(weight = "bold", size = px(11)),
                locations = cells_column_labels(everything()))
  }
  
  # ---- CalYear column ----
  tbl <- tbl |>
    tab_style(
      style = list(cell_fill(color = if (dark_mode) "#444444" else "#ECF0F1"),
                   cell_text(weight = "bold")),
      locations = cells_column_labels(columns = "CalYear")
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "CalYear")
    ) |>
    tab_style(
      style = cell_borders(sides = "left",
                           color = if (dark_mode) "#666666" else "#DEE2E6",
                           weight = px(2)),
      locations = cells_body(columns = "CalYear")
    )
  
  # ---- Gap rows: top border as visual separator ----
  gap_border <- if (dark_mode) "#555555" else "#BBBBBB"
  tbl <- tbl |>
    tab_style(
      style = cell_borders(sides = "top", color = gap_border, weight = px(2)),
      locations = cells_body(rows = gap_rows)
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = gap_border, weight = px(2)),
      locations = cells_stub(rows = gap_rows)
    )
  
  # ---- Mean row: bold + medium green fill ----
  mean_fill <- if (dark_mode) "#2d5a27" else "#c7e6b0"
  tbl <- tbl |>
    tab_style(
      style = list(cell_fill(color = mean_fill), cell_text(weight = "bold")),
      locations = cells_body(rows = mean_row)
    ) |>
    tab_style(
      style = list(cell_fill(color = mean_fill), cell_text(weight = "bold")),
      locations = cells_stub(rows = mean_row)
    )
  
  # ---- Forward Percentile row: bold + light yellow fill ----
  fwd_pct_fill <- if (dark_mode) "#3d3d1a" else "#fff9e6"
  tbl <- tbl |>
    tab_style(
      style = list(cell_fill(color = fwd_pct_fill), cell_text(weight = "bold")),
      locations = cells_body(rows = fwd_pct_row)
    ) |>
    tab_style(
      style = list(cell_fill(color = fwd_pct_fill), cell_text(weight = "bold")),
      locations = cells_stub(rows = fwd_pct_row)
    )
  
  # ---- Percentile rows: light green fill ----
  pct_fill <- if (dark_mode) "#1e3d1a" else "#eaf5e3"
  tbl <- tbl |>
    tab_style(
      style = cell_fill(color = pct_fill),
      locations = cells_body(rows = pct_rows)
    ) |>
    tab_style(
      style = cell_fill(color = pct_fill),
      locations = cells_stub(rows = pct_rows)
    )
  
  # ---- P-50: bold ----
  tbl <- tbl |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = p50_row)) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_stub(rows = p50_row))
  
  # ---- Comparison rows: light blue fill ----
  comparison_fill <- if (dark_mode) "#1e2d3d" else "#f0f8ff"
  tbl <- tbl |>
    tab_style(
      style = cell_fill(color = comparison_fill),
      locations = cells_body(rows = comparison_rows)
    ) |>
    tab_style(
      style = cell_fill(color = comparison_fill),
      locations = cells_stub(rows = comparison_rows)
    )
  
  # ---- Delta rows: bold ----
  tbl <- tbl |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = c(delta_week_row, delta_month_row))
    )
  
  # ---- Percentile comparison rows: bold ----
  tbl <- tbl |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = c(week_ago_pct_row, month_ago_pct_row))
    )
  
  tbl
}

# ------------------------------------------------------------
# Time series plot built from the same wide df that feeds the gt table.
# Pivots back to long, constructs a date from YEAR + month name,
# and plots a single line ordered chronologically.
# Stat rows (Mean, P-xx, gap rows, comparison rows) are excluded automatically.
# ------------------------------------------------------------
build_crush_ts_plot <- function(wide_df, title = "Crush Spread Over Time",
                                date_min = NULL, date_max = NULL,
                                dark_mode = FALSE) {
  
  month_to_num <- c(
    January = 1, February = 2, March = 3,    April = 4,
    May     = 5, June     = 6, July  = 7,    August    = 8,
    September = 9, October = 10, November = 11, December = 12
  )
  
  stat_labels <- c("Mean", "Fwd%", "P-10", "P-25", "P-50", "P-75", "P-90",
                   "__gap1__", "__gap2__", "__gap3__",
                   "Week Ago Value", "Week Ago Percentile", "Δ Week", 
                   "Month Ago Value", "Month Ago Percentile", "Δ Month")
  
  month_cols <- names(wide_df)[names(wide_df) %in% names(month_to_num)]
  
  df <- wide_df |>
    filter(!(YEAR %in% stat_labels)) |>
    mutate(YEAR = as.integer(YEAR)) |>
    select(YEAR, all_of(month_cols)) |>
    pivot_longer(cols = all_of(month_cols),
                 names_to  = "MONTH_NAME",
                 values_to = "VALUE") |>
    filter(!is.na(VALUE)) |>
    mutate(
      MONTH_NUM     = month_to_num[MONTH_NAME],
      CONTRACT_DATE = as.Date(paste(YEAR, MONTH_NUM, "01", sep = "-"))
    ) |>
    arrange(CONTRACT_DATE)
  
  # Apply date filter from slider if provided
  if (!is.null(date_min)) df <- df |> filter(CONTRACT_DATE >= as.Date(date_min))
  if (!is.null(date_max)) df <- df |> filter(CONTRACT_DATE <= as.Date(date_max))
  
  if (dark_mode) {
    paper_bg <- "#222222"; plot_bg <- "#2a2a2a"; grid_col <- "#444444"
    font_col <- "#ffffff"; line_col <- "#18BC9C"; zero_col <- "#666666"
  } else {
    paper_bg <- "white";   plot_bg <- "#F8F9FA"; grid_col <- "#E5E5E5"
    font_col <- "#2C3E50"; line_col <- "#2C3E50"; zero_col <- "#AAAAAA"
  }
  
  if (nrow(df) == 0) {
    return(plot_ly() |> layout(title = list(text = "No data", x = 0.02)))
  }
  
  plot_ly(data = df) |>
    add_trace(
      x    = ~CONTRACT_DATE,
      y    = ~VALUE,
      type = "scatter",
      mode = "lines",
      line = list(color = line_col, width = 1.8),
      hovertemplate = paste0("<b>Contract:</b> %{x|%b %Y}<br>",
                             "<b>Spread:</b> $%{y:.3f}<br>",
                             "<extra></extra>")
    ) |>
    layout(
      title  = list(text = title, x = 0.02, xanchor = "left",
                    font = list(size = 13, color = font_col)),
      xaxis  = list(title = "Contract Date", color = font_col,
                    gridcolor = grid_col, showgrid = TRUE, zeroline = FALSE),
      yaxis  = list(title = "Crush Spread ($/gal)", color = font_col,
                    gridcolor = grid_col, showgrid = TRUE,
                    zeroline = TRUE, zerolinecolor = zero_col, zerolinewidth = 1),
      hovermode     = "x unified",
      margin        = list(l = 60, r = 20, b = 50, t = 40),
      paper_bgcolor = paper_bg,
      plot_bgcolor  = plot_bg,
      font          = list(family = "Arial, sans-serif", size = 11, color = font_col),
      showlegend    = FALSE
    )
}