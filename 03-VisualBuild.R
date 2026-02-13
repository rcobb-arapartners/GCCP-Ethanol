# 03-VisualBuild.R

# ------------------------------------------------------------
# Append stats rows to the wide year x month data frame.
# Layout matches the reference:
#   [year rows]
#   __gap1__   <- thin separator
#   Mean
#   __gap2__   <- thin separator
#   P-10, P-25, P-50, P-75, P-90
# ------------------------------------------------------------
append_stats_rows <- function(wide_df) {
  
  value_cols <- names(wide_df)[names(wide_df) != "YEAR"]
  
  make_blank <- function(label) {
    row <- tibble(YEAR = label)
    for (col in value_cols) row[[col]] <- NA_real_
    row
  }
  
  make_stat_row <- function(label, fn) {
    row <- tibble(YEAR = label)
    for (col in value_cols) row[[col]] <- fn(as.numeric(unlist(wide_df[[col]])))
    row
  }
  
  bind_rows(
    wide_df,
    make_blank("__gap1__"),
    make_stat_row("Mean", function(x) mean(x,              na.rm = TRUE)),
    make_blank("__gap2__"),
    make_stat_row("P-10", function(x) quantile(x, 0.10,   na.rm = TRUE)),
    make_stat_row("P-25", function(x) quantile(x, 0.25,   na.rm = TRUE)),
    make_stat_row("P-50", function(x) quantile(x, 0.50,   na.rm = TRUE)),
    make_stat_row("P-75", function(x) quantile(x, 0.75,   na.rm = TRUE)),
    make_stat_row("P-90", function(x) quantile(x, 0.90,   na.rm = TRUE))
  )
}

# ------------------------------------------------------------
# GT table builder
# Row targeting uses integer indices (not bare column expressions)
# to avoid gt tidy-eval scoping issues with local variables.
# ------------------------------------------------------------
build_crush_table <- function(wide_df, title = "Crush Spreads", decimals = 3, dark_mode = FALSE) {
  
  color_scale <- scales::col_numeric(
    palette = c("#d73027","#f46d43","#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63","#1a9850"),
    domain  = NULL
  )
  
  value_cols  <- names(wide_df)[names(wide_df) != "YEAR"]
  stat_labels <- c("Mean", "P-10", "P-25", "P-50", "P-75", "P-90")
  gap_labels  <- c("__gap1__", "__gap2__")
  
  full_df <- append_stats_rows(wide_df)
  
  # Pre-compute row indices for each group
  n           <- nrow(full_df)
  year_rows   <- which(!(full_df$YEAR %in% c(gap_labels, stat_labels)))
  gap_rows    <- which(full_df$YEAR %in% gap_labels)
  stat_rows   <- which(full_df$YEAR %in% stat_labels)
  mean_row    <- which(full_df$YEAR == "Mean")
  pct_rows    <- which(full_df$YEAR %in% c("P-10","P-25","P-50","P-75","P-90"))
  p50_row     <- which(full_df$YEAR == "P-50")
  
  # ---- Base table ----
  tbl <- full_df |>
    gt(rowname_col = "YEAR") |>
    tab_header(title = md(paste0("**", title, "**"))) |>
    cols_align(align = "center", columns = everything()) |>
    fmt_number(columns = all_of(value_cols), rows = year_rows, decimals = decimals) |>
    fmt_number(columns = all_of(value_cols), rows = stat_rows, decimals = 2) |>
    sub_missing(columns = all_of(value_cols), rows = gap_rows, missing_text = "") |>
    text_transform(
      locations = cells_stub(rows = gap_rows),
      fn = function(x) ""
    )
  
  # ---- Color scale on year data rows only ----
  for (col in value_cols) {
    tbl <- tbl |>
      data_color(columns = all_of(col), rows = year_rows, fn = color_scale)
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
  
  tbl
}