# 03-VisualBuild.R
# Visual builders with dark mode support and dual-axis charts

# ------------------------------------------------------------
# GT table with dark mode and red-green color scale
# ------------------------------------------------------------
build_liquidations_table <- function(liq_df, title = "Liquidations", decimals = 3, dark_mode = FALSE) {
  
  # Color palette: red (low) to green (high)
  color_scale <- scales::col_numeric(
    palette = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    domain = NULL
  )
  
  # Base table
  tbl <- liq_df |>
    gt(rowname_col = "eth_year") |>
    tab_header(title = md(paste0("**", title, "**"))) |>
    fmt_number(columns = -eth_year, decimals = decimals) |>
    cols_align(align = "center", columns = everything())
  
  # Apply color scale to all numeric columns except year
  numeric_cols <- names(liq_df)[names(liq_df) != "eth_year"]
  for (col in numeric_cols) {
    tbl <- tbl |>
      data_color(columns = all_of(col), fn = color_scale)
  }
  
  # Dark mode styling
  if (dark_mode) {
    tbl <- tbl |>
      tab_options(
        table.font.size = px(12),
        table.background.color = "#222222",
        heading.background.color = "#222222",
        column_labels.background.color = "#333333",
        stub.background.color = "#2a2a2a",
        row.striping.background_color = "#2a2a2a",
        data_row.padding = px(6),
        column_labels.font.weight = "bold",
        heading.title.font.size = px(16),
        table.border.top.style = "solid",
        table.border.top.width = px(2),
        table.border.top.color = "#18BC9C"
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
        table.font.size = px(12),
        data_row.padding = px(6),
        column_labels.font.weight = "bold",
        heading.title.font.size = px(16),
        table.border.top.style = "solid",
        table.border.top.width = px(2),
        table.border.top.color = "#2C3E50"
      ) |>
      tab_style(style = cell_text(weight = "bold", size = px(11)),
                locations = cells_column_labels(everything()))
  }
  
  # Highlight CalYear column
  tbl |>
    tab_style(
      style = list(
        cell_fill(color = if (dark_mode) "#444444" else "#ECF0F1"),
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(columns = "CalYear")
    ) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = "CalYear")) |>
    tab_style(
      style = cell_borders(sides = c("left"),
                           color = if (dark_mode) "#666666" else "#DEE2E6",
                           weight = px(2)),
      locations = cells_body(columns = "CalYear")
    )
}

# ------------------------------------------------------------
# Dual-axis Plotly lifepath: Crush + Ethanol (left), Corn (right)
# ------------------------------------------------------------
build_dual_axis_lifepath_plot <- function(crush_df, ethanol_contract, yield = 2.9, dark_mode = FALSE) {
  
  df_plot <- crush_df |>
    filter(eth_desc == ethanol_contract) |>
    mutate(trading_month = floor_date(date, "months")) |>
    group_by(corn_desc, eth_desc, trading_month) |>
    mutate(rolling_avg_corn = cummean(corn_close)) |>
    ungroup() |>
    mutate(
      crush_spread = eth_close - (rolling_avg_corn / 100 / yield),
      implied_spread = eth_close - (corn_close / 100 / yield),
      spread_used = if_else(expired, crush_spread, implied_spread)
    ) |>
    arrange(date)
  
  if (nrow(df_plot) == 0) {
    return(plot_ly() |> layout(title = list(text = "No data", x = 0.02)))
  }
  
  # Dark mode colors
  if (dark_mode) {
    paper_bg <- "#222222"
    plot_bg <- "#2a2a2a"
    grid_color <- "#444444"
    font_color <- "#ffffff"
    crush_color <- "#18BC9C"
    ethanol_color <- "#3498DB"
    corn_color <- "#E67E22"
  } else {
    paper_bg <- "white"
    plot_bg <- "#F8F9FA"
    grid_color <- "#E5E5E5"
    font_color <- "#2C3E50"
    crush_color <- "#2C3E50"
    ethanol_color <- "#3498DB"
    corn_color <- "#E67E22"
  }
  
  # Create dual-axis plot
  plot_ly(data = df_plot) |>
    # Left axis: Crush spread
    add_trace(
      x = ~date,
      y = ~spread_used,
      type = "scatter",
      mode = "lines",
      name = "Crush Spread",
      line = list(color = crush_color, width = 2),
      yaxis = "y1",
      hovertemplate = paste0("<b>Date:</b> %{x}<br>",
                             "<b>Crush:</b> $%{y:.4f}<br>",
                             "<extra></extra>")
    ) |>
    # Left axis: Ethanol price
    add_trace(
      x = ~date,
      y = ~eth_close,
      type = "scatter",
      mode = "lines",
      name = "Ethanol Price",
      line = list(color = ethanol_color, width = 1.5, dash = "dot"),
      yaxis = "y1",
      hovertemplate = paste0("<b>Date:</b> %{x}<br>",
                             "<b>Ethanol:</b> $%{y:.4f}<br>",
                             "<extra></extra>")
    ) |>
    # Right axis: Corn price (convert to cents)
    add_trace(
      x = ~date,
      y = ~corn_close,
      type = "scatter",
      mode = "lines",
      name = "Corn Price",
      line = list(color = corn_color, width = 1.5, dash = "dash"),
      yaxis = "y2",
      hovertemplate = paste0("<b>Date:</b> %{x}<br>",
                             "<b>Corn:</b> %{y:.2f}¢<br>",
                             "<extra></extra>")
    ) |>
    layout(
      title = list(text = ethanol_contract, x = 0.02, xanchor = "left",
                   font = list(size = 14, family = "Arial, sans-serif", color = font_color)),
      xaxis = list(
        title = "Date",
        gridcolor = grid_color,
        showgrid = TRUE,
        zeroline = FALSE,
        color = font_color
      ),
      # Left Y-axis: Crush spread + Ethanol price ($/gal)
      yaxis = list(
        title = "Crush Spread & Ethanol Price ($/gal)",
        titlefont = list(color = font_color),
        tickfont = list(color = font_color),
        gridcolor = grid_color,
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinecolor = if (dark_mode) "#666666" else "#999999",
        zerolinewidth = 1
      ),
      # Right Y-axis: Corn price (cents/bu)
      yaxis2 = list(
        title = "Corn Price (¢/bu)",
        titlefont = list(color = corn_color),
        tickfont = list(color = corn_color),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      hovermode = "x unified",
      legend = list(
        x = 0.02,
        y = 0.98,
        bgcolor = if (dark_mode) "rgba(42,42,42,0.8)" else "rgba(255,255,255,0.8)",
        bordercolor = if (dark_mode) "#666666" else "#CCCCCC",
        borderwidth = 1,
        font = list(color = font_color)
      ),
      margin = list(l = 70, r = 70, b = 60, t = 80),
      paper_bgcolor = paper_bg,
      plot_bgcolor = plot_bg,
      font = list(family = "Arial, sans-serif", size = 11, color = font_color)
    )
}