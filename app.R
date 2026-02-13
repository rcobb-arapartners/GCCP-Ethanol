# app.R

source("01-MetaLoad.R")
source("02-DataBuild.R")
source("03-VisualBuild.R")

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = "Crush Spreads",
  theme = light_theme,
  
  nav_panel(
    title = "Liquidations ON Corn Months",
    icon  = icon("table"),
    
    page_sidebar(
      
      # ---- Sidebar ----
      sidebar = sidebar(
        open  = "desktop",
        width = 260,
        title = "Controls",
        
        dateInput("max_date",   "Max Date (query cutoff)",    value = Sys.Date()),
        dateInput("as_of_date", "As-of Date (expiration logic)", value = Sys.Date()),
        numericInput("yield", "Yield (gal/bu)", value = 2.9, min = 0.1, step = 0.01),
        actionButton("load_data", "Load Data", class = "btn-primary", width = "100%"),
        hr(),
        uiOutput("status"),
        hr(),
        h5("Export Data"),
        downloadButton("dl_on",      "Download ON Months CSV",    class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_off",     "Download OFF Months CSV",   class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_ethanol", "Download Ethanol Raw CSV",  class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_corn",    "Download Corn Raw CSV",     class = "btn-sm", style = "width:100%;")
      ),
      
      # ---- Two tables side by side ----
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          full_screen = TRUE,
          card_header(h5("Liquidations ON Corn Months", style = "margin:0;")),
          card_body(
            style = "overflow-y:auto; overflow-x:auto; padding:0.5rem;",
            gt_output("table_on")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header(h5("Liquidations OFF Corn Months", style = "margin:0;")),
          card_body(
            style = "overflow-y:auto; overflow-x:auto; padding:0.5rem;",
            gt_output("table_off")
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(checkboxInput("dark_mode", "Dark Mode", value = FALSE)),
  
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML("
      .btn-primary { background-color:#2C3E50 !important; border-color:#2C3E50 !important; font-weight:500; }
      .btn-primary:hover { background-color:#34495E !important; border-color:#34495E !important; }
      .card { border:1px solid var(--bs-border-color); box-shadow:0 2px 4px rgba(0,0,0,0.05); margin-bottom:4px !important; }
      .card-header { background-color:var(--bs-light); border-bottom:1px solid var(--bs-border-color); font-weight:600; padding:0.4rem; }
      [data-bs-theme='dark'] .card-header { background-color:var(--bs-dark); }
      .form-check-input:checked { background-color:#18BC9C; border-color:#18BC9C; }
      .status-text { font-size:0.85rem; padding:0.5rem; background-color:var(--bs-light); border-radius:4px; margin-top:0.5rem; }
      [data-bs-theme='dark'] .status-text { background-color:var(--bs-dark); }
    "))),
    shinyjs::hidden(div(
      id    = "loading_overlay",
      style = "position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.6);z-index:9999;display:flex;align-items:center;justify-content:center;",
      div(
        style = "background:#000;color:#fff;padding:30px 40px;border-radius:8px;border:1px solid #333;text-align:center;font-family:'Courier New',monospace;",
        tags$div(class = "spinner-border text-light", role = "status", style = "margin-bottom:15px;width:2rem;height:2rem;"),
        tags$div("Calling MarketView API...", style = "font-size:14px;")
      )
    ))
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  session$setCurrentTheme(light_theme)
  observeEvent(input$dark_mode, {
    if (isTRUE(input$dark_mode)) session$setCurrentTheme(dark_theme) else session$setCurrentTheme(light_theme)
  })
  
  rv <- reactiveValues(
    loaded      = FALSE,
    ethanol_df  = NULL,
    corn_df     = NULL,
    prices_full = NULL,
    last_load   = NULL,
    err         = NULL
  )
  
  # ---- Status panel ----
  output$status <- renderUI({
    if (isTRUE(rv$loaded) && is.null(rv$err)) {
      div(class = "status-text",
          tags$div(style = "font-size:12px;", paste0("✓ Loaded: ", rv$last_load)),
          tags$div(style = "font-size:12px;", paste0("✓ Contracts: ", nrow(rv$prices_full))))
    } else if (!is.null(rv$err)) {
      div(class = "status-text", style = "color:#E74C3C; border-left:3px solid #E74C3C;",
          tags$div(style = "font-weight:600;", "Error:"),
          tags$div(style = "font-size:12px;", rv$err))
    } else {
      div(class = "status-text", style = "color:#7F8C8D;",
          tags$div(style = "font-size:12px;", "⚡ Idle. Click 'Load Data' to fetch."))
    }
  })
  
  # ---- Load data ----
  observeEvent(input$load_data, {
    rv$err <- NULL
    shinyjs::show("loading_overlay")
    
    tryCatch({
      max_d   <- isolate(as.Date(input$max_date))
      as_of_d <- isolate(as.Date(input$as_of_date))
      yld     <- isolate(as.numeric(input$yield))
      
      ethanol_df <- build_marketview_daily_forward_data_on_corn_months(
        base_symbols   = "GCU",
        calendar_years = 2007:2026,
        month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
        min_date       = "2000-01-01",
        max_date       = max_d,
        as_of_date     = as_of_d
      )
      
      corn_df <- build_marketview_daily_forward_data_on_corn_months(
        base_symbols   = "ZC",
        calendar_years = 2007:2026,
        month_codes    = c("H","K","N","U","Z"),
        min_date       = "2000-01-01",
        max_date       = max_d,
        as_of_date     = as_of_d
      )
      
      rv$ethanol_df  <- ethanol_df
      rv$corn_df     <- corn_df
      rv$prices_full <- build_prices_full(ethanol_df, corn_df, as_of_d, yld)
      rv$loaded      <- TRUE
      rv$last_load   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      shinyjs::hide("loading_overlay")
    }, error = function(e) {
      rv$err <- as.character(e$message)
      shinyjs::hide("loading_overlay")
    })
  })
  
  # ---- Rebuild prices_full when as_of_date or yield changes (no re-fetch) ----
  observeEvent(list(input$as_of_date, input$yield), {
    req(rv$loaded, rv$ethanol_df, rv$corn_df)
    as_of_d <- as.Date(input$as_of_date)
    yld     <- as.numeric(input$yield)
    
    ethanol_restamped <- rv$ethanol_df |>
      mutate(EXPIRED = if_else(as.Date(as_of_d) > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    corn_restamped <- rv$corn_df |>
      mutate(EXPIRED = if_else(as.Date(as_of_d) > ceiling_date(CONTRACT_DATE_ID, 'months') - days(1), 1, 0))
    
    rv$prices_full <- build_prices_full(ethanol_restamped, corn_restamped, as_of_d, yld)
  })
  
  # ---- GT tables ----
  output$table_on <- render_gt({
    req(rv$loaded, rv$prices_full)
    wide <- shape_for_table(rv$prices_full, "CRUSH_SPREAD_ON_MONTH")
    build_crush_table(wide, title = "Liquidations ON Corn Months", decimals = 3, dark_mode = input$dark_mode)
  })
  
  output$table_off <- render_gt({
    req(rv$loaded, rv$prices_full)
    wide <- shape_for_table(rv$prices_full, "CRUSH_SPREAD_OFF_MONTH")
    build_crush_table(wide, title = "Liquidations OFF Corn Months", decimals = 3, dark_mode = input$dark_mode)
  })
  
  # ---- Downloads ----
  output$dl_on <- downloadHandler(
    filename = function() paste0("liquidations_on_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv$prices_full)
      shape_for_table(rv$prices_full, "CRUSH_SPREAD_ON_MONTH") |> write_csv(file)
    }
  )
  
  output$dl_off <- downloadHandler(
    filename = function() paste0("liquidations_off_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv$prices_full)
      shape_for_table(rv$prices_full, "CRUSH_SPREAD_OFF_MONTH") |> write_csv(file)
    }
  )
  
  output$dl_ethanol <- downloadHandler(
    filename = function() paste0("ethanol_raw_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv$ethanol_df)
      write_csv(rv$ethanol_df, file)
    }
  )
  
  output$dl_corn <- downloadHandler(
    filename = function() paste0("corn_raw_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv$corn_df)
      write_csv(rv$corn_df, file)
    }
  )
}

shinyApp(ui, server)