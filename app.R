# app.R
# Crush Spreads App

source("01-MetaLoad.R")
source("02-DataBuild.R")
source("03-VisualBuild.R")

ui <- page_navbar(
  title = "Crush Spreads",
  theme = light_theme,
  
  nav_panel(title = "Liquidations ON Corn Months", icon = icon("table"),
            page_sidebar(
              sidebar = sidebar(open = "desktop", width = 340, title = "Controls",
                                dateInput("max_date", "Max Date (query cutoff)", value = Sys.Date()),
                                numericInput("yield", "Yield (gal/bu)", value = 2.9, min = 0.1, step = 0.01),
                                actionButton("load_data", "Load Data", class = "btn-primary", width = "100%"),
                                hr(),
                                uiOutput("status"),
                                hr(),
                                
                                # Download buttons
                                h5("Export Data"),
                                downloadButton("download_liquidations", "Download Liquidations CSV", class = "btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                                downloadButton("download_lifepath", "Download Lifepath CSV", class = "btn-sm", style = "width: 100%;")
              ),
              
              # Top row: GT table (1/3 width) and lifepath (2/3 width)
              layout_columns(
                col_widths = c(4, 8),
                
                # Left: GT table with as-of date filter above it
                card(
                  full_screen = TRUE,
                  max_height = "48vh",
                  card_header(
                    div(
                      h5("Liquidations ON Corn Months", style = "margin: 0 0 8px 0;"),
                      dateInput("as_of_date", "As-of Date (expiration logic)", value = Sys.Date(), width = "100%")
                    )
                  ),
                  card_body(style = "overflow-y: auto; overflow-x: auto; max-height: 42vh; padding: 0.5rem;", gt_output("liquidations_table"))
                ),
                
                # Right: Lifepath with contract selector and date slider
                card(
                  full_screen = TRUE,
                  max_height = "48vh",
                  card_header(
                    div(
                      selectInput("ethanol_contract", "Ethanol Contract", choices = c("Load data first" = ""), width = "100%"),
                      conditionalPanel(
                        condition = "output.data_loaded",
                        uiOutput("date_slider_ui")
                      )
                    )
                  ),
                  card_body(style = "overflow-y: auto; max-height: 42vh;", plotlyOutput("lifepath_plot", height = "400px"))
                )
              ),
              
              # Bottom row: Additional tools (minimal spacing)
              layout_columns(
                col_widths = c(12),
                card(
                  max_height = "15vh",
                  card_header(h6("Additional Analysis Tools", style = "margin: 0; font-size: 0.9rem;")),
                  card_body(
                    style = "padding: 0.3rem;",
                    div(style = "text-align: center; color: #7F8C8D; padding: 0.3rem;",
                        p(style = "margin: 0; font-size: 0.85rem;", strong("Coming Soon"), " - Additional analysis tools will be added here."))
                  )
                )
              )
            )
  ),
  
  nav_panel(title = "Liquidations OFF Corn Months", icon = icon("table-cells"),
            page_sidebar(
              sidebar = sidebar(open = "desktop", width = 340, title = "Controls",
                                p("OFF Corn Months is under construction.", style = "color: #7F8C8D; font-style: italic;"),
                                actionButton("load_data_off", "Load Data", class = "btn-primary", width = "100%")),
              card(full_screen = TRUE,
                   card_header(h4("Liquidations OFF Corn Months", style = "margin: 0;")),
                   card_body(div(style = "text-align: center; padding: 2rem;",
                                 h3("Under Construction", style = "color: #7F8C8D;"),
                                 p("This tab will be wired later."))))
            )
  ),
  
  nav_spacer(),
  nav_item(checkboxInput("dark_mode", "Dark Mode", value = FALSE)),
  
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML("
      .btn-primary { background-color: #2C3E50 !important; border-color: #2C3E50 !important; font-weight: 500; }
      .btn-primary:hover { background-color: #34495E !important; border-color: #34495E !important; }
      .card { border: 1px solid var(--bs-border-color); box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 3px !important; }
      .card-header { background-color: var(--bs-light); border-bottom: 1px solid var(--bs-border-color); font-weight: 600; padding: 0.4rem; }
      [data-bs-theme='dark'] .card-header { background-color: var(--bs-dark); }
      .form-check-input:checked { background-color: #18BC9C; border-color: #18BC9C; }
      .status-text { font-size: 0.85rem; padding: 0.5rem; background-color: var(--bs-light); border-radius: 4px; margin-top: 0.5rem; }
      [data-bs-theme='dark'] .status-text { background-color: var(--bs-dark); }
      .bslib-page-sidebar > div { gap: 3px !important; }
      .layout-columns { gap: 3px !important; }
      .bslib-gap-spacing { gap: 3px !important; }
    "))),
    shinyjs::hidden(div(
      id = "loading_overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.6); z-index: 9999; display: flex; align-items: center; justify-content: center;",
      div(style = "background: #000; color: #fff; padding: 30px 40px; border-radius: 8px; border: 1px solid #333; text-align: center; font-family: 'Courier New', monospace;",
          tags$div(class = "spinner-border text-light", role = "status", style = "margin-bottom: 15px; width: 2rem; height: 2rem;"),
          tags$div("Calling MarketView API...", style = "font-size: 14px;"))
    ))
  )
)

server <- function(input, output, session) {
  session$setCurrentTheme(light_theme)
  observeEvent(input$dark_mode, {
    if (isTRUE(input$dark_mode)) session$setCurrentTheme(dark_theme) else session$setCurrentTheme(light_theme)
  })
  
  rv <- reactiveValues(
    loaded = FALSE, 
    ethanol_df = NULL, 
    corn_df = NULL, 
    crush_df = NULL,
    crush_df_full = NULL,
    last_load = NULL, 
    err = NULL,
    contract_date_min = NULL,
    contract_date_max = NULL
  )
  
  # Flag for conditionalPanel
  output$data_loaded <- reactive({ rv$loaded })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Dynamic date slider
  output$date_slider_ui <- renderUI({
    req(rv$loaded, rv$crush_df_full, input$ethanol_contract, nzchar(input$ethanol_contract))
    
    contract_dates <- rv$crush_df_full |>
      filter(eth_desc == input$ethanol_contract) |>
      pull(date)
    
    if (length(contract_dates) == 0) return(NULL)
    
    date_min <- min(contract_dates, na.rm = TRUE)
    date_max <- max(contract_dates, na.rm = TRUE)
    
    rv$contract_date_min <- date_min
    rv$contract_date_max <- date_max
    
    sliderInput(
      "date_range",
      "Lifepath Date Range:",
      min = date_min,
      max = date_max,
      value = c(date_min, date_max),
      timeFormat = "%b %Y",
      width = "100%"
    )
  })
  
  output$status <- renderUI({
    if (isTRUE(rv$loaded) && is.null(rv$err)) {
      liq_count <- if (!is.null(liq_df())) nrow(liq_df()) else 0
      div(class = "status-text",
          tags$div(style = "font-size: 12px;", paste0("✓ Loaded: ", rv$last_load)),
          tags$div(style = "font-size: 12px;", paste0("✓ Crush rows: ", format(nrow(rv$crush_df_full), big.mark = ","))),
          tags$div(style = "font-size: 12px;", paste0("✓ Liquidation years: ", liq_count)))
    } else if (!is.null(rv$err)) {
      div(class = "status-text", style = "color: #E74C3C; border-left: 3px solid #E74C3C;",
          tags$div(style = "font-weight: 600;", "Error:"),
          tags$div(style = "font-size: 12px;", rv$err))
    } else {
      div(class = "status-text", style = "color: #7F8C8D;",
          tags$div(style = "font-size: 12px;", "⚡ Idle. Click 'Load Data' to fetch."))
    }
  })
  
  observeEvent(input$load_data, {
    rv$err <- NULL
    max_d <- isolate(as.Date(input$max_date))
    
    shinyjs::show("loading_overlay")
    tryCatch({
      ethanol_df <- fetch_marketview_data(
        base_symbols = "GCU", 
        calendar_years = 2007:2026,
        month_codes = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
        min_date = "2000-01-01",
        max_date = max_d
      )
      
      corn_df <- fetch_marketview_data(
        base_symbols = "ZC", 
        calendar_years = 2007:2026,
        month_codes = c("H", "K", "N", "U", "Z"),
        min_date = "2000-01-01",
        max_date = max_d
      )
      
      rv$ethanol_df <- ethanol_df
      rv$corn_df <- corn_df
      rv$loaded <- TRUE
      rv$last_load <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Build initial crush_df_full based on current as_of_date
      asof <- as.Date(input$as_of_date)
      crush_df_full <- build_crush_df(ethanol_df, corn_df, asof) |> na.omit()
      rv$crush_df_full <- crush_df_full
      rv$crush_df <- crush_df_full
      
      ethanol_choices <- crush_df_full |> distinct(eth_desc) |> arrange(eth_desc) |> pull(eth_desc)
      if (length(ethanol_choices) > 0) {
        updateSelectInput(session, "ethanol_contract", choices = ethanol_choices, selected = ethanol_choices[1])
      } else {
        updateSelectInput(session, "ethanol_contract", choices = c("No contracts" = ""), selected = "")
      }
      
      shinyjs::hide("loading_overlay")
    }, error = function(e) {
      rv$err <- as.character(e$message)
      shinyjs::hide("loading_overlay")
    })
  })
  
  # REACTIVE: Liquidations dataframe - depends on crush_df_full, as_of_date, and yield
  liq_df <- reactive({
    req(rv$loaded, rv$crush_df_full)
    
    # Trigger on as_of_date and yield
    asof <- as.Date(input$as_of_date)
    yld <- as.numeric(input$yield)
    
    print(paste("Recalculating liquidations at", Sys.time(), "for as-of date:", asof))
    print(paste("Number of rows in crush_df_full:", nrow(rv$crush_df_full)))
    
    result <- calc_liquidations(rv$crush_df_full, yld, asof)
    
    print(paste("Liquidations table has", nrow(result), "years"))
    
    result
  })
  
  # Filter crush data when date slider changes
  observeEvent(input$date_range, {
    req(rv$loaded, rv$crush_df_full, input$ethanol_contract)
    
    rv$crush_df <- rv$crush_df_full |>
      filter(
        eth_desc == input$ethanol_contract,
        date >= input$date_range[1], 
        date <= input$date_range[2]
      )
  })
  
  # Update filtered data when contract changes
  observeEvent(input$ethanol_contract, {
    req(rv$loaded, rv$crush_df_full, input$ethanol_contract, nzchar(input$ethanol_contract))
    
    rv$crush_df <- rv$crush_df_full |>
      filter(eth_desc == input$ethanol_contract)
  })
  
  # Recalculate crush_df_full when as-of date changes
  observeEvent(input$as_of_date, {
    req(rv$loaded, rv$ethanol_df, rv$corn_df)
    
    asof <- as.Date(input$as_of_date)
    print(paste("As-of date changed to:", asof))
    
    crush_df_full <- build_crush_df(rv$ethanol_df, rv$corn_df, asof) |> na.omit()
    rv$crush_df_full <- crush_df_full
    
    print(paste("Rebuilt crush_df_full with", nrow(crush_df_full), "rows"))
    
    # Update filtered data for current contract
    if (!is.null(input$ethanol_contract) && nzchar(input$ethanol_contract)) {
      if (!is.null(input$date_range)) {
        rv$crush_df <- crush_df_full |>
          filter(
            eth_desc == input$ethanol_contract,
            date >= input$date_range[1], 
            date <= input$date_range[2]
          )
      } else {
        rv$crush_df <- crush_df_full |>
          filter(eth_desc == input$ethanol_contract)
      }
    }
    
    # Update selector
    ethanol_choices <- crush_df_full |> distinct(eth_desc) |> arrange(eth_desc) |> pull(eth_desc)
    current_sel <- isolate(input$ethanol_contract)
    new_sel <- if (!is.null(current_sel) && current_sel %in% ethanol_choices) current_sel else ethanol_choices[1]
    updateSelectInput(session, "ethanol_contract", choices = ethanol_choices, selected = new_sel)
  })
  
  # Outputs
  output$liquidations_table <- render_gt({
    req(liq_df())
    print("Rendering GT table")
    build_liquidations_table(liq_df(), "Liquidations ON Corn Months", decimals = 3, dark_mode = input$dark_mode)
  })
  
  output$lifepath_plot <- renderPlotly({
    req(rv$loaded, rv$crush_df, input$ethanol_contract, nzchar(input$ethanol_contract))
    req(nrow(rv$crush_df) > 0)
    build_dual_axis_lifepath_plot(rv$crush_df, input$ethanol_contract, as.numeric(input$yield), dark_mode = input$dark_mode)
  })
  
  # Download handlers
  output$download_liquidations <- downloadHandler(
    filename = function() {
      paste0("liquidations_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(liq_df())
      write_csv(liq_df(), file)
    }
  )
  
  output$download_lifepath <- downloadHandler(
    filename = function() {
      paste0("lifepath_", gsub(" ", "_", input$ethanol_contract), "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$crush_df, input$ethanol_contract, nzchar(input$ethanol_contract))
      
      lifepath_df <- rv$crush_df |>
        mutate(
          trading_month = floor_date(date, "months"),
          yield_used = as.numeric(input$yield)
        ) |>
        group_by(corn_desc, eth_desc, trading_month) |>
        mutate(rolling_avg_corn = cummean(corn_close)) |>
        ungroup() |>
        mutate(
          crush_spread = eth_close - (rolling_avg_corn / 100 / yield_used),
          implied_spread = eth_close - (corn_close / 100 / yield_used),
          spread_used = if_else(expired, crush_spread, implied_spread)
        ) |>
        select(date, eth_year, eth_month, month_name, corn_year, corn_month,
               eth_desc, corn_desc, eth_close, corn_close, rolling_avg_corn,
               expired, crush_spread, implied_spread, spread_used, yield_used) |>
        arrange(date)
      
      write_csv(lifepath_df, file)
    }
  )
  
  observeEvent(input$load_data_off, {
    showNotification("OFF Corn Months not wired yet.", type = "message", duration = 3)
  })
}

shinyApp(ui, server)