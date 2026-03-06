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
  
  # ---- Liquidations tab ----
  nav_panel(
    title = "Liquidations",
    icon  = icon("table"),
    page_sidebar(
      sidebar = sidebar(
        open  = "desktop",
        width = 260,
        title = "Controls",
        
        dateInput("max_date", "Max Date (query cutoff)", value = Sys.Date()),
        actionButton("load_data",     "Load Data",     class = "btn-primary", width = "100%"),
        actionButton("force_refresh", "Force Refresh", class = "btn-warning",
                     width = "100%", style = "margin-top:4px;"),
        uiOutput("last_fetch_info"),
        
        hr(),
        
        uiOutput("as_of_date_ui"),
        fluidRow(
          column(6, numericInput("min_year", "Min Year", value = 2007,
                                 min = 2007, max = 2099, step = 1)),
          column(6, selectInput("max_year", "Max Year",
                                choices  = c(as.integer(format(Sys.Date(), "%Y")),
                                             as.integer(format(Sys.Date(), "%Y")) + 1L),
                                selected = as.integer(format(Sys.Date(), "%Y"))))
        ),
        numericInput("yield", "Yield (gal/bu)", value = 2.9, min = 0.1, step = 0.01),
        actionButton("apply_changes", "Apply", class = "btn-success", width = "100%"),
        
        hr(),
        uiOutput("status"),
        hr(),
        h5("Export Data"),
        downloadButton("dl_on",      "Download ON Months CSV",   class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_off",     "Download OFF Months CSV",  class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_ethanol", "Download Ethanol Raw CSV", class = "btn-sm", style = "width:100%; margin-bottom:6px;"),
        downloadButton("dl_corn",    "Download Corn Raw CSV",    class = "btn-sm", style = "width:100%;")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(h5("Liquidations ON Corn Months", style = "margin:0;")),
          card_body(style = "overflow-y:auto; overflow-x:auto; padding:0.5rem;",
                    gt_output("table_on"))
        ),
        card(
          full_screen = TRUE,
          card_header(div(
            h5("ON Corn Months - Crush Spread Over Time", style = "margin:0 0 4px 0;"),
            uiOutput("slider_on_ui")
          )),
          card_body(style = "padding:0.5rem;",
                    plotlyOutput("plot_on", height = "400px"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(h5("Liquidations OFF Corn Months", style = "margin:0;")),
          card_body(style = "overflow-y:auto; overflow-x:auto; padding:0.5rem;",
                    gt_output("table_off"))
        ),
        card(
          full_screen = TRUE,
          card_header(div(
            h5("OFF Corn Months - Crush Spread Over Time", style = "margin:0 0 4px 0;"),
            uiOutput("slider_off_ui")
          )),
          card_body(style = "padding:0.5rem;",
                    plotlyOutput("plot_off", height = "400px"))
        )
      )
    )
  ),
  
  # ---- How To Use tab ----
  nav_panel(
    title = "How To Use",
    icon  = icon("circle-info"),
    fluidRow(
      column(
        width = 8, offset = 2,
        style = "padding-top:2rem;",
        
        card(
          card_header(h5("Overview", style = "margin:0;")),
          card_body(
            p("This app calculates ethanol crush spreads across ON and OFF corn contract months,
               using daily price data pulled from the MarketView API. Each table covers a different
               set of corn contract pairings. Expired months are valued using the full-month average
               of daily closes. Active forward months use the current implied spread from the most
               recent available closing price."),
            p("Two tables are produced side by side with matching time series charts: ON Corn Months
               covers ethanol contracts paired with an active corn futures month, and OFF Corn Months
               covers all remaining ethanol months paired against the nearest available corn contract.
               Each chart shows the historical evolution of the crush spread for every contract month
               in the table.")
          )
        ),
        
        card(
          card_header(h5("Loading Data", style = "margin:0;")),
          card_body(
            tags$dl(
              tags$dt("Max Date (query cutoff)"),
              tags$dd("Sets the upper boundary for the data pull from the MarketView API. All price
                       history is fetched up to and including this date. Defaults to today."),
              tags$dt("Load Data"),
              tags$dd("Triggers the data pipeline. On the first load of the day, the app queries the
                       MarketView API for both ethanol (GCU) and corn (ZC) contracts and saves the
                       results to a local cache folder. On all subsequent clicks during the same calendar
                       day, data is read from that cache rather than making another API call. The time
                       of the last actual API fetch is always shown below this button."),
              tags$dt("Force Refresh"),
              tags$dd("Skips the cache check entirely and queries the API regardless of whether data
                       was already fetched today. Use this when you know updated prices are available
                       and need the latest data within the same day.")
            )
          )
        ),
        
        card(
          card_header(h5("Adjusting the View", style = "margin:0;")),
          card_body(
            tags$dl(
              tags$dt("As-of Date (expiration logic)"),
              tags$dd("Controls which contract months are treated as expired versus forward. A contract
                       month is considered expired once the as-of date reaches or passes that contract's
                       last actual trading day. After data is loaded, the dropdown is populated with
                       only real trading days from the dataset, preventing selection of weekends or
                       market holidays."),
              tags$dt("Min Year / Max Year"),
              tags$dd("Filters the output tables to show only the selected range of contract years."),
              tags$dt("Yield (gal/bu)"),
              tags$dd("The ethanol yield in gallons per bushel. Used to convert corn prices from
                       cents per bushel to a per-gallon equivalent when calculating the crush spread.
                       The default value is 2.9."),
              tags$dt("Apply"),
              tags$dd("Recalculates the crush spread tables and charts using the current As-of Date,
                       Yield, and Year range settings. Does not re-fetch data from the API.")
            )
          )
        ),
        
        card(
          card_header(h5("Crush Spread Formula", style = "margin:0;")),
          card_body(
            p("The crush spread for each contract month is:"),
            tags$pre(
              style = "background:var(--bs-light); padding:0.75rem; border-radius:4px; font-size:0.85rem;",
              "Crush Spread  =  Ethanol Price  -  ( Corn Price / 100 / Yield )"
            ),
            tags$ul(
              tags$li(strong("Ethanol Price"), " - GCU contract closing price in dollars per gallon"),
              tags$li(strong("Corn Price"), " - ZC contract closing price in cents per bushel"),
              tags$li(strong("Yield"), " - conversion factor in gallons per bushel (default 2.9)")
            ),
            p("For expired months, both the ethanol and corn prices used are the arithmetic average
               of all daily closing prices within that contract month. For active forward months, the
               most recent available closing price is used for both legs.")
          )
        ),
        
        card(
          card_header(h5("ON vs OFF Corn Months", style = "margin:0;")),
          card_body(
            p("Ethanol contracts are split into two groups based on which corn futures month they
               are conventionally priced against:"),
            tags$table(
              class = "table table-sm table-bordered",
              style = "font-size:0.85rem;",
              tags$thead(tags$tr(tags$th("Ethanol Month"), tags$th("Paired Corn Contract"))),
              tags$tbody(
                tags$tr(tags$td("January (F), February (G)"),                tags$td("March corn (H)")),
                tags$tr(tags$td("March (H), April (J)"),                     tags$td("May corn (K)")),
                tags$tr(tags$td("May (K), June (M)"),                        tags$td("July corn (N)")),
                tags$tr(tags$td("July (N), August (Q)"),                     tags$td("September corn (U)")),
                tags$tr(tags$td("September (U), October (V), November (X)"), tags$td("December corn (Z)")),
                tags$tr(tags$td("December (Z)"),                             tags$td("March corn, following year (H)"))
              )
            ),
            p("ON Corn Months are ethanol months that align directly with an active corn futures month.
               OFF Corn Months are all remaining ethanol months, priced against the nearest corn contract.")
          )
        ),
        
        card(
          card_header(h5("Local Cache", style = "margin:0;")),
          card_body(
            p("To avoid unnecessary API calls, raw price data is stored locally in a folder named ",
              tags$code("current-day-crush-data"), " in the app working directory. This folder is
               created automatically on first use and contains four files:"),
            tags$ul(
              tags$li(tags$code("ethanol_df.rds"), " - ethanol contract daily price history"),
              tags$li(tags$code("corn_df.rds"), " - corn contract daily price history"),
              tags$li(tags$code("cache_date.txt"), " - the calendar date the cache was last written"),
              tags$li(tags$code("fetch_timestamp.txt"), " - the exact date and time of the last API call")
            ),
            p("The cache is automatically invalidated and rebuilt on the first Load Data click of each
               new calendar day. The last fetch timestamp shown below the Load Data button allows you
               to confirm how current the data is at a glance.")
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
      .btn-success { background-color:#18BC9C !important; border-color:#18BC9C !important; font-weight:500; }
      .btn-success:hover { background-color:#15a589 !important; border-color:#15a589 !important; }
      .btn-warning { background-color:#E67E22 !important; border-color:#E67E22 !important; font-weight:500; color:#fff !important; }
      .btn-warning:hover { background-color:#CA6F1E !important; border-color:#CA6F1E !important; color:#fff !important; }
      .card { border:1px solid var(--bs-border-color); box-shadow:0 2px 4px rgba(0,0,0,0.05); margin-bottom:4px !important; }
      .card-header { background-color:var(--bs-light); border-bottom:1px solid var(--bs-border-color); font-weight:600; padding:0.4rem; }
      [data-bs-theme='dark'] .card-header { background-color:var(--bs-dark); }
      .form-check-input:checked { background-color:#18BC9C; border-color:#18BC9C; }
      .status-text { font-size:0.85rem; padding:0.5rem; background-color:var(--bs-light); border-radius:4px; margin-top:0.5rem; }
      [data-bs-theme='dark'] .status-text { background-color:var(--bs-dark); }
      .last-fetch-info { font-size:0.78rem; color:#7F8C8D; padding:0.25rem 0; margin-top:4px; line-height:1.5; }
      dt { font-weight:600; margin-top:0.75rem; }
      dd { margin-left:1rem; margin-bottom:0.25rem; color:var(--bs-secondary-color); }
    "))),
    shinyjs::hidden(div(
      id    = "loading_overlay",
      style = "position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.6);z-index:9999;display:flex;align-items:center;justify-content:center;",
      div(
        style = "background:#000;color:#fff;padding:30px 40px;border-radius:8px;border:1px solid #333;text-align:left;font-family:'Courier New',monospace;min-width:400px;",
        tags$div(
          style = "display:flex;align-items:center;margin-bottom:18px;",
          tags$div(class = "spinner-border text-light", role = "status",
                   style = "width:1.4rem;height:1.4rem;margin-right:12px;flex-shrink:0;"),
          tags$div("Loading data...", style = "font-size:13px;font-weight:600;letter-spacing:0.4px;")
        ),
        tags$div(id = "loading_message",
                 style = "font-size:12px;color:#aaa;min-height:18px;line-height:1.6;",
                 "Initializing...")
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
    loaded             = FALSE,
    ethanol_df         = NULL,
    corn_df            = NULL,
    prices_full        = NULL,
    last_load          = NULL,
    from_cache         = FALSE,
    err                = NULL,
    applied_as_of_date = Sys.Date(),
    applied_yield      = 2.9,
    applied_min_year   = 2007,
    applied_max_year   = as.integer(format(Sys.Date(), "%Y"))
  )
  
  # Increments at the end of every load to force last_fetch_info to re-read disk
  load_trigger <- reactiveVal(0L)
  
  # ---- Cache helpers ----
  cache_dir       <- "current-day-crush-data"
  cache_date_file <- file.path(cache_dir, "cache_date.txt")
  cache_ethanol   <- file.path(cache_dir, "ethanol_df.rds")
  cache_corn      <- file.path(cache_dir, "corn_df.rds")
  cache_fetch_ts  <- file.path(cache_dir, "fetch_timestamp.txt")
  
  cache_is_fresh <- function() {
    if (!file.exists(cache_date_file)) return(FALSE)
    cached_date <- tryCatch(readLines(cache_date_file, warn = FALSE)[1], error = function(e) "")
    cached_date == as.character(Sys.Date())
  }
  
  wipe_cache <- function() {
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir, showWarnings = FALSE)
  }
  
  save_cache <- function(ethanol_df, corn_df) {
    dir.create(cache_dir, showWarnings = FALSE)
    saveRDS(ethanol_df, cache_ethanol)
    saveRDS(corn_df,    cache_corn)
    writeLines(as.character(Sys.Date()),                cache_date_file)
    writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), cache_fetch_ts)
  }
  
  read_last_fetch_ts <- function() {
    if (!file.exists(cache_fetch_ts)) return(NULL)
    tryCatch(readLines(cache_fetch_ts, warn = FALSE)[1], error = function(e) NULL)
  }
  
  # ---- Last fetch info (shown below Load Data buttons) ----
  output$last_fetch_info <- renderUI({
    load_trigger()  # invalidates every time a load completes, guaranteed after disk writes
    ts <- read_last_fetch_ts()
    if (is.null(ts)) {
      div(class = "last-fetch-info", "No cache found. Click Load Data to fetch.")
    } else {
      stale <- !cache_is_fresh()
      div(class = "last-fetch-info",
          paste0("Last API fetch: ", ts),
          if (stale) tags$div(style = "color:#E74C3C; margin-top:2px;",
                              "Cache is from a prior day. Load Data will refresh.")
      )
    }
  })
  
  # ---- As-of date picker: populated from actual trading days once loaded ----
  output$as_of_date_ui <- renderUI({
    if (isTRUE(rv$loaded) && !is.null(rv$ethanol_df)) {
      trading_days <- as.character(sort(unique(rv$ethanol_df$DATE_ID), decreasing = TRUE))
      selectInput("as_of_date", "As-of Date (expiration logic)",
                  choices = trading_days, selected = trading_days[1])
    } else {
      dateInput("as_of_date", "As-of Date (expiration logic)", value = Sys.Date())
    }
  })
  
  # ---- Status panel ----
  output$status <- renderUI({
    if (isTRUE(rv$loaded) && is.null(rv$err)) {
      div(class = "status-text",
          tags$div(style = "font-size:12px;", paste0("Loaded: ",    rv$last_load)),
          tags$div(style = "font-size:12px;", paste0("Contracts: ", nrow(rv$prices_full))),
          tags$div(style = "font-size:12px; color:#7F8C8D; margin-top:2px;",
                   if (isTRUE(rv$from_cache)) "Served from today's cache" else "Fetched live from API")
      )
    } else if (!is.null(rv$err)) {
      div(class = "status-text", style = "color:#E74C3C; border-left:3px solid #E74C3C;",
          tags$div(style = "font-weight:600;", "Error:"),
          tags$div(style = "font-size:12px;", rv$err))
    } else {
      div(class = "status-text", style = "color:#7F8C8D;",
          tags$div(style = "font-size:12px;", "Idle. Click Load Data to fetch."))
    }
  })
  
  # ---- Shared load/cache logic ----
  do_load <- function(force = FALSE) {
    rv$err <- NULL
    shinyjs::show("loading_overlay")
    
    tryCatch({
      max_d  <- isolate(as.Date(input$max_date))
      as_of_d <- isolate(as.Date(input$as_of_date))
      yld    <- isolate(as.numeric(input$yield))
      min_yr <- isolate(as.integer(input$min_year))
      max_yr <- isolate(as.integer(input$max_year))
      
      if (!force && cache_is_fresh()) {
        shinyjs::html("loading_message", "Cache is current. Loading ethanol data from disk...")
        ethanol_df <- readRDS(cache_ethanol)
        shinyjs::html("loading_message", "Loading corn data from disk...")
        corn_df    <- readRDS(cache_corn)
        rv$from_cache <- TRUE
        rv$last_load  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      } else {
        shinyjs::html("loading_message", "Cache is stale or absent. Clearing old cache...")
        wipe_cache()
        
        shinyjs::html("loading_message",
                      paste0("Querying MarketView API for ethanol contracts (GCU ",
                             min_yr, " - ", max_yr, ")..."))
        ethanol_df <- build_marketview_daily_forward_data_on_corn_months(
          base_symbols   = "GCU",
          calendar_years = min_yr:max_yr,
          month_codes    = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
          min_date       = "2000-01-01",
          max_date       = max_d,
          as_of_date     = as_of_d
        )
        
        shinyjs::html("loading_message",
                      paste0("Querying MarketView API for corn contracts (ZC ",
                             min_yr, " - ", max_yr, ")..."))
        corn_df <- build_marketview_daily_forward_data_on_corn_months(
          base_symbols   = "ZC",
          calendar_years = min_yr:max_yr,
          month_codes    = c("H","K","N","U","Z"),
          min_date       = "2000-01-01",
          max_date       = max_d,
          as_of_date     = as_of_d
        )
        
        shinyjs::html("loading_message", "Saving results to local cache...")
        save_cache(ethanol_df, corn_df)
        rv$from_cache <- FALSE
        rv$last_load  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      }
      
      shinyjs::html("loading_message", "Applying expiry logic...")
      ethanol_df <- ethanol_df |>
        group_by(DESCRIPTION) |>
        mutate(EXPIRED = if_else(as.Date(as_of_d) >= max(DATE_ID), 1, 0)) |>
        ungroup()
      corn_df <- corn_df |>
        group_by(DESCRIPTION) |>
        mutate(EXPIRED = if_else(as.Date(as_of_d) >= max(DATE_ID), 1, 0)) |>
        ungroup()
      
      shinyjs::html("loading_message", "Building crush spreads...")
      rv$ethanol_df  <- ethanol_df
      rv$corn_df     <- corn_df
      rv$prices_full <- build_prices_full(ethanol_df, corn_df, as_of_d, yld)
      rv$loaded      <- TRUE
      
      rv$applied_as_of_date <- as_of_d
      rv$applied_yield      <- yld
      rv$applied_min_year   <- min_yr
      rv$applied_max_year   <- max_yr
      
      shinyjs::html("loading_message", "Done.")
      load_trigger(load_trigger() + 1L)
      shinyjs::hide("loading_overlay")
      
    }, error = function(e) {
      rv$err <- as.character(e$message)
      shinyjs::hide("loading_overlay")
    })
  }
  
  observeEvent(input$load_data,     { do_load(force = FALSE) })
  observeEvent(input$force_refresh, { do_load(force = TRUE)  })
  
  # ---- Apply changes (recalculate without re-fetching) ----
  observeEvent(input$apply_changes, {
    req(rv$loaded, rv$ethanol_df, rv$corn_df)
    as_of_d <- as.Date(input$as_of_date)
    yld     <- as.numeric(input$yield)
    min_yr  <- as.integer(input$min_year)
    max_yr  <- as.integer(input$max_year)
    ethanol_restamped <- rv$ethanol_df |>
      group_by(DESCRIPTION) |>
      mutate(EXPIRED = if_else(as.Date(as_of_d) >= max(DATE_ID), 1, 0)) |>
      ungroup()
    corn_restamped <- rv$corn_df |>
      group_by(DESCRIPTION) |>
      mutate(EXPIRED = if_else(as.Date(as_of_d) >= max(DATE_ID), 1, 0)) |>
      ungroup()
    rv$prices_full        <- build_prices_full(ethanol_restamped, corn_restamped, as_of_d, yld)
    rv$applied_as_of_date <- as_of_d
    rv$applied_yield      <- yld
    rv$applied_min_year   <- min_yr
    rv$applied_max_year   <- max_yr
  })
  
  # ---- GT tables ----
  output$table_on <- render_gt({
    req(wide_on())
    build_crush_table(wide_on(),
                      title       = "Liquidations ON Corn Months",
                      subtitle    = paste0("Shaded cells indicate forward values (contracts expiring after ",
                                           format(rv$applied_as_of_date, "%m/%d/%Y"), ")"),
                      decimals    = 3,
                      as_of_date  = rv$applied_as_of_date,
                      max_year    = rv$applied_max_year,
                      ethanol_df  = rv$ethanol_df,
                      corn_df     = rv$corn_df,
                      yield       = rv$applied_yield,
                      spread_type = "ON",
                      dark_mode   = input$dark_mode)
  })
  
  output$table_off <- render_gt({
    req(wide_off())
    build_crush_table(wide_off(),
                      title       = "Liquidations OFF Corn Months",
                      subtitle    = paste0("Shaded cells indicate forward values (contracts expiring after ",
                                           format(rv$applied_as_of_date, "%m/%d/%Y"), ")"),
                      decimals    = 3,
                      as_of_date  = rv$applied_as_of_date,
                      max_year    = rv$applied_max_year,
                      ethanol_df  = rv$ethanol_df,
                      corn_df     = rv$corn_df,
                      yield       = rv$applied_yield,
                      spread_type = "OFF",
                      dark_mode   = input$dark_mode)
  })
  
  # ---- Wide dfs ----
  wide_on  <- reactive({ req(rv$loaded, rv$prices_full); shape_for_table(rv$prices_full, "CRUSH_SPREAD_ON_MONTH")  })
  wide_off <- reactive({ req(rv$loaded, rv$prices_full); shape_for_table(rv$prices_full, "CRUSH_SPREAD_OFF_MONTH") })
  
  # ---- Slider helpers ----
  ts_date_range_from_wide <- function(wide_df) {
    stat_labels <- c("Mean","P-10","P-25","P-50","P-75","P-90",
                     "__gap1__","__gap2__","__gap3__",
                     "Week Ago Value","Week Ago Percentile","Delta Week",
                     "Month Ago Value","Month Ago Percentile","Delta Month")
    month_to_num <- c(January=1,February=2,March=3,April=4,May=5,June=6,
                      July=7,August=8,September=9,October=10,November=11,December=12)
    month_cols <- names(wide_df)[names(wide_df) %in% names(month_to_num)]
    dates <- wide_df |>
      filter(!(YEAR %in% stat_labels)) |>
      mutate(YEAR = as.integer(YEAR)) |>
      select(YEAR, all_of(month_cols)) |>
      pivot_longer(all_of(month_cols), names_to = "MONTH_NAME", values_to = "VALUE") |>
      filter(!is.na(VALUE)) |>
      mutate(CONTRACT_DATE = as.Date(paste(YEAR, month_to_num[MONTH_NAME], "01", sep = "-"))) |>
      pull(CONTRACT_DATE)
    list(min = min(dates), max = max(dates))
  }
  
  output$slider_on_ui <- renderUI({
    req(wide_on()); r <- ts_date_range_from_wide(wide_on())
    sliderInput("date_range_on",  label = NULL, min = r$min, max = r$max,
                value = c(r$min, r$max), timeFormat = "%b %Y", width = "100%")
  })
  output$slider_off_ui <- renderUI({
    req(wide_off()); r <- ts_date_range_from_wide(wide_off())
    sliderInput("date_range_off", label = NULL, min = r$min, max = r$max,
                value = c(r$min, r$max), timeFormat = "%b %Y", width = "100%")
  })
  
  # ---- Time series plots ----
  output$plot_on <- renderPlotly({
    req(wide_on(), input$date_range_on)
    build_crush_ts_plot(wide_on(),  title = "ON Corn Months - Crush Spread",
                        date_min = input$date_range_on[1],  date_max = input$date_range_on[2],
                        dark_mode = input$dark_mode)
  })
  output$plot_off <- renderPlotly({
    req(wide_off(), input$date_range_off)
    build_crush_ts_plot(wide_off(), title = "OFF Corn Months - Crush Spread",
                        date_min = input$date_range_off[1], date_max = input$date_range_off[2],
                        dark_mode = input$dark_mode)
  })
  
  # ---- Downloads ----
  output$dl_on <- downloadHandler(
    filename = function() paste0("liquidations_on_",  format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(f) { req(rv$prices_full); shape_for_table(rv$prices_full, "CRUSH_SPREAD_ON_MONTH")  |> write_csv(f) }
  )
  output$dl_off <- downloadHandler(
    filename = function() paste0("liquidations_off_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(f) { req(rv$prices_full); shape_for_table(rv$prices_full, "CRUSH_SPREAD_OFF_MONTH") |> write_csv(f) }
  )
  output$dl_ethanol <- downloadHandler(
    filename = function() paste0("ethanol_raw_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(f) { req(rv$ethanol_df); write_csv(rv$ethanol_df, f) }
  )
  output$dl_corn <- downloadHandler(
    filename = function() paste0("corn_raw_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(f) { req(rv$corn_df); write_csv(rv$corn_df, f) }
  )
}

shinyApp(ui, server)