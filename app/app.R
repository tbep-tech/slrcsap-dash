library(shiny)
library(bslib)
library(DT)
library(plotly)
library(slrcsap)
library(ggplot2)
library(tidyr)
library(dplyr)

# Define common NOAA gauge stations
gauge_options <- list(
  "St. Petersburg, FL" = 8726520,
  "Cedar Key, FL" = 8727520
)

# Define scenario options
scenario_options <- list(
  "NOAA Intermediate Low" = "IntLow",
  "NOAA Intermediate" = "Int", 
  "NOAA Intermediate High" = "IntHigh",
  "NOAA Low" = "Low",
  "NOAA High" = "High"
)

# UI
ui <- page_navbar(
  title = "Sea Level Rise Data Explorer",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#0066cc",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545"
  ),
  
  # Historical Data Tab
  nav_panel(
    title = "Historical Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 300,
        
        # Gauge selection
        selectInput("gauge", 
                    "Select NOAA Gauge:",
                    choices = gauge_options,
                    selected = 8726520),
        
        # Custom gauge input
        numericInput("custom_gauge",
                     "Or enter custom gauge ID:",
                     value = NULL,
                     min = 1000000,
                     max = 9999999),
        
        # Units selection
        radioButtons("units",
                     "Display Units:",
                     choices = list("Feet" = "ft", "Meters" = "m"),
                     selected = "ft"),
        
        # Action button to refresh data
        input_task_button("refresh", "Load Data", type = "primary"),
        
        br(),
        
        # Status message
        div(id = "status", style = "margin-top: 15px;")
      ),
      
      # Main content area for historical data
      card(
        card_header("Historical Sea Level Plot"),
        card_body(
          plotlyOutput("sealevel_plot", height = "450px")
        )
      ),
      
      br(),
      
      card(
        card_header("Historical Sea Level Data"),
        card_body(
          DT::dataTableOutput("sealevel_table")
        )
      )
    )
  ),
  
  # Projections Tab
  nav_panel(
    title = "Future Projections",
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 300,
        
        # Gauge selection
        selectInput("gauge2", 
                    "Select NOAA Gauge:",
                    choices = gauge_options,
                    selected = 8726520),
        
        # Custom gauge input
        numericInput("custom_gauge2",
                     "Or enter custom gauge ID:",
                     value = NULL,
                     min = 1000000,
                     max = 9999999),
        
        # Scenario selection
        checkboxGroupInput("scenarios",
                           "Select Scenarios:",
                           choices = scenario_options,
                           selected = c("IntLow", "Int", "IntHigh")),
        
        # Units selection
        radioButtons("units2",
                     "Display Units:",
                     choices = list("Feet" = "ft", "Meters" = "m"),
                     selected = "ft"),
        
        # Action button to refresh data
        input_task_button("refresh2", "Load Data", type = "primary"),
        
        br(),
        
        # Status message
        div(id = "status2", style = "margin-top: 15px;")
      ),
      
      # Main content area for projections
      card(
        card_header("Sea Level Rise Projections Plot"),
        card_body(
          plotlyOutput("scenario_plot", height = "450px")
        )
      ),
      
      br(),
      
      card(
        card_header("Sea Level Rise Projections Data"),
        card_body(
          DT::dataTableOutput("scenario_table")
        )
      )
    )
  ),
  
  # About Tab
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        card_header("About This App"),
        card_body(
          h4("Sea Level Rise Data Explorer"),
          p("This Shiny app provides an interactive interface to explore sea level data using the", 
            code("slrcsap"), "R package."),
          
          h5("Features:"),
          tags$ul(
            tags$li("View historical sea level data from NOAA tide gauges"),
            tags$li("Explore future sea level rise projections based on NOAA 2022 scenarios"),
            tags$li("Interactive plots and data tables"),
            tags$li("Support for multiple gauge stations"),
            tags$li("Customizable units (feet or meters)")
          ),
          
          h5("Data Sources:"),
          tags$ul(
            tags$li(tags$a("NOAA Tides and Currents", 
                           href = "https://tidesandcurrents.noaa.gov", 
                           target = "_blank")),
            tags$li(tags$a("Interagency Sea Level Rise Scenario Tool", 
                           href = "https://sealevel.nasa.gov/task-force-scenario-tool", 
                           target = "_blank"))
          ),
          
          h5("Package Information:"),
          p("Built with the", 
            tags$a("slrcsap R package", 
                   href = "https://tbep-tech.github.io/slrcsap/", 
                   target = "_blank"),
            "developed by the Tampa Bay Estuary Program.")
        )
      ),
      
      card(
        card_header("Quick Start"),
        card_body(
          h6("How to use:"),
          tags$ol(
            tags$li("Select a gauge station from the dropdown"),
            tags$li("Choose your preferred units"),
            tags$li("Click 'Load Data' to fetch information"),
            tags$li("Explore the plots and data tables"),
            tags$li("Switch between Historical and Projections tabs")
          ),
          
          br(),
          
          h6("Gauge Stations:"),
          p("The app includes two pre-configured Florida stations (St. Petersburg, Cedar Key), or you can enter any NOAA gauge ID.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    sealevel_data = NULL,
    scenario_data = NULL,
    current_gauge = NULL,
    current_gauge2 = NULL
  )
  
  # Determine which gauge to use for historical data
  selected_gauge <- reactive({
    if (!is.null(input$custom_gauge) && !is.na(input$custom_gauge)) {
      return(input$custom_gauge)
    } else {
      return(input$gauge)
    }
  })
  
  # Determine which gauge to use for projections
  selected_gauge2 <- reactive({
    if (!is.null(input$custom_gauge2) && !is.na(input$custom_gauge2)) {
      return(input$custom_gauge2)
    } else {
      return(input$gauge2)
    }
  })
  
  # Load historical data when refresh button is clicked
  observeEvent(input$refresh, {
    gauge_id <- selected_gauge()
    values$current_gauge <- gauge_id
    
    # Show loading status
    removeUI("#status > div")
    insertUI("#status", "beforeEnd",
             div(class = "alert alert-info", role = "alert",
                 icon("spinner", class = "fa-spin"), " Loading historical data..."))
    
    # Load sea level data
    tryCatch({
      values$sealevel_data <- get_sealevel(gauge = gauge_id)
      
      # Show success status
      removeUI("#status > div")
      insertUI("#status", "beforeEnd",
               div(class = "alert alert-success", role = "alert",
                   icon("check-circle"), " Historical data loaded successfully!"))
      
      # Auto-hide success message after 3 seconds
      shinyjs::delay(3000, removeUI("#status > div"))
      
    }, error = function(e) {
      values$sealevel_data <- NULL
      
      # Show error status
      removeUI("#status > div")
      insertUI("#status", "beforeEnd",
               div(class = "alert alert-danger", role = "alert",
                   icon("exclamation-triangle"), 
                   " Error loading historical data: ", e$message))
    })
  })
  
  # Load projection data when refresh button is clicked
  observeEvent(input$refresh2, {
    gauge_id <- selected_gauge2()
    values$current_gauge2 <- gauge_id
    
    # Show loading status
    removeUI("#status2 > div")
    insertUI("#status2", "beforeEnd",
             div(class = "alert alert-info", role = "alert",
                 icon("spinner", class = "fa-spin"), " Loading projection data..."))
    
    # Load scenario data (need to map NOAA gauge ID to PSMSL ID)
    tryCatch({
      # For demonstration, using a mapping for common gauges
      psmsl_id <- switch(as.character(gauge_id),
                         "8726520" = 520,  # St. Petersburg
                         "8727520" = 428,  # Cedar Key  
                         "8724580" = 188,  # Key West (approximate)
                         520) # Default to St. Petersburg
      
      if (length(input$scenarios) == 0) {
        stop("Please select at least one scenario")
      }
      
      values$scenario_data <- get_scenario(id = psmsl_id, scenario = input$scenarios)
      
      # Show success status
      removeUI("#status2 > div")
      insertUI("#status2", "beforeEnd",
               div(class = "alert alert-success", role = "alert",
                   icon("check-circle"), " Projection data loaded successfully!"))
      
      # Auto-hide success message after 3 seconds
      shinyjs::delay(3000, removeUI("#status2 > div"))
      
    }, error = function(e) {
      values$scenario_data <- NULL
      
      # Show error status
      removeUI("#status2 > div")
      insertUI("#status2", "beforeEnd",
               div(class = "alert alert-danger", role = "alert",
                   icon("exclamation-triangle"), 
                   " Error loading projection data: ", e$message))
    })
  })
  
  # Historical sea level plot
  output$sealevel_plot <- renderPlotly({
    if (is.null(values$sealevel_data)) {
      # Create a simple message plot for plotly
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Click 'Load Data' to view historical sea level data", 
                 size = 5, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    } else {
      tryCatch({
        # Use plotly = TRUE argument
        plot_sealevel(values$sealevel_data, units = input$units, plotly = TRUE) %>%
          layout(title = list(text = paste("Historical Sea Level Data - Gauge", values$current_gauge),
                              x = 0.5))
      }, error = function(e) {
        # Create error plot for plotly
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error creating plot:", e$message), 
                   size = 4, color = "red") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
      })
    }
  })
  
  # Historical sea level data table
  output$sealevel_table <- DT::renderDataTable({
    if (is.null(values$sealevel_data)) {
      return(data.frame(Message = "Click 'Load Data' to view historical sea level data"))
    }
    
    # Select columns based on units
    if (input$units == "ft") {
      display_data <- values$sealevel_data[, c("gauge", "Year", "Month", "date", "msl_ft")]
      names(display_data)[5] <- "MSL (ft)"
    } else {
      display_data <- values$sealevel_data[, c("gauge", "Year", "Month", "date", "msl_m")]
      names(display_data)[5] <- "MSL (m)"
    }
    
    DT::datatable(display_data, 
                  options = list(pageLength = 15, scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')),
                  rownames = FALSE,
                  class = 'table-striped table-hover')
  })
  
  # Scenario plot
  output$scenario_plot <- renderPlotly({
    if (is.null(values$scenario_data)) {
      # Create a simple message plot for plotly
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Click 'Load Data' to view sea level rise projections", 
                 size = 5, color = "gray50") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    } else {
      tryCatch({
        # Use plotly = TRUE argument
        plot_scenario(values$scenario_data, units = input$units2, plotly = TRUE) %>%
          layout(title = list(text = paste("Sea Level Rise Projections - Station ID", unique(values$scenario_data$id)),
                              x = 0.5),
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      }, error = function(e) {
        # Create error plot for plotly
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error creating plot:", e$message), 
                   size = 4, color = "red") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
      })
    }
  })
  
  # Scenario data table
  output$scenario_table <- DT::renderDataTable({
    if (is.null(values$scenario_data)) {
      return(data.frame(Message = "Click 'Load Data' to view sea level rise projections"))
    }
    
    # Select columns based on units
    if (input$units2 == "ft") {
      display_data <- values$scenario_data[, c("id", "scenario", "year", "slr_ft")]
      value_col <- "slr_ft"
      unit_suffix <- " (ft)"
    } else {
      display_data <- values$scenario_data[, c("id", "scenario", "year", "slr_m")]
      value_col <- "slr_m"
      unit_suffix <- " (m)"
    }
    
    # Convert to wide format using tidyr - first pivot then rename
    wide_data <- display_data %>%
      tidyr::pivot_wider(
        names_from = scenario,
        values_from = all_of(value_col)
      ) %>%
      dplyr::rename(
        "Station ID" = id,
        "Year" = year
      ) %>%
      dplyr::arrange(Year)
    
    # Add unit suffix to scenario column names
    scenario_cols <- names(wide_data)[!names(wide_data) %in% c("Station ID", "Year")]
    new_names <- paste0(scenario_cols, unit_suffix)
    names(wide_data)[names(wide_data) %in% scenario_cols] <- new_names
    
    # Convert to data frame for DT
    wide_data <- as.data.frame(wide_data)
    
    # Get final scenario column names for formatting
    final_scenario_cols <- names(wide_data)[!names(wide_data) %in% c("Station ID", "Year")]
    
    DT::datatable(wide_data, 
                  options = list(pageLength = 15, scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')),
                  rownames = FALSE,
                  class = 'table-striped table-hover') %>%
      DT::formatRound(columns = final_scenario_cols, digits = 3)
  })
}

# Run the app
shinyApp(ui = ui, server = server)