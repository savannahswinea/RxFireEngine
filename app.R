library(shiny)
library(httr2)
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(leaflet)

# ---- Function to get 3-day weather ----
# Inspired by https://3mw.albert-rapp.de/p/weather-api
get_forecast <- function(lat, lon) {
  # Get point metadata including grid data URL
  point_meta <- request("https://api.weather.gov") |>
    req_url_path_append("points", paste0(lat, ",", lon)) |>
    req_perform() |>
    resp_body_json()
  
  grid_url <- point_meta$properties$forecastGridData
  
  grid_data <- request(grid_url) |>
    req_perform() |>
    resp_body_json() |>
    pluck("properties")
  
  extract_quant_values <- function(var, label) {
    tibble(
      start_time = map_chr(var$values, "validTime"),
      value = map_dbl(var$values, "value"),
      variable = label
    ) |>
      separate(start_time, into = c("start", "duration"), sep = "/") |>
      mutate(
        start = ymd_hms(start),
        date = as_date(start)
      )
  }
  
  # Extract variables of interest
  temp_max <- extract_quant_values(grid_data$maxTemperature, "max_temp") %>%
    mutate(value = value * 9/5 + 32)   # Celsius -> Fahrenheit
  temp_min <- extract_quant_values(grid_data$minTemperature, "min_temp") %>%
    mutate(value = value * 9/5 + 32)   # Celsius -> Fahrenheit
  wind_speed <- extract_quant_values(grid_data$windSpeed, "wind_speed")
  rh_min <- extract_quant_values(grid_data$minRelativeHumidity, "min_rh")
  precip_amt <- extract_quant_values(grid_data$quantitativePrecipitation, "precip_amt")
  
  # Combine and summarize to daily average values for next 3 days
  forecast_all <- bind_rows(temp_max, temp_min, wind_speed, rh_min, precip_amt) |>
    filter(date <= Sys.Date() + 3) |>
    group_by(date, variable) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = variable, values_from = value)
  
  forecast_all
}


# ---- Function to generate utility function graphs ----
double_logistic <- function(t, rise_mid, fall_mid, rise_slope = 0.1, fall_slope = 0.5,
                            flat_length = 0.025, flat_slope = 0, flat_intercept = 1 - 0.025) {
  l1 <- 1 + exp((rise_mid - t) / rise_slope)
  l2 <- 1 + exp((fall_mid - t) / fall_slope)
  y <- flat_length + (flat_intercept - flat_slope * t) * (1 / l1 - 1 / l2)
  return(y)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("RxFire Engine Prototype"),
  
  tabsetPanel(
    tabPanel("Welcome!",
             fluidRow(
               column(8, offset = 2,
                      tags$div(
                        id = "welcome-text",
                        tags$h4("What You Need to Know"),
                        tags$p("This app helps fire managers make prescribed burn decisions using short-term weather forecasts, historical weather information, and habitat rotation intervals."),
                        tags$p("Our prototype focuses on the Eglin Air Force Base, located in Florida. We developed a tool that is able to recommend a burning schedule based on weather conditions and the expected benefit associated with how long ago units had been burned. This website demonstrates how decision-makers could interact with this tool."),
                        tags$h4("Data Needs"),
                        tags$ul(
                          tags$li("This app utilizes information with the following names and descriptions:"),
                          tags$ul(
                            tags$li("Unit: a unique name for each unit"),
                            tags$li("Habitat: a habitat classification for that unit"),
                            tags$li("YearLastBurned: the year a unit was last burned"),
                            tags$li("Latitude, Longitude for each unit")
                          ),
                        ),
                        tags$h4("More Information and Contact"),
                        tags$p("The model developed to recommend burn schedules is described in detail in:"),
                        tags$p("Majumder, R., Terando, A. J., Hiers, J. K., Collazo, J. A., & Reich, B. J. (2025). A spatiotemporal optimization engine for prescribed burning in the Southeast US. Ecological Informatics, 85. ",
                        tags$a(href = "https://doi.org/10.1016/j.ecoinf.2024.102956", target = "_blank", "https://doi.org/10.1016/j.ecoinf.2024.102956")),
                        tags$p(
                          "For questions or feedback, please contact Savannah Swinea at",
                          tags$a(href = "mailto:sswinea@ncsu.edu", "sswinea@ncsu.edu")
                        )
               )
             )
    )
    ),
    
    tabPanel("Weather Forecast",
             sidebarLayout(
               sidebarPanel(
                 h4("Instructions"),
                 p("Click the button below to fetch a 3-day weather forecast for five burn units in the Eglin Air Force Base."),
                 actionButton("run_forecast", "Run Forecast")
               ),
               mainPanel(
                 uiOutput("forecast_bullets"),
                 leafletOutput("unit_map", height = 400)
               )
             )
    ),
    
    tabPanel("Habitat Rotation Intervals",
             sidebarLayout(
               sidebarPanel(
                 h4("Instructions"),
                 p("Adjust the minimum and maximum rotation intervals (in years) for each habitat below. Then click 'Evaluate Units' to receive guidance on which units are ready to burn."),
                 br(),
                 uiOutput("threshold_inputs"),
                 actionButton("evaluate", "Evaluate Units")
               ),
               
               mainPanel(
                 h3("Which units are ready to burn?"),
                 uiOutput("burn_feedback"),
                 br(),
                 h3("Utility Curves"),
                 uiOutput("burn_graphs")
               )
             )
    ),
    
    tabPanel("Burn Optimization",
             sidebarLayout(
               sidebarPanel(
                 h4("Instructions"),
                 p("Now that you have set the ideal rotation intervals for each habitat, click 'Generate Burn Schedule' to find out which units would be optimal to burn in the next three days."),
                 br(),
                 actionButton("burn_schedule", "Generate Burn Schedule")
               ),
               
             mainPanel(
               h3("Optimal Burn Schedule"),
               tableOutput("burn_table")
             )
    )
  )
)
)

# ---- Server ----
server <- function(input, output, session) {
  shared <- reactiveValues(y = NULL, habitats = NULL, forecast = NULL)
  
  # Read eglin2021.csv once, normalize lat/lon
  get_points_from_file <- reactive({
    app_dir <- tryCatch(
      dirname(normalizePath(sys.frames()[[1]]$ofile)),
      error = function(e) getwd()
    )
    
    csv_path <- file.path(app_dir, "eglin2021.csv")
    
    validate(
      need(file.exists(csv_path), paste("CSV not found at:", csv_path))
    )
    
    df <- read.csv(csv_path)
    
    df <- df |>
      dplyr::rename(Latitude = lat, Longitude = lon) |>
      dplyr::mutate(SDSFEATURENAME = as.character(SDSFEATURENAME))
    
    df <- dplyr::sample_n(df, 5)
    
    df
  })
  
  # Run forecast on each point incrementally
  observeEvent(input$run_forecast, {
    points <- get_points_from_file()
    n <- nrow(points)
    
    # Initialize forecast column as list
    points$forecast <- vector("list", n)
    shared$forecast <- points  # store in reactiveValues
    
    withProgress(message = "Running forecast...", value = 0, {
      for (i in seq_len(n)) {
        incProgress(i/n, detail = paste("Processing unit", i, "of", n, "-", points$SDSFEATURENAME[i]))
        
        fc <- tryCatch({
          get_forecast(points$Latitude[i], points$Longitude[i])
        }, error = function(e) {
          tibble(
            start = NA,
            date = NA,
            max_temp = NA,
            min_temp = NA,
            wind_speed = NA,
            min_rh = NA,
            precip_amt = NA
          )
        })
        
        shared$forecast$forecast[[i]] <- fc
        
        # Optional: small pause to ensure progress bar updates
        Sys.sleep(0.05)
      }
      
      # Unnest all forecasts
      shared$forecast <- shared$forecast |> tidyr::unnest(cols = forecast)
      setProgress(1, detail = "Forecast complete!")
    })
  })
  
  # Map units
  output$unit_map <- renderLeaflet({
    df <- shared$forecast
    req(df)
    
    leaflet(df) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        label = ~SDSFEATURENAME, radius = 5,
        fillOpacity = 0.8, color = "blue"
      )
  })
  
  # Output max temp bullets
  output$forecast_bullets <- renderUI({
    df <- shared$forecast
    req(df)
    
    df_max <- df |>
      group_by(SDSFEATURENAME) |>
      filter(max_temp == max(max_temp, na.rm = TRUE)) |>
      slice_head(n = 1) |>
      ungroup()
    
    bullets <- df_max |>
      mutate(msg = paste0(
        "<li><strong>", SDSFEATURENAME, "</strong> will experience a maximum temperature of <strong>", 
        round(max_temp, 1), "\u00B0F</strong> on <strong>", format(date, "%b %d"), "</strong> in the next three days.</li>"
      )) |>
      pull(msg) |>
      unname() |>
      paste(collapse = "\n")
    
    HTML(paste0("<ul>", bullets, "</ul>"))
  })
  
  # Reactive: Get unique habitats
  unique_habitats <- reactive({
    req(get_points_from_file())
    unique(get_points_from_file()$Habitat)
  })
  
  # Dynamic UI: min/max inputs for each Habitat
  output$threshold_inputs <- renderUI({
    req(unique_habitats())
    lapply(unique_habitats(), function(hab) {
      safe_id <- make.names(hab)
      tagList(
        h4(hab),
        numericInput(paste0("min_", safe_id), "Min Years Since Burn", value = 1),
        numericInput(paste0("max_", safe_id), "Max Years Since Burn", value = 5)
      )
    })
  })
  
  # Reactive: Generate feedback after evaluation
  output$burn_feedback <- renderUI({
    req(input$evaluate)
    isolate({
      df <- get_points_from_file()
      req(all(c("SDSFEATURENAME", "Habitat", "YLB") %in% names(df)))
      
      habs <- unique_habitats()
      
      thresholds <- lapply(habs, function(hab) {
        safe_id <- make.names(hab)
        data.frame(
          Habitat = hab,
          Min = input[[paste0("min_", safe_id)]],
          Max = input[[paste0("max_", safe_id)]],
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
      
      df <- df %>% left_join(thresholds, by = "Habitat")
      
      df <- df %>%
        rowwise() %>%
        mutate(ysb = lubridate::year(Sys.Date()) - YLB,
               message = case_when(
                 is.na(ysb) ~ paste(SDSFEATURENAME, "has missing burn data."),
                 ysb < Min ~ paste(SDSFEATURENAME, "was burned recently."),
                 ysb > Max ~ paste(SDSFEATURENAME, "has not been burned recently enough."),
                 TRUE ~ paste(SDSFEATURENAME, "falls within the ideal rotation interval.")
               )) %>%
        ungroup()
      
      HTML(paste0("<ul>", paste0("<li>", df$message, "</li>", collapse = ""), "</ul>"))
    })
  })
  
  # Reactive: Generate plots for each habitat
  output$burn_graphs <- renderUI({
    req(input$evaluate)
    isolate({
      habs <- unique_habitats()
      
      plots <- lapply(habs, function(hab) {
        safe_id <- make.names(hab)
        min_val <- input[[paste0("min_", safe_id)]]
        max_val <- input[[paste0("max_", safe_id)]]
        
        x <- seq(0, 12.5, by = 0.01)
        util <- double_logistic(t = x, rise_mid = min_val, fall_mid = max_val)
        utildf <- data.frame(ysb = x, util = util)
        
        plotOutput(paste0("plot_", safe_id))
      })
      
      do.call(tagList, plots)
    })
  })
  
  # Render each plot dynamically
  observe({
    habs <- unique_habitats()
    lapply(habs, function(hab) {
      safe_id <- make.names(hab)
      output[[paste0("plot_", safe_id)]] <- renderPlot({
        min_val <- input[[paste0("min_", safe_id)]]
        max_val <- input[[paste0("max_", safe_id)]]
        x <- seq(0, 12.5, by = 0.01)
        util <- double_logistic(t = x, rise_mid = min_val-0.5, fall_mid = max_val+2.5)
        utildf <- data.frame(ysb = x, util = util)
        
        ggplot(utildf, aes(x = ysb, y = util)) +
          geom_line(color = "blue") +
          geom_segment(aes(x = min_val, xend = min_val, y = 0, yend = 1), linetype = 5) +
          geom_segment(aes(x = max_val, xend = max_val, y = 0, yend = 1), linetype = 5) +
          labs(x = "Years since burn", y = "Utility",
               title = paste("Habitat:", hab)) +
          scale_x_continuous(limits = c(0, 13), breaks = seq(0, 12, 2)) +
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
          theme_bw() +
          theme(axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                plot.title = element_text(size = 16, face = "bold"))
      })
    })
  })
  
  # Burn optimization table
  output$burn_table <- renderTable({
    req(input$burn_schedule) 
    
    df <- get_points_from_file()
    req(df)
    
    day_labels <- c("1-day", "2-day", "3-day")
    blank_cols <- as_tibble(matrix(NA_character_, nrow = nrow(df), ncol = 3))
    colnames(blank_cols) <- day_labels
    
    table_df <- bind_cols(df["SDSFEATURENAME"], blank_cols) |>
      rename(Unit = SDSFEATURENAME)
    table_df
  })
}

shinyApp(ui, server)

# For deploying
#setwd("~/GitHub/RxFireEngine")
#rsconnect::deployApp()
