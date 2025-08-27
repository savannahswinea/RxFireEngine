library(shiny)
library(httr2)
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(leaflet)

# packageVersion("curl")
# packageVersion("httr2")

# ---- Function to get 3-day weather ----
# Inspired by https://3mw.albert-rapp.de/p/weather-api
get_forecast <- function(lat, lon, start_date = Sys.Date()) {
  
  # ---- Use NCEI for historical 2021 data ----
  if (year(start_date) == 2021) {
    
    # NCEI token stored in environment variable
    ncei_token <- Sys.getenv("ralplPZgDncehbSWtiZrhgVdUnKAjVyb")
    
    # Find nearest station
    station_req <- request("https://www.ncei.noaa.gov/cdo-web/api/v2/stations") |>
      req_url_query(
        datasetid = "GHCND",
        limit = 1,
        startdate = start_date,
        enddate = start_date,
        latitude = lat,
        longitude = lon
      ) |>
      req_headers(token = ncei_token) |>
      req_perform() |>
      resp_body_json()
    
    if (length(station_req$results) == 0) {
      return(tibble(error = "No NCEI station found for this location/date"))
    }
    
    station_id <- station_req$results[[1]]$id
    
    # Request daily data for 3 days
    data_req <- request("https://www.ncei.noaa.gov/cdo-web/api/v2/data") |>
      req_url_query(
        datasetid = "GHCND",
        stationid = station_id,
        startdate = start_date,
        enddate = start_date + 2,
        units = "standard",
        limit = 1000
      ) |>
      req_headers(token = ncei_token) |>
      req_perform() |>
      resp_body_json()
    
    # Convert to tibble
    df <- map_dfr(data_req$results, function(x) {
      tibble(
        date = as_date(x$date),
        variable = x$datatype,
        value = x$value
      )
    })
    
    # Pivot to match variable names in app
    df <- df %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      rename(
        max_temp = TMAX,
        min_temp = TMIN,
        precip_amt = PRCP,
        wind_speed = AWND
      ) %>%
      mutate(
        min_rh = NA  # Relative humidity not available in GHCND
      )
    
    return(df)
    
  } else {
    
    # ---- NWS API for current/future forecasts ----
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
      ) %>%
        separate(start_time, into = c("start", "duration"), sep = "/") %>%
        mutate(
          start = ymd_hms(start),
          date = as_date(start)
        )
    }
    
    temp_max <- extract_quant_values(grid_data$maxTemperature, "max_temp")
    temp_min <- extract_quant_values(grid_data$minTemperature, "min_temp")
    wind_speed <- extract_quant_values(grid_data$windSpeed, "wind_speed")
    rh_min <- extract_quant_values(grid_data$minRelativeHumidity, "min_rh")
    precip_amt <- extract_quant_values(grid_data$quantitativePrecipitation, "precip_amt")
    
    forecast_all <- bind_rows(temp_max, temp_min, wind_speed, rh_min, precip_amt) %>%
      mutate(
        value = ifelse(variable %in% c("max_temp", "min_temp"), value * 9/5 + 32, value)
      ) %>%
      filter(date >= start_date & date <= start_date + 2) %>%
      group_by(date, variable) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = variable, values_from = value)
    
    return(forecast_all)
    
  }
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
                        tags$p("This app helps fire managers make prescribed burn decisions using short-term weather forecasts and habitat rotation intervals.",
                               style = "margin-top: 20px;"),
                        tags$h4("What You Need"),
                        tags$ul(
                          tags$li("If you have a shapefile (.zip), the attribute table needs the following columns:"),
                          tags$ul(
                            tags$li("Unit: a unique name for each unit"),
                            tags$li("Habitat: a habitat classification for that unit"),
                            tags$li("YearsSinceBurn: how many years ago a unit was burned")
                          ),
                          tags$li("If you have a spreadsheet (.csv or .xlsx), you also need Latitude and Longitude columns.")
                        ),
                        tags$h4("Download Example Data"),
                        tags$p(tags$a(href = "RxFire_example.xlsx", download = NA, "Click here to download the example spreadsheet.")
                        ),
                        tags$p("For questions or feedback, please contact Savannah Swinea at sswinea@ncsu.edu.")
                      )
               )
             )
    ),
    
    tabPanel("Weather Forecast",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_upload", "Upload shapefile (.zip) or spreadsheet (.csv/.xlsx)"),
                 dateInput(
                   "forecast_date", 
                   "Select start date for 3-day forecast:",
                   value = as.Date("2021-01-01"),
                   min = as.Date("2021-01-01"),
                   max = as.Date("2021-12-29") # prevent spilling into 2022
                 ),
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
             mainPanel(
             )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  shared <- reactiveValues(y = NULL, habitats = NULL)
  
  # Helper to normalize lat/lon column names
  normalize_latlon <- function(df) {
    lat_names <- c("Latitude", "Lat", "latitude", "lat")
    lon_names <- c("Longitude", "Long", "longitude", "long")
    
    lat_col <- intersect(names(df), lat_names)
    lon_col <- intersect(names(df), lon_names)
    
    validate(
      need(length(lat_col) > 0 && length(lon_col) > 0, 
           "Spreadsheet must include recognizable latitude and longitude columns.")
    )
    
    df <- df |>
      rename(Latitude = all_of(lat_col[1]),
             Longitude = all_of(lon_col[1]))
    
    df
  }
  
  # Read uploaded file and extract lat/lon
  get_points_from_upload <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    temp_dir <- tempdir()
    
    if (ext == "zip") {
      unzip(input$file_upload$datapath, exdir = temp_dir)
      shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      shp_data <- st_read(shp_file[1])
      centroids <- st_centroid(shp_data)
      coords <- st_coordinates(centroids)
      df <- data.frame(
        Unit = shp_data$Name %||% paste0("Unit_", seq_len(nrow(shp_data))),
        Latitude = coords[,2],
        Longitude = coords[,1],
        stringsAsFactors = FALSE
      )
      
    } else if (ext %in% c("csv", "xlsx")) {
      if (ext == "csv") {
        df <- read_csv(input$file_upload$datapath)
      } else {
        df <- read_excel(input$file_upload$datapath)
      }
      
      df <- normalize_latlon(df)
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
  })
  
  # Run forecast on each point
  forecast_data <- eventReactive(input$run_forecast, {
    points <- get_points_from_upload()
    req(points)
    req(input$forecast_date)
    
    points |>
      rowwise() |>
      mutate(
        forecast = list(tryCatch(
          get_forecast(Latitude, Longitude, start_date = input$forecast_date),
          error = function(e) tibble(error = "NWS request failed"))
        )
      ) |>
      unnest(forecast)
  })
  
  # Map units
  output$unit_map <- renderLeaflet({
    df <- forecast_data()
    req(df)
    
    leaflet(df) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        label = ~Unit, radius = 5,
        fillOpacity = 0.8, color = "blue"
      )
  })
  
  # Output max temp in 3-day interval for each unit
  output$forecast_bullets <- renderUI({
    df <- forecast_data()
    req(df)
    
    df_max <- df |>
      group_by(Unit) |>
      filter(max_temp == max(max_temp, na.rm = TRUE)) |>
      slice_head(n = 1) |>   # In case of ties, take the first
      ungroup()
    
    bullets <- df_max |>
      mutate(msg = paste0(
        "<li><strong>", Unit, "</strong> will experience a maximum temperature of <strong>", 
        round(max_temp, 1), "\u00B0F</strong> on <strong>", format(date, "%b %d"), "</strong> in the next three days.</li>"
      )) |>
      pull(msg) |>
      unname() |>   # drop names to avoid JSON warning
      paste(collapse = "\n")
    
    HTML(paste0("<ul>", bullets, "</ul>"))
  })
  
  # Reactive: Get unique habitats
  unique_habitats <- reactive({
    req(get_points_from_upload())
    unique(get_points_from_upload()$Habitat)
  })
  
  # Dynamic UI: min/max inputs for each Habitat
  output$threshold_inputs <- renderUI({
    req(unique_habitats())
    lapply(unique_habitats(), function(hab) {
      safe_id <- make.names(hab)  # sanitize input ID
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
      df <- get_points_from_upload()
      req(all(c("Unit", "Habitat", "YearsSinceBurn") %in% names(df)))
      
      habs <- unique_habitats()
      
      # Collect thresholds
      thresholds <- lapply(habs, function(hab) {
        safe_id <- make.names(hab)
        data.frame(
          Habitat = hab,
          Min = input[[paste0("min_", safe_id)]],
          Max = input[[paste0("max_", safe_id)]],
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
      
      # Join thresholds to input data
      df <- df %>% left_join(thresholds, by = "Habitat")
      
      # Generate messages
      df <- df %>%
        rowwise() %>%
        mutate(message = case_when(
          is.na(YearsSinceBurn) ~ paste(Unit, "has missing burn data."),
          YearsSinceBurn < Min ~ paste(Unit, "was burned recently."),
          YearsSinceBurn > Max ~ paste(Unit, "has not been burned recently enough."),
          TRUE ~ paste(Unit, "falls within the ideal rotation interval.")
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
        
        # x sequence
        x <- seq(0, 12.5, by = 0.01)
        
        # Generate utility curve using user thresholds
        util <- double_logistic(t = x, rise_mid = min_val, fall_mid = max_val)
        utildf <- data.frame(ysb = x, util = util)
        
        # Plot with ggplot
        p <- ggplot(utildf, aes(x = ysb, y = util)) +
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
}

shinyApp(ui, server)

# For deploying
#setwd("~/GitHub/RxFireEngine")
#rsconnect::deployApp()
