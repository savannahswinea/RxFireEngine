library(shiny)
library(httr2)
library(tidyverse)
library(lubridate)

# Function to get 3-day weather
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
  temp_max <- extract_quant_values(grid_data$maxTemperature, "max_temp")
  temp_min <- extract_quant_values(grid_data$minTemperature, "min_temp")
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

ui <- fluidPage(
  titlePanel("RxFire Engine Prototype"),
  
  tabsetPanel(
    tabPanel("Weather Forecast",
             sidebarLayout(
               sidebarPanel(
                 numericInput("lat", "Latitude:", value = 38.8894),
                 numericInput("lon", "Longitude:", value = -77.0352),
                 actionButton("get_forecast", "Get Forecast")
               ),
               mainPanel(
                 tableOutput("forecast_table")
               )
             )
    ),
    
    tabPanel("Plot y = x²",
             sidebarLayout(
               sidebarPanel(
                 numericInput("x_input", "Enter a value for x:", value = 1)
               ),
               mainPanel(
                 plotOutput("plot_y")
               )
             )
    ),
    
    tabPanel("Plot z = log(y)",
             mainPanel(
               plotOutput("plot_z"),
               textOutput("y_value"),
               tableOutput("xy_table")
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Weather forecast reactive
  forecast_data <- eventReactive(input$get_forecast, {
    req(input$lat, input$lon)
    tryCatch({
      get_forecast(input$lat, input$lon)
    }, error = function(e) {
      tibble(error = "Failed to get forecast")
    })
  })
  
  output$forecast_table <- renderTable({
    forecast_data()
  })
  
  ## ---- y = x^2 ----
  shared <- reactiveValues(y = NULL)
  
  observe({
    shared$y <- input$x_input^2
  })
  
  output$plot_y <- renderPlot({
    x <- input$x_input
    y <- x^2
    plot(x, y, pch = 16, col = "blue",
         main = "y = x²",
         xlab = "x", ylab = "y")
  })
  
  ## ---- z = log(y) ----
  output$plot_z <- renderPlot({
    req(shared$y)
    z <- log(shared$y)
    plot(shared$y, z, pch = 16, col = "darkgreen",
         main = "z = log(y)",
         xlab = "y", ylab = "z")
  })
  
  output$y_value <- renderText({
    req(shared$y)
    paste("Current value of y:", round(shared$y, 2))
  })
  
  output$xy_table <- renderTable({
    req(shared$y)
    data.frame(
      x = input$x_input,
      y = shared$y
    )
  })
}

shinyApp(ui, server)

