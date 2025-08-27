# ---- Libraries ----
install.packages("curl")
install.packages("httr2")

packageVersion("curl")
packageVersion("httr2")


library(httr2)
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(leaflet)   # optional if you still want map output

# ---- Example run ----
# Replace with your test file path:
test_file <- "shiny_test.xlsx"

# ---- Load file (CSV, XLSX, or ZIP shapefile) ----
get_points_from_file <- function(filepath) {
  ext <- tools::file_ext(filepath)
  temp_dir <- tempdir()
  
  if (ext == "zip") {
    unzip(filepath, exdir = temp_dir)
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    shp_data <- st_read(shp_file[1])
    centroids <- st_centroid(shp_data)
    coords <- st_coordinates(centroids)
    df <- data.frame(
      Unit = shp_data$Name %||% paste0("Unit_", seq_len(nrow(shp_data))),
      Latitude = coords[,2],
      Longitude = coords[,1],
      Habitat = shp_data$Habitat %||% NA,
      YearsSinceBurn = shp_data$YearsSinceBurn %||% NA
    )
  } else if (ext %in% c("csv", "xlsx")) {
    if (ext == "csv") {
      df <- read_csv(filepath)
    } else {
      df <- read_excel(filepath)
    }
    df <- normalize_latlon(df)
  } else {
    stop("Unsupported file type")
  }
  
  df
}

# ---- Normalize lat/lon columns ----
normalize_latlon <- function(df) {
  lat_names <- c("Latitude", "Lat", "latitude", "lat")
  lon_names <- c("Longitude", "Long", "longitude", "long")
  
  lat_col <- intersect(names(df), lat_names)
  lon_col <- intersect(names(df), lon_names)
  
  if (length(lat_col) == 0 || length(lon_col) == 0) {
    stop("Spreadsheet must include recognizable latitude and longitude columns.")
  }
  
  df |>
    rename(Latitude = all_of(lat_col[1]),
           Longitude = all_of(lon_col[1]))
}

points <- get_points_from_file(test_file)
print(points)

# Example thresholds per habitat
thresholds <- data.frame(
  Habitat = unique(points$Habitat),
  Min = 1,
  Max = 5
)

lat <- 35.8
lon <- -78.6

# ---- Function to get 3-day weather ----
get_forecast <- function(lat, lon, start_date = Sys.Date()) {
  ua <- "RxFire Engine Prototype (sswinea@ncsu.edu)"
  
  point_meta <- request("https://api.weather.gov") |>
    req_user_agent(ua) |>
    req_url_path_append("points", paste0(lat, ",", lon)) |>
    req_perform() |>
    resp_body_json()
  
  grid_url <- point_meta$properties$forecastGridData
  
  grid_data <- request(grid_url) |>
    req_user_agent(ua) |>
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
      mutate(start = ymd_hms(start),
             date = as_date(start))
  }
  
  temp_max <- extract_quant_values(grid_data$maxTemperature, "max_temp")
  temp_min <- extract_quant_values(grid_data$minTemperature, "min_temp")
  wind_speed <- extract_quant_values(grid_data$windSpeed, "wind_speed")
  rh_min <- extract_quant_values(grid_data$minRelativeHumidity, "min_rh")
  precip_amt <- extract_quant_values(grid_data$quantitativePrecipitation, "precip_amt")
  
  forecast_all <- bind_rows(temp_max, temp_min, wind_speed, rh_min, precip_amt) |>
    mutate(value = ifelse(variable %in% c("max_temp", "min_temp"), value * 9/5 + 32, value)) |>
    filter(date >= start_date & date <= start_date + 2) |>
    group_by(date, variable) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = variable, values_from = value)
  
  forecast_all
}

# this works
# https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&dataTypes=AIR_TEMP&stations=USW00013858&startDate=2021-01-01&endDate=2021-01-03&boundingBox=35.9,-78.7,35.7,-78.5&format=csv&units=standard

# Current or Historical Weather
get_forecast <- function(lat, lon, start_date = Sys.Date()) {
  if (year(start_date) == 2021) {
    # ---- Historical data via NCEI v1 endpoint ----
    
    delta <- 0.1  # small bounding box (~10 km)
    minLat <- lat - delta
    maxLat <- lat + delta
    minLon <- lon - delta
    maxLon <- lon + delta
    
    # Build API URL
    ncei_url <- "https://www.ncei.noaa.gov/access/services/data/v1"
    full_url <- paste0(
      ncei_url,
      "?dataset=daily-summaries",
      "&dataTypes=AIR_TEMP",
      "&stations=USW00013858",
      "&startDate=", start_date,
      "&endDate=", start_date + 2,
      "&boundingBox=", maxLat, ",", minLon, ",", minLat, ",", maxLon,
      "&format=json",
      "&units=standard"
    )
    
    df <- request(full_url) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE) |>
      as_tibble()
    
    if (nrow(df) == 0) {
      return(tibble(error = "No NCEI data found for this location/date"))
    }
    
    # Rename variables to match your app
    df <- df %>%
      rename(
        date = DATE,
        max_temp = TMAX,
        min_temp = TMIN,
        precip_amt = PRCP,
        wind_speed = AWND
      ) %>%
      mutate(
        date = as_date(date),
        min_rh = NA, # RH not available
        max_temp = as.numeric(max_temp),
        min_temp = as.numeric(min_temp),
        precip_amt = as.numeric(precip_amt),
        wind_speed = as.numeric(wind_speed)
      )
    
    return(df)
    
  } else {
    
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
      ) |>
        separate(start_time, into = c("start", "duration"), sep = "/") |>
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
    
    forecast_all <- bind_rows(temp_max, temp_min, wind_speed, rh_min, precip_amt) |>
      mutate(
        value = ifelse(variable %in% c("max_temp", "min_temp"), value * 9/5 + 32, value)
      ) |>
      filter(date >= start_date & date <= start_date + 2) |>
      group_by(date, variable) |>
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = variable, values_from = value)
    
    return(forecast_all)
  }
}

# ---- Run forecast for all points ----
run_forecast <- function(points) {
  points |>
    rowwise() |>
    mutate(
      forecast = list(tryCatch(
        get_forecast(Latitude, Longitude, start_date),
        error = function(e) tibble(error = "NWS request failed"))
      )
    ) |>
    unnest(forecast)
}

# Run forecast
start_date <- as.Date("2021-01-01")
forecast <- run_forecast(points)
print(forecast)

# ---- Evaluate burn readiness ----
evaluate_burns <- function(df, thresholds) {
  df |>
    left_join(thresholds, by = "Habitat") |>
    rowwise() |>
    mutate(message = case_when(
      is.na(YearsSinceBurn) ~ paste(Unit, "has missing burn data."),
      YearsSinceBurn < Min ~ paste(Unit, "was burned recently."),
      YearsSinceBurn > Max ~ paste(Unit, "has not been burned recently enough."),
      TRUE ~ paste(Unit, "falls within the ideal rotation interval.")
    )) |>
    ungroup()
}

# Evaluate
burn_eval <- evaluate_burns(points, thresholds)
print(burn_eval$message)

# ---- Double Logistic Function ----
double_logistic <- function(t, rise_mid, fall_mid, rise_slope = 0.1, fall_slope = 0.5,
                            flat_length = 0.025, flat_slope = 0, flat_intercept = 1 - 0.025) {
  l1 <- 1 + exp((rise_mid - t) / rise_slope)
  l2 <- 1 + exp((fall_mid - t) / fall_slope)
  y <- flat_length + (flat_intercept - flat_slope * t) * (1 / l1 - 1 / l2)
  return(y)
}

# Example utility curve plot for first habitat thresholds
library(ggplot2)
x <- seq(0, 12.5, by = 0.01)
util <- double_logistic(t = x, rise_mid = 1, fall_mid = 5)
utildf <- data.frame(ysb = x, util = util)

ggplot(utildf, aes(x = ysb, y = util)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(1, 5), linetype = 5) +
  labs(x = "Years since burn", y = "Utility",
       title = paste("Habitat:", thresholds$Habitat[1]))

#___________________________________________________________________________________________________
get_forecast <- function(lat, lon, start_date = Sys.Date()) {
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
    mutate(
      value = ifelse(variable %in% c("max_temp", "min_temp"), value * 9/5 + 32, value)
    ) |>
    filter(date >= start_date & date <= start_date + 2) |>  # 3-day forecast
    group_by(date, variable) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = variable, values_from = value)
  
  forecast_all
}
