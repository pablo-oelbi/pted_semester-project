# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)
library(sf)
library(sp)
library(tmap)
library(tmaptools)
library(geosphere)
library(purrr)

# Define a function to get the trips for a single user
get_trips <- function(df) {
  df <- df %>%
    arrange(datetime) %>%  # Sort data by datetime
    mutate(time_diff = c(0, difftime(datetime[-1], datetime[-n()], units = "mins"))) %>%  # Calculate time difference between records
    mutate(trip_id = cumsum(transport_mode != lag(transport_mode, default = transport_mode[1]) | time_diff > 15)) %>%  # Identify individual trips
    mutate(next_transport_mode_tmp = lead(transport_mode, default = NA),
           last_transport_mode_tmp = lag(transport_mode, default = NA)) %>% 
    group_by(trip_id) %>%
    mutate(next_transport_mode = last(next_transport_mode_tmp),
           last_transport_mode = first(last_transport_mode_tmp)) 
  df$next_transport_mode_tmp <- NULL
  df$last_transport_mode_tmp <- NULL
  return(df)
}

# List all CSV files in the directory
file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)

# Read each CSV file into a data frame and concatenate them together
posmo <- do.call(rbind, lapply(file_list, function(x) read.csv(x, stringsAsFactors = FALSE)))

# Convert the datetime column to POSIXct type
posmo$datetime <- as.POSIXct(posmo$datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")

# Convert user_id to a factor and then to numeric
posmo$user_id <- as.numeric(factor(posmo$user_id))

# Keep only the necessary columns
posmo <- posmo |> 
  arrange(user_id, datetime) |>  # Sort data by user_id and datetime
  select(user_id, datetime,transport_mode, lon_x, lat_y)  # Select necessary columns

# Check for NA's and empty strings in the data
any(is.na(posmo))
any(sapply(posmo |>select(user_id,transport_mode, lon_x, lat_y), function(x) any(x == "")))

# Create a data frame with non-empty transport_mode
posmo <- posmo|>filter(transport_mode != "")

# Remove duplicated rows, split the data frame by user_id, apply the get_trips function to each group, and bind the results into a single data frame posmo_enriched.
posmo_enriched <- posmo|>distinct() |> group_by(user_id)|>group_split()|>map_dfr(get_trips)

# Function to calculate distances for one user
calculate_distances <- function(data) {
  # Convert the data to sf object for spatial operations
  coordinates <- st_as_sf(data, coords = c("lon_x", "lat_y"), crs = 4326)
  coordinates_matrix <- st_coordinates(coordinates)
  distances <- c(0, distVincentySphere(coordinates_matrix[-1, ], coordinates_matrix[-nrow(data), ]))
  data$distance <- distances
  
  # Calculate cumulative distance and time for each trip
  data <- data %>%
    arrange(trip_id, datetime) %>%
    group_by(trip_id) %>%
    mutate(
      cumulative_distance =cumsum(distance),
      cumulative_time = cumsum(time_diff),
      elapsed_time = sum(time_diff),
      total_trip_distance = sum(distance)
    ) %>%
    ungroup() %>%
    mutate(
      has_train_before = last_transport_mode == 'Train',
      has_train_after = next_transport_mode == 'Train',
      has_train_somewhere = transport_mode == "Train" | next_transport_mode == 'Train' | last_transport_mode == 'Train'
    )
  
  return(data)
}

# Apply the calculate_distances function to each user
data_by_user <- split(posmo_enriched, posmo_enriched$user_id)
posmo_enriched <- do.call(rbind, lapply(data_by_user, calculate_distances))

# Filter trips that involve a train
posmo_enriched_train <- posmo_enriched |> filter(has_train_somewhere == TRUE)

# Grouped analysis
posmo_analysis <- posmo_enriched_train %>%
  mutate(day = lubridate::date(datetime)) %>%
  group_by(trip_id, user_id, day, transport_mode, last_transport_mode, next_transport_mode) %>%
  summarise(
    distance = sum(distance),
    total_time = max(cumulative_time),
    direct_distance = distVincentySphere(c(lon_x[1], lat_y[1]), c(lon_x[n()], lat_y[n()]))
  ) %>%
  ungroup()

# Visualize trips
library(tmap)

# Iterate through trips with has_train_somewhere = TRUE
train_trips <- posmo_enriched_train %>%
  filter(has_train_somewhere)

for (i in unique(train_trips$trip_id)) {
  # Subset data for the current trip
  current_trip <- posmo_enriched_train %>%
    filter(trip_id == i)
  
  # Create a map for the current trip
  tmap_mode("view")
  tm_shape(current_trip) +
    tm_dots(col = "transport_mode") +
    tm_basemap(server = "OpenStreetMap") +
    tm_layout(title = paste("Trip ID:", i))
}

# Convert the data frame to a spatial object
df_sf <- st_as_sf(train_trips, coords = c("lon_x", "lat_y"), crs = 4326)

# Get the shapefile for Switzerland
library(rnaturalearth)
library(rnaturalearthdata)
switzerland <- ne_countries(scale = "medium", country = "Switzerland", returnclass = "sf")

# Convert posmo_enriched_train to sf object and set CRS
posmo_sf <- st_as_sf(posmo_enriched_train, coords = c("lon_x", "lat_y"), crs = 4326)

# Transform the CRS of switzerland to match posmo_sf
switzerland <- st_transform(switzerland, crs = st_crs(posmo_sf))

# Spatial join to select trips within Switzerland
posmo_within_switzerland <- st_join(posmo_sf, switzerland, join = st_within)

# Filter out points outside of Switzerland
posmo_within_switzerland <- posmo_within_switzerland[!is.na(posmo_within_switzerland$iso_a2), ]

# Plot the trips within Switzerland
tmap_mode("plot")
tm_shape(posmo_within_switzerland) +
  tm_dots(col = "transport_mode") +
  tm_basemap(server = "OpenStreetMap") +
  tm_layout(title = "Trips within Switzerland")




posmo_analysis %>%
  group_by(transport_mode, last_transport_mode, next_transport_mode) %>%
  summarise(count = n(), .groups = 'drop')



#Für die Analyse: 
clipr::write_clip(posmo_analysis %>%
                    group_by(last_transport_mode, transport_mode, next_transport_mode) %>%
                    summarise(count = n(), .groups = 'drop'))


clipr::write_clip(posmo_analysis)


