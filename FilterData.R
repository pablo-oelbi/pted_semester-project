# Load packages
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

library(dplyr)
library(purrr)

# Define a function to get the trips for a single user
get_trips <- function(df) {
  df <- df %>%
    arrange(datetime) %>%  # make sure the data is in the correct order
    mutate(time_diff = c(0, difftime(datetime[-1], datetime[-n()], units = "mins"))) %>% 
    mutate(trip_id = cumsum(transport_mode != lag(transport_mode, default = transport_mode[1]) | time_diff > 15)) %>%
    mutate(next_transport_mode_tmp = lead(transport_mode, default = NA),
           next_trip_id = trip_id+1)%>%
    group_by(trip_id) %>%
    mutate(next_transport_mode = last(next_transport_mode_tmp)) 
    #fill(next_transport_mode, .direction = "up")
  
  df$next_transport_mode_tmp <- NULL
  
  return(df)
}


# Import and prepare data
posmo <- read_delim("data/posmo_complete.csv")



# data analysis for the quality
any(is.na(df))



posmo_enriched = posmo|>distinct() |> group_by(user_id)|>group_split()|>map_dfr(get_trips)


# Add a row number column to posmo
#posmo <- posmo |> 
#  mutate(row_num = row_number())


# Retrieve the transportation mode from the previous row
posmo <- posmo |> 
  mutate(last_transport_mode = lag(transport_mode, n = 1))


# Filter the dataframe to include only the days with train trips
train_days <- posmo |> 
  filter(last_transport_mode == "Train") %>%
  distinct(date)

# Filter the posmo dataframe to include only the days with train trips
posmo_filtered <- posmo |> 
  filter(date %in% train_days$date) 

# Remove the train trip data (we don't need this) 
posmo_filtered <- posmo_filtered |> 
  filter(!(transport_mode == "Train" & last_transport_mode == "Train"))



## Create a list with all the trips right before the train trips ##

# Initialize variables
trips <- list()
current_trip <- data.frame()
previous_transport_mode <- NULL

# Iterate over each data point
for (i in 1:nrow(posmo_filtered)) {
  if (i == 1) {
    # Add the first data point to the current trip
    current_trip <- rbind(current_trip, posmo_filtered[i, ])
  } else {
    # Check if transport mode or time difference indicates the end of the trip
    if (posmo_filtered$transport_mode[i] != previous_transport_mode ||
        difftime(posmo_filtered$datetime[i], posmo_filtered$datetime[i - 1], units = "mins") > 15) {
      
      # Check if the next trip is with Train
      next_transport_mode <- posmo_filtered$transport_mode[i]
      if (next_transport_mode == "Train") {
        # Add the completed trip to the list of trips
        trips <- c(trips, list(current_trip))
      }
      
      # Start a new trip with the current data point
      current_trip <- data.frame()
      current_trip <- rbind(current_trip, posmo_filtered[i, ])
    } else {
      # Add the data point to the current trip
      current_trip <- rbind(current_trip, posmo_filtered[i, ])
    }
  }
  
  # Store the transport mode for the next iteration
  previous_transport_mode <- posmo_filtered$transport_mode[i]
}



## Visualization on a map ##

# Create a list to store trip objects
trip_objects <- list()

# Convert each trip data frame to a SpatialPointsDataFrame object
for (i in 1:length(trips)) {
  trip <- trips[[i]]
  trip_sp <- SpatialPointsDataFrame(trip[, c("lon_x", "lat_y")], trip)
  
  # Store the trip object in the list
  trip_objects[[i]] <- trip_sp
}


# Create a map for each trip
for (i in 1:length(trip_objects)) {
  tm_shape(trip_objects[[i]]) +
    tm_dots()
}

# Combine all trips into a single sf object
all_trips <- do.call(rbind, trip_objects)

# Create a map with tmap for all trips
tmap_mode("view")
tm_shape(all_trips) +
  tm_dots(col = "transport_mode")


