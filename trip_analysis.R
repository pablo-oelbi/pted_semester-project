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

# Keep only the necessary columns and combine walk and run
posmo <- posmo |> 
  arrange(user_id, datetime) |>  
  select(user_id, datetime,transport_mode, lon_x, lat_y) |>
  mutate(transport_mode = ifelse(transport_mode == 'Run', 'Walk', transport_mode),
         transport_mode = ifelse(transport_mode == 'Funicular', 'Tram', transport_mode))

# Check for NA's and empty strings in the data
any(is.na(posmo))

# check which columns: 
apply(posmo, 2, function(x) any(is.na(x)))
#remove na data
posmo <- na.omit(posmo)

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

posmo_train_related <- posmo_analysis |> 
  filter(next_transport_mode == 'Train' | last_transport_mode == 'Train') |> 
  filter(transport_mode!= 'Other1' & transport_mode!= 'Train')

visualize_trajectory <- function(user_id, trip_id, data) {
  # Subset the data for the specified user and trip
  specific_data <- data %>%
    dplyr::filter(user_id == !!user_id, trip_id %in% c((!!trip_id - 1), !!trip_id, (!!trip_id + 1)))
  
  # If there's no data for this user and trip, return a message
  if (nrow(specific_data) == 0) {
    return("No data for this user and trip")
  }
  
  # Convert the data to a spatial object
  data_sf <- sf::st_as_sf(specific_data, coords = c("lon_x", "lat_y"), crs = 4326)
  
  # Set up the map
  tmap::tmap_mode("view")
  
  # Create the map
  map <- tmap::tm_shape(data_sf) +
    tmap::tm_dots(col = "transport_mode") +
    tmap::tm_basemap(server = "OpenStreetMap") +
    tmap::tm_layout(title = paste("User ID:", user_id, "Trip ID:", trip_id))
  
  return(map)
}


#visualize_trajectory(user_id = 1, trip_id = 18, data = posmo_enriched_train)

# Boxplots, andere Plots
# Load the necessary library
library(ggplot2)
library(dplyr)

# Function to create a boxplot
create_boxplot <- function(data, value_column, remove_outliers = FALSE, add_title = FALSE, x_label_visible = TRUE, fixed_order = FALSE) {
  if (remove_outliers) {
    data <- data %>%
      group_by(transport_mode) %>%
      mutate(Q1 = quantile(get(value_column), 0.25, na.rm = TRUE),
             Q3 = quantile(get(value_column), 0.75, na.rm = TRUE),
             IQR = Q3 - Q1) %>%
      filter(#get(value_column) >= (Q1 - 1.5 * IQR),
             get(value_column) <= (Q3 + 1.5 * IQR))
  }
  
  
  
  
  if (fixed_order){
  # Define the fixed order for the transport_mode factor
  fixed_order <- c("Walk", "Bike", "Bus","Tram", "Car")
    data$transport_mode <- factor(data$transport_mode, levels = fixed_order)
  }else{
    data$transport_mode <- with(data, reorder(transport_mode, get(value_column), FUN = mean)) # Order by mean  
  }
  
  p<-ggplot(data, aes(x = transport_mode, y = !!sym(value_column), fill = transport_mode)) +
    geom_boxplot(outlier.shape = NA) +  # Don't plot outliers
    geom_jitter(width = 0.3, size = 1, alpha = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "white") +
    scale_fill_brewer(palette = "Set3") +
    labs(y = value_column) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  if (add_title) {
    p <- p + labs(title = paste("Boxplot of", value_column, "by Transport Mode"))
  }
  if (x_label_visible) {
    p <- p + labs(x = "Transport Mode")
  }else{
    p <- p + labs(x = "")
  }
  return(p)
}

# Example usage:
create_boxplot(data = posmo_train_related, value_column = "distance", remove_outliers = TRUE)


library(cowplot)

combined_boxplot <- function(data, value_columns, remove_outliers = FALSE, title = "Combined Boxplots") {
  
  # Create an empty list to store individual plots
  plot_list <- list()
  x_label_visible = FALSE
  # Loop over value_columns to create individual boxplots
  for (i in 1:length(value_columns)) {
    if (i == length(value_columns)){
      x_label_visible = TRUE
    }
    p <- create_boxplot(data, value_columns[i], remove_outliers, FALSE, x_label_visible, TRUE)
    plot_list[[i]] <- p
  }
  
  # Combine the plots using cowplot
  combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
  
  # Add a common title to the combined plot
  combined_plot <- cowplot::ggdraw(combined_plot) + 
    cowplot::draw_label(title, fontface = 'bold', size = 14, x = 0.5, y = 0.98)
  
  return(combined_plot)
}

# All Variables together
combined_boxplot(data = posmo_train_related, value_columns = c("distance", "direct_distance", "total_time"), remove_outliers = TRUE)


posmo_train_related<-posmo_train_related%>%
  group_by(transport_mode) %>%
  mutate(Q1 = quantile(get('distance'), 0.25, na.rm = TRUE),
         Q3 = quantile(get('distance'), 0.75, na.rm = TRUE),
         IQR = Q3 - Q1) %>%
  filter(get('distance') <= (Q3 + 1.5 * IQR))

# density plot
create_density_plot <- function(data, x_var = "distance", group_var = "transport_mode", title = "Density Plot") {
  library(ggplot2)
  ggplot(data, aes_string(x = x_var, fill = group_var)) +
    geom_density(alpha = 0.5) +
    labs(title = title, x = x_var, y = "Density") +
    theme_minimal()
}

create_density_plot(posmo_train_related)



create_pair_plot <- function(data, vars = c("distance", "total_time", "direct_distance"), title = "Pair Plot") {
  library(GGally)
  ggpairs(data, columns = vars, title = title, mapping = aes(color = transport_mode))
}

create_pair_plot(posmo_train_related)


create_pair_plot(posmo_train_related |> filter(distance > 2500))
create_pair_plot(posmo_train_related |> filter(distance < 2500 & total_time<250))
create_pair_plot(posmo_train_related |> filter(distance < 2500 & total_time<25))

posmo_train_related<-posmo_train_related |> filter(distance < 2500 & total_time<25)


# Assuming posmo_train_related is your DataFrame
# Convert transport_mode to a factor
posmo_train_related$transport_mode <- as.factor(posmo_train_related$transport_mode)

# Perform ANOVA
model <- aov(distance ~ transport_mode, data = posmo_train_related)
summary(model)

# The output of the Analysis of Variance (ANOVA) test indicates that there are statistically significant differences in the average distances traveled among the different modes of transport.
# 
# The term 'significant' in this context means that the observed differences in the means are unlikely to have occurred by chance. The p-value, which is less than 0.0000000000000002, supports this finding. A p-value is a measure of the probability that an observed difference could have occurred just by random chance. The smaller the p-value, the greater the statistical evidence you have to reject the null hypothesis (which in this case is that there is no difference in means across the groups). A commonly used threshold to denote statistical significance is 0.05, and any p-value less than this is generally described as statistically significant.
# 
# The F-value of 47.97 is quite large, indicating a strong relationship between the mode of transport and the distance traveled. This F-value is a ratio of the variance between the groups (variance in distances across different transport modes) to the variance within the groups (variance in distances within the same mode of transport). A larger F-value suggests that the variance between groups is significantly greater than the variance within groups, further strengthening the conclusion that the mode of transport has a significant impact on the distance traveled.
# 
# In summary, the output of the ANOVA test strongly suggests that the mode of transport used has a significant effect on the distance traveled. That is, the average distance traveled varies significantly depending on whether the mode of transport is a bike, bus, car, tram, etc. Further analysis (such as a post-hoc test) could be conducted to determine which specific modes of transport are significantly different from each other in terms of the distance traveled.
# 

# Perform the Tukey HSD test
posthoc <- TukeyHSD(model)

# Print the results
print(posthoc)


