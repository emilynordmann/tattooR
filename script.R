# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(geosphere)

# Path to your GPX file
gpx_file_path <- "activity_16614330055.gpx"

# Function to read GPX file and extract elevation and time data
read_gpx <- function(file) {
  st_read(file, layer = "track_points", quiet = TRUE) %>%
    filter(!is.na(ele) & !is.na(time)) %>%
    mutate(
      time = as.POSIXct(time, format="%Y-%m-%dT%H:%M:%OS", tz="UTC"),
      lat = st_coordinates(geometry)[, 2],
      lon = st_coordinates(geometry)[, 1],
      distance = c(0, cumsum(distVincentySphere(
        cbind(lag(lon), lag(lat)), cbind(lon, lat))[!is.na(lag(lon))]))
    )
}


# Aggregate elevation data in 25-meter intervals
aggregate_elevation <- function(elevation_data) {
  elevation_data %>%
    mutate(distance_interval = floor(distance / 25) * 25) %>%
    group_by(distance_interval) %>%
    summarise(mean_elevation = mean(ele, na.rm = TRUE))
}

# Read GPX file and process elevation data
elevation_data <- read_gpx(gpx_file_path)
aggregated_data <- aggregate_elevation(elevation_data)
min_elevation <- min(aggregated_data$mean_elevation, na.rm = TRUE)

# Create the mountain profile plot
plot <- ggplot() +
  geom_line(data = aggregated_data %>% filter(distance_interval < 8000), aes(x = distance_interval, y = mean_elevation), color = "black") +
  geom_line(data = aggregated_data %>% filter(distance_interval >= 8000 & distance_interval <= 9700), aes(x = distance_interval, y = mean_elevation), color = "black", linetype = 2) +
  geom_line(data = aggregated_data %>% filter(distance_interval > 9700), aes(x = distance_interval, y = mean_elevation), color = "black") +
  theme_void() +
  scale_y_continuous(limits = c(min_elevation, NA)) +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_blank())

# Text and positioning details
text <- " ruith a dh'ionnsaigh na beinne......."
x_shift <- -13000  # Adjust as needed to move text to the left
spacing <- 350     # Adjust as needed for letter spacing

# Calculate text positions (single string)
text_positions <- seq(min(aggregated_data$distance_interval) + x_shift, 
                      min(aggregated_data$distance_interval) + (nchar(text) - 1) * spacing + x_shift, 
                      length.out = nchar(text))

# Add each character to the plot
for (i in seq_along(text_positions)) {
  plot <- plot + annotate("text", x = text_positions[i], 
                          y = aggregated_data$mean_elevation[which.min(abs(aggregated_data$distance_interval - text_positions[i]))] + 25, 
                          label = substr(text, i, i), 
                          color = "black", size = 5, angle = 0, family = "Lucida Console")
}

# Display and save the plot
print(plot)
ggsave("elevation_profile_with_text.png", plot, width = 30, height = 5, units = "cm")
