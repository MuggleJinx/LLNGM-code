# Load packages
library(ncdf4)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(rnaturalearth) # For map data

# Step 1: Read u-wind and v-wind data
# Open netCDF files
u_nc <- nc_open("wind/uwnd.10m.gauss.2025.nc")
v_nc <- nc_open("wind/vwnd.10m.gauss.2025.nc")

# Extract variables
u_wind <- ncvar_get(u_nc, "uwnd") # u-wind data
v_wind <- ncvar_get(v_nc, "vwnd") # v-wind data
lon <- ncvar_get(u_nc, "lon") # longitude
lat <- ncvar_get(u_nc, "lat") # latitude
time <- ncvar_get(u_nc, "time") # time (hours since 1800-01-01)

# Close files
nc_close(u_nc)
nc_close(v_nc)

range(lon)
range(lat)

# Step 2: Process time
# Time unit: hours since 1800-01-01
time_origin <- as.POSIXct("1800-01-01 00:00:00", tz = "UTC")
time <- time_origin + hours(time)
hour <- (time - as.POSIXct("2025-01-01 00:00:00", tz = "UTC")) / 3600
hour

time_n <- length(unique(hour))
time_n

# Step 3: Calculate wind speed and direction
# Create arrays with same dimensions as u_wind and v_wind
wind_speed <- array(NA, dim = dim(u_wind))
wind_direction <- array(NA, dim = dim(u_wind))

# Wind speed = sqrt(u^2 + v^2)
# Wind direction = atan2(u, v) * (180 / pi) % 360
# for (t in 1:dim(u_wind)[3]) {
#   wind_speed[,,t] <- sqrt(u_wind[,,t]^2 + v_wind[,,t]^2)
#   wind_direction[,,t] <- (atan2(u_wind[,,t], v_wind[,,t]) * (180 / pi)) %% 360
# }

# Step 4: Create data frame
# Convert arrays to long format
wind_data <- expand.grid(lon = lon, lat = lat, hour = hour) %>%
  mutate(
    # wind_speed = as.vector(wind_speed),
    # wind_direction = as.vector(wind_direction),
    u_wind = as.vector(u_wind),
    v_wind = as.vector(v_wind)
  )
str(wind_data)
any(is.na(wind_data)) # No NA
range(wind_data$u_wind)
range(wind_data$v_wind)

unique(wind_data$hour)

lon_range <- c(10, 36)
lat_range <- c(34, 48)
wind_data_sub <- wind_data %>%
  filter(
    hour >= 0 & hour <= 60,
    lon >= lon_range[1] & lon <= lon_range[2],
    lat >= lat_range[1] & lat <= lat_range[2]
  )

str(wind_data_sub)

saveRDS(wind_data_sub, "wind/wind_data_sub.rds")

# Step 5: Visualize longitude and latitude data
# Read saved subset data
wind_data_sub <- readRDS("wind/wind_data_sub.rds")

# Select a time point for visualization (e.g., first hour)
hour_to_plot <- 0
wind_data_hour <- wind_data_sub %>%
  filter(hour == hour_to_plot)

# Calculate wind speed and direction for this time point
wind_data_hour <- wind_data_hour %>%
  mutate(
    wind_speed = sqrt(u_wind^2 + v_wind^2),
    wind_direction = (atan2(u_wind, v_wind) * (180 / pi)) %% 360
  )

# Get Europe map
europe <- ne_countries(scale = "medium", continent = "europe", returnclass = "sf")

# Plot basic map
ggplot() +
  # Add Europe map background
  geom_sf(data = europe, fill = "lightgray", color = "white", size = 0.2) +
  # Add wind direction and speed using arrows (geom_segment)
  geom_segment(
    data = wind_data_hour, # %>% sample_frac(0.1), # Randomly sample 10% of points to avoid overcrowding
    aes(
      x = lon, y = lat,
      xend = lon + u_wind / 5, yend = lat + v_wind / 5,
      color = wind_speed
    ),
    arrow = arrow(length = unit(0.1, "cm")),
    size = 0.5
  ) +
  # Limit map range
  coord_sf(xlim = lon_range, ylim = lat_range) +
  # Add color legend
  scale_color_viridis_c(name = "Wind Speed (m/s)") +
  # Add title and labels
  labs(
    # title = paste("Wind Direction and Speed at Hour", hour_to_plot),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggsave("Figures/wind_direction_speed.png")


# Create wind speed heatmap
# ggplot() +
#   # Add Europe map background
#   geom_sf(data = europe, fill = "lightgray", color = "white", size = 0.2) +
#   # Add wind speed points
#   geom_tile(
#     data = wind_data_hour,
#     aes(x = lon, y = lat, fill = wind_speed)
#   ) +
#   # Limit map range
#   coord_sf(xlim = c(16, 32), ylim = c(34, 55)) +
#   # Add color legend
#   scale_fill_viridis_c(name = "Wind Speed (m/s)") +
#   # Add title and labels
#   labs(
#     title = paste("Wind Speed at Hour", hour_to_plot),
#     x = "Longitude",
#     y = "Latitude"
#   ) +
#   theme_minimal()

# Create wind direction animation (requires gganimate package)
# If you want to create animation, uncomment below and install gganimate package
# library(gganimate)
#
# wind_animation <- ggplot() +
#   geom_sf(data = europe, fill = "lightgray", color = "white", size = 0.2) +
#   geom_segment(
#     data = wind_data_sub %>%
#       group_by(hour) %>%
#       sample_frac(0.05),
#     aes(
#       x = lon, y = lat,
#       xend = lon + u_wind/5, yend = lat + v_wind/5,
#       color = sqrt(u_wind^2 + v_wind^2)
#     ),
#     arrow = arrow(length = unit(0.1, "cm")),
#     size = 0.5
#   ) +
#   coord_sf(xlim = c(16, 32), ylim = c(34, 55)) +
#   scale_color_viridis_c(name = "Wind Speed (m/s)") +
#   labs(title = "Hour: {frame_time}") +
#   transition_time(hour) +
#   ease_aes('linear')
#
# animate(wind_animation, nframes = 60, fps = 5, width = 800, height = 600)
