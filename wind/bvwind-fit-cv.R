set.seed(123)
library(ngme2)
library(tidyverse)
library(fmesher)

fit_gauss <- readRDS("model-fits/wind-fit-gauss.rds")
fit_nig <- readRDS("model-fits/wind-fit-nig.rds")
data <- readRDS("wind/wind_data_sub.rds")
str(data)

# plot lon and lat
ggplot(data, aes(x = lon, y = lat)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Longitude and Latitude")

loc <- data %>%
  select(lon, lat) %>%
  distinct()

mesh_s <- fm_mesh_2d(
  loc,
  cutoff = 1,
  max.edge = c(2, 5)
)

mesh_s$n
plot(mesh_s)
points(loc$lon, loc$lat, col = "red")

data$hour <- as.integer(data$hour) / 6
head(data)

# Reshaping data to combine u_wind and v_wind
data_long <- data %>%
  pivot_longer(
    cols = c(u_wind, v_wind),
    names_to = "direction",
    values_to = "wind"
  )


cv_splits <- create_paired_cv_splits(data_long, loc_col = c("lon", "lat"), group = "direction", k = 10, seed = 123)

cv <- cross_validation(
  list(
    bv_gauss = fit_gauss,
    bv_nig = fit_nig
  ),
  type = "custom",
  test_idx = cv_splits$test_idx,
  train_idx = cv_splits$train_idx,
  parallel = FALSE,
  n_gibbs_samples = 100,
  print = TRUE,
  N_sim = 2,
  seed = 123,
  merge_groups = TRUE
)
saveRDS(cv, "model-fits/bvwind-fit-cv.rds")
cv <- readRDS("model-fits/bvwind-fit-cv.rds")
cv


# Print the mean scores in a pretty format
table <- cv$mean.scores
print(table)
