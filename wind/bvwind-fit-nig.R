set.seed(123)
library(fmesher)
library(tidyverse)
library(ngme2)
data <- readRDS("wind/wind_data_sub.rds")
str(data)

loc <- data %>%
  select(lon, lat) %>%
  distinct()

mesh_s <- fm_mesh_2d(
  loc,
  cutoff = 1,
  max.edge = c(2, 5)
)

mesh_s$n
# plot(mesh_s)
# points(loc$lon, loc$lat, col = "red")

data$hour <- as.integer(data$hour) / 6
head(data)

# Reshaping data to combine u_wind and v_wind
data_long <- data %>%
  pivot_longer(
    cols = c(u_wind, v_wind),
    names_to = "direction",
    values_to = "wind"
  )

head(data_long)
range(data_long$hour)
range(data_long$lon)
range(data_long$lat)

mesh_t <- fm_mesh_1d(data_long$hour)
mesh_t$n

fit_nig <- ngme(
  formula = wind ~
    fe(~ 0 + 1, which = "u_wind") +
    fe(~ 0 + lon, which = "u_wind") +
    fe(~ 0 + lat, which = "v_wind") +
    f(
      map = list(
        ~hour,
        ~ lon + lat
      ),
      model = tp(
        first = ar1(mesh = mesh_t),
        second = bv_matern(
          theta = -0.5,
          mesh = mesh_s,
          sub_models = list(
            u_wind = matern(),
            v_wind = matern()
          )
        )
      ),
      noise = noise_nig(
        fix_theta_sigma = TRUE,
        nu_lower = 0.01
      )
    ),
  group = data_long$direction,
  data = data_long,
  family = noise_normal(),
  control_opt = control_opt(
    print_check_info = TRUE,
    optimizer = precond_sgd(),
    verbose = TRUE,
    burnin = 100,
    iterations = 200,
    n_batch = 10,
    seed = 1,
    start_sd = 0.01,
    trend_lim = 0.05,
    std_lim = 3
  )
)
fit_nig
saveRDS(fit_nig, "model-fits/wind-fit-nig.rds")
fit_nig <- readRDS("model-fits/wind-fit-nig.rds")
traceplot(fit_nig)
