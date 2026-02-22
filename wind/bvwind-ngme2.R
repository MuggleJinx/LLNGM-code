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
range(data$hour)
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


####### Gaussian ########

gauss_time <- Sys.time()
fit_gauss <- ngme(
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
          fix_theta = TRUE,
          mesh = mesh_s,
          sub_models = list(
            u_wind = matern(),
            v_wind = matern()
          )
        )
      ),
      noise = noise_normal(
        fix_theta_sigma = TRUE
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
    iterations = 100,
    n_batch = 10,
    seed = 1,
    start_sd = 0.01,
    trend_lim = 0.05,
    std_lim = 3
  )
)
gauss_time <- Sys.time() - gauss_time
fit_gauss
traceplot(fit_gauss)
saveRDS(fit_gauss, "model-fits/wind-fit-gauss.rds")
fit_gauss <- readRDS("model-fits/wind-fit-gauss.rds")


####### NIG ########

nig_time <- Sys.time()
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
        prior = priors(
          nu = prior_normal(0, 0.1)
        )
      )
    ),
  group = data_long$direction,
  data = data_long,
  family = noise_normal(),
  control_opt = control_opt(
    print_check_info = TRUE,
    verbose = TRUE,
    optimizer = precond_sgd(),
    burnin = 100,
    iterations = 120,
    n_batch = 10,
    seed = 1,
    start_sd = 0.01,
    trend_lim = 0.1,
    std_lim = 2
  )
)
nig_time <- Sys.time() - nig_time
fit_nig
traceplot(fit_nig)
saveRDS(fit_nig, "model-fits/wind-fit-nig.rds")
fit_nig <- readRDS("model-fits/wind-fit-nig.rds")


####### CV ########



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
  n_gibbs_samples = 200,
  print = TRUE,
  N_sim = 4,
  seed = 123,
  merge_groups = TRUE
)
saveRDS(cv, "model-fits/bvwind-fit-cv.rds")
cv <- readRDS("model-fits/bvwind-fit-cv.rds")
cv


# Print the mean scores in a pretty format
table <- cv$mean.scores
print(table)



# Save the final result to md
file_addr <- "wind/bvwind_result.md"

md_content <- c(
  "# Bivariate Wind Model Results",
  "",
  paste("**Date:**", Sys.Date()),
  "",
  "**Package Versions:**",
  paste("- **ngme2:**", as.character(packageVersion("ngme2"))),
  "",
  "## Gaussian Model",
  "```",
  paste(capture.output(summary(fit_gauss)), collapse = "\n"),
  "```",
  "**Fitting Time:**",
  paste("- **Gaussian:**", format(gauss_time)),
  "",
  "## NIG Model",
  "```",
  paste(capture.output(summary(fit_nig)), collapse = "\n"),
  "```",
  "**Fitting Time:**",
  paste("- **NIG:**", format(nig_time)),
  "",
  "## Cross Validation Results",
  "```",
  paste(capture.output(print(cv)), collapse = "\n"),
  "```"
)

writeLines(md_content, file_addr)
