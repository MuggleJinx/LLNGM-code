set.seed(1)
load("climate/precip.data.Rdata")

library(ngme2)
library(fmesher)

mesh <- fmesher::fm_mesh_2d(
  loc = loc,
  max.edge = c(6, 8),
  offset = c(-0.05, -0.1),
  min.angle = 21
)

Y_flat <- as.vector(Y)
year <- rep(seq_len(ncol(Y)), each = nrow(Y))
long <- rep(loc[, 1], ncol(Y))
lat <- rep(loc[, 2], ncol(Y))
df <- data.frame(Y = Y_flat, year = year, long = long, lat = lat)
df_10years <- df[df$year %in% 1:10, ]

dir.create("model-fits", showWarnings = FALSE, recursive = TRUE)

fit_nig <- ngme(
  Y ~ 0 + f(~ long + lat,
    model = matern(mesh = mesh),
    name = "matern",
    noise = noise_nig()
  ),
  replicate = ~year,
  data = df_10years,
  control_opt = control_opt(
    print_check_info = TRUE,
    sampling_strategy = "ws",
    iterations = 500,
    pflug_alpha = 0.99,
    optimizer = precond_sgd(),
    max_num_threads = 20,
    seed = 1,
    verbose = TRUE
  )
)

summary(fit_nig)
traceplot(fit_nig, "matern")
traceplot(fit_nig)

saveRDS(fit_nig, file = "model-fits/climate-fit-nig.rds")
