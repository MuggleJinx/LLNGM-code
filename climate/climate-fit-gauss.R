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

fit_gauss <- ngme(
  Y ~ 0 + f(~ long + lat,
    model = matern(mesh = mesh),
    name = "matern",
    noise = noise_normal()
  ),
  replicate = ~year,
  data = df_10years,
  control_opt = control_opt(
    sampling_strategy = "ws",
    iterations = 500,
    optimizer = precond_sgd(),
    seed = 1,
    verbose = TRUE,
    pflug_alpha = 0.99
  )
)

summary(fit_gauss)
traceplot(fit_gauss, "matern")
traceplot(fit_gauss)

saveRDS(fit_gauss, file = "model-fits/climate-fit-gauss.rds")
