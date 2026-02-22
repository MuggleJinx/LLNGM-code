set.seed(1)
load("climate/precip.data.Rdata")

library(ngme2)
library(fmesher)
library(Matrix)

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

basis.matrix <- function(x, y, xd, yd, n) {
  xs <- (x - min(xd)) / (max(xd) - min(xd))
  ys <- (y - min(yd)) / (max(yd) - min(yd))
  M <- matrix(1, length(x), 1)
  for (i in 1:n) {
    for (j in 1:n) {
      M <- cbind(M, sin(i * pi * xs) * sin(j * pi * ys))
      M <- cbind(M, cos(i * pi * xs) * cos(j * pi * ys))
      M <- cbind(M, sin(i * pi * xs) * cos(j * pi * ys))
      M <- cbind(M, cos(i * pi * xs) * sin(j * pi * ys))
    }
  }
  M
}

A.alt <- fmesher::fm_basis(mesh, loc = loc.alt)
alt.mesh <- as.vector(t(A.alt) %*% c(alt)) / colSums(A.alt)
alt.mesh[is.na(alt.mesh)] <- 0

B <- basis.matrix(mesh$loc[, 1], mesh$loc[, 2], loc[, 1], loc[, 2], 2)
B <- cbind(B, alt.mesh / 1000)
n_basis <- ncol(B)

dir.create("model-fits", showWarnings = FALSE, recursive = TRUE)

fit_nig_ns <- ngme(
  Y ~ 0 + f(~ long + lat,
    model = matern(
      mesh = mesh,
      B_kappa = B,
      theta_kappa = rep(0, n_basis)
    ),
    name = "matern",
    noise = noise_nig(
      B_mu = B,
      theta_mu = rep(0, n_basis),
      B_sigma = B,
      theta_sigma = rep(0, n_basis),
      B_nu = B,
      theta_nu = rep(0, n_basis),
      nu_lower_bound = 0.01
    )
  ),
  replicate = ~year,
  data = df_10years,
  control_opt = control_opt(
    sampling_strategy = "ws",
    n_parallel_chain = 4,
    iterations = 400,
    optimizer = precond_sgd(),
    seed = 1,
    pflug_alpha = 0.99,
    verbose = TRUE
  )
)

summary(fit_nig_ns)
traceplot(fit_nig_ns, "matern", combine = FALSE)
traceplot(fit_nig_ns, combine = FALSE)

saveRDS(fit_nig_ns, file = "model-fits/climate-fit-nig-ns.rds")
