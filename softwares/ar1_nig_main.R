seed <- 235
set.seed(seed)
library(ngme2)

true_noise <- noise_nig(mu = 3, sigma = 2, nu = 0.4)
ar1 <- f(1:500, model = ar1(rho = 0.8), noise = true_noise)
W <- simulate(ar1, seed = seed)[[1]]
Y <- W + rnorm(500, 0, 1)
plot(W, type = "l")


######## Ngme ########
x <- 1:500
time_ngme <- system.time(
  res <- ngme(
    Y ~ 0 + f(x, model = ar1(), noise = noise_nig()),
    data = data.frame(Y = Y, x = x),
    control_opt = control_opt(
      print_check_info = TRUE,
      seed = seed,
      n_batch = 20,
      iterations = 2000,
      optimizer = precond_sgd(),
      rao_blackwellization = TRUE
    )
  )
)
time_ngme
summary(res)
ngme2::traceplot(res, "field1", hline = c(0.8, 3, 2, 0.4))


######## Stan MCMC ########
library(TMB)
library(tmbstan)

setwd("softwares")
writeLines(readLines("ar1_nig.cpp"), "ar1_nig.cpp")
compile("ar1_nig.cpp")
dyn.load(dynlib("ar1_nig"))

n <- length(Y)
data_list <- list(y = Y, n = n)
par_list <- list(
  mu             = 0,
  log_sigma      = 0,
  rho_un         = 0,
  log_sigma_eps  = 0,
  log_nu         = 0,
  W              = rep(0, n),
  logV           = rep(0, n)
)

obj <- MakeADFun(data_list, par_list, DLL = "ar1_nig")

time_stan <- system.time(
  fit_mcmc <- tmbstan(
    obj,
    chains = 4,
    iter   = 2000,
    warmup = 1000,
    cores  = 4,
    seed   = seed,
    init   = "random" # 也可以给定 par_list
  )
)
time_stan

print(fit_mcmc, pars = c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu"), digits = 3)
# Access the posterior means of all parameters:
samples_stan <- as.matrix(fit_mcmc)
param_names <- c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu")
posterior_means <- sapply(param_names, function(p) mean(samples_stan[, p]))
names(posterior_means) <- param_names
cat("Posterior means (TMBStan):\n")
for (p in param_names) {
  cat(p, "=", round(posterior_means[p], 3), "\n")
}
stan_est <- c(
  mu         = posterior_means["mu"],
  sigma      = exp(posterior_means["log_sigma"]),
  rho        = tanh(posterior_means["rho_un"]),
  sigma_eps  = exp(posterior_means["log_sigma_eps"]),
  nu         = exp(posterior_means["log_nu"])
)
names(stan_est) <- c("mu", "sigma", "rho", "sigma_eps", "nu")
stan_est


######## TMBStan ########
obj_random_WV <- MakeADFun(data_list, par_list, DLL = "ar1_nig", random = c("W", "logV"))

time_fit_random_WV <- system.time(
  fit_random_WV <- tmbstan(
    obj_random_WV,
    laplace = TRUE,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = seed,
    init = "random" # 也可以给定 par_list
  )
)
time_fit_random_WV

print(fit_random_WV, pars = c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu"), digits = 3)

# Access the posterior means of all parameters:
samples_random_WV <- as.matrix(fit_random_WV)
param_names <- c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu")
posterior_means <- sapply(param_names, function(p) mean(samples_random_WV[, p]))
names(posterior_means) <- param_names
cat("Posterior means (TMBStan):\n")
for (p in param_names) {
  cat(p, "=", round(posterior_means[p], 3), "\n")
}
tmbstan_est <- c(
  mu         = posterior_means["mu"],
  sigma      = exp(posterior_means["log_sigma"]),
  rho        = tanh(posterior_means["rho_un"]),
  sigma_eps  = exp(posterior_means["log_sigma_eps"]),
  nu         = exp(posterior_means["log_nu"])
)
names(tmbstan_est) <- c("mu", "sigma", "rho", "sigma_eps", "nu")
tmbstan_est

######## NGVB failed to run ########
# library(INLA)
# library(ngvb)
# x <- 1:500
# formula <- y ~ -1 + f(x,  model = "ar1")
# LGM     <- inla(formula,
#                 data = data_list,
#                 control.compute = list(config = TRUE))

# LnGM <- ngvb(fit = LGM)



######## Compare ########
r <- ngme_result(res, "field1")
ngme_noise <- noise_nig(mu = r$mu, nu = r$nu, sigma = r$sigma)

stan_noise <- noise_nig(mu = stan_est["mu"], sigma = stan_est["sigma"], nu = stan_est["nu"])
tmbstan_noise <- noise_nig(mu = tmbstan_est["mu"], sigma = tmbstan_est["sigma"], nu = tmbstan_est["nu"])

plot(
  true = true_noise,
  ngme = ngme_noise,
  stan = stan_noise,
  tmb = tmbstan_noise,
  xlim = c(-5, 5)
)

result <- compare_noise_kld(true = true_noise, ngme = ngme_noise, stan = stan_noise, tmb = tmbstan_noise)
result


true_est <- c(mu = 3, sigma = 2, rho = 0.8, sigma_eps = 1, nu = 0.4)
ngme_est <- c(
  mu = r$mu,
  sigma = r$sigma,
  rho = r$rho,
  sigma_eps = ngme_result(res, "data")$sigma,
  nu = r$nu
)
ngme_est
stan_est
tmbstan_est

table <- rbind(true_est, ngme_est, stan_est, tmbstan_est)
table
