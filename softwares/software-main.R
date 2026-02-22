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
    Y ~ 0 + f(x,
      model = ar1(),
      prior = priors(
        rho = prior_normal(0, 2)
      ),
      noise = noise_nig(
        prior = priors(
          mu = prior_normal(0, 10),
          sigma = prior_normal(0, 2),
          nu = prior_normal(0, 2)
        )
      )
    ),
    family = noise_normal(
      prior = priors(
        sigma = prior_normal(0, 2)
      )
    ),
    data = data.frame(Y = Y, x = x),
    control_opt = control_opt(
      std_lim = 0.5,
      trend_lim = 0.05,
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
ngme2::traceplot(res, hline = c(0.8, 3, 2, 0.4, 1))


# generate posterior samples
time_ngme_samples <- system.time(
  ngme_samples <- compute_ngme_sgld_samples(
    fit = res,
    iterations = 500,
    optimizer = sgld(stepsize = 0.001),
    burnin = 10,
    n_parallel_chain = 8,
    alpha = 0.55,
    t0 = 0,
    start_sd = 0.01,
    burnin_iter = 0,
    seed = seed,
    verbose = TRUE,
    name = "all"
  )
)
time_ngme_samples
ngme_refit <- attr(ngme_samples, "refit")
ngme2::traceplot(ngme_refit)

ngme_ci <- ngme_sgld_ci(
  ngme_samples,
  lower = 0.025, upper = 0.975
)
# The posterior mean and 95% credible interval for the regression coefficients
ngme_ci$estimates
ngme_ci$ci


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
    init   = "random"
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


# Obtain the transformed 95 CI
posts <- as.data.frame(fit_mcmc)

posts$mu <- posts$mu
posts$sigma <- exp(posts$log_sigma)
posts$sigma_eps <- exp(posts$log_sigma_eps)
posts$nu <- exp(posts$log_nu)
posts$rho <- tanh(posts$rho_un)

mcmc_summary_tab <- data.frame(
  mean = colMeans(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")]),
  sd   = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, sd),
  q025 = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, quantile, 0.025),
  q975 = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, quantile, 0.975)
)
print(mcmc_summary_tab)





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


# Obtain the transformed 95 CI
posts <- as.data.frame(fit_random_WV)

posts$mu <- posts$mu
posts$sigma <- exp(posts$log_sigma)
posts$sigma_eps <- exp(posts$log_sigma_eps)
posts$nu <- exp(posts$log_nu)
posts$rho <- tanh(posts$rho_un)

tmbstan_summary_tab <- data.frame(
  mean = colMeans(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")]),
  sd   = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, sd),
  q025 = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, quantile, 0.025),
  q975 = apply(posts[, c("mu", "sigma", "sigma_eps", "nu", "rho")], 2, quantile, 0.975)
)
print(tmbstan_summary_tab)




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
r <- ngme_ci$estimates
ngme_noise <- noise_nig(mu = r["mu (field1)"], nu = r["nu (field1)"], sigma = r["sigma (field1)"])
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
  mu = r[["mu (field1)"]],
  sigma = r[["sigma (field1)"]],
  rho = r[["rho (field1)"]],
  sigma_eps = ngme_result(res, "data")[["sigma"]],
  nu = r[["nu (field1)"]]
)
ngme_est
stan_est
tmbstan_est

table <- rbind(true_est, ngme_est, stan_est, tmbstan_est)
table



# Save the final result
file_addr <- "software_result.md"

md_content <- c(
  "# Simulation Results",
  "",
  paste("**Date:**", Sys.Date()),
  "",
  "**Package Versions:**",
  paste("- **ngme2:**", as.character(packageVersion("ngme2"))),
  paste("- **TMB:**", as.character(packageVersion("TMB"))),
  paste("- **tmbstan:**", as.character(packageVersion("tmbstan"))),
  "",
  "## ngme",
  "### Execution Time (estimation)",
  "```",
  paste(capture.output(print(time_ngme)), collapse = "\n"),
  "```",
  "",
  "### Execution Time (sampling)",
  "```",
  paste(capture.output(print(time_ngme_samples)), collapse = "\n"),
  "```",
  "",
  "### Estimates",
  "```",
  paste(capture.output(print(ngme_ci$estimates)), collapse = "\n"),
  "```",
  "",
  "### Credible Intervals",
  "```",
  paste(capture.output(print(ngme_ci$ci)), collapse = "\n"),
  "```",
  "",
  "## MCMC",
  "### Execution Time",
  "```",
  paste(capture.output(print(time_stan)), collapse = "\n"),
  "```",
  "",
  "### Fit Summary",
  "```",
  paste(capture.output(print(fit_mcmc, pars = c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu"), digits = 3)), collapse = "\n"),
  "```",
  "",
  "### 95 CI",
  "```",
  paste(capture.output(print(mcmc_summary_tab)), collapse = "\n"),
  "```",
  "",
  "## TMBStan(Laplace)",
  "### Execution Time",
  "```",
  paste(capture.output(print(time_fit_random_WV)), collapse = "\n"),
  "```",
  "",
  "### Fit Summary",
  "```",
  paste(capture.output(print(fit_random_WV, pars = c("mu", "log_sigma", "rho_un", "log_sigma_eps", "log_nu"), digits = 3)), collapse = "\n"),
  "```",
  "",
  "### 95 CI",
  "```",
  paste(capture.output(print(tmbstan_summary_tab)), collapse = "\n"),
  "```",
  "",
  "## Comparison",
  "### Parameter Estimates",
  paste(capture.output(knitr::kable(table, format = "markdown", digits = 4)), collapse = "\n"),
  "",
  "### KLD Result",
  "```",
  paste(capture.output(print(result)), collapse = "\n"),
  "```"
)

writeLines(md_content, file_addr)
