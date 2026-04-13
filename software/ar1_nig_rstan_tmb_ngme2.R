# Compare the NIG-noise AR1 state-space model using ngme2, TMB, and rstan.
#
# This follows LLNGM-code/softwares/software-main.R, but replaces tmbstan with:
#   1. direct rstan NUTS on the full latent model,
#   2. direct TMB Laplace optimization over W and logV,
#   3. ngme2 optimization.
#
# Fast smoke run:
#   AR1_NIG_COMPARE_N=80 AR1_NIG_COMPARE_STAN_ITER=400 \
#   AR1_NIG_COMPARE_STAN_WARMUP=200 AR1_NIG_COMPARE_NGME_ITER=80 \
#   AR1_NIG_COMPARE_CHAINS=2 Rscript Code/software_compare/ar1_nig_rstan_tmb_ngme2.R

required_packages <- c("ngme2", "TMB", "rstan", "ggplot2", "knitr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(ngme2)
  library(TMB)
  library(rstan)
  library(ggplot2)
})

int_env <- function(name, default) {
  as.integer(Sys.getenv(name, unset = as.character(default)))
}

num_env <- function(name, default) {
  as.numeric(Sys.getenv(name, unset = as.character(default)))
}

bool_env <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  value %in% c("1", "true", "yes")
}

find_repo_root <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(path, "AGENTS.md")) && dir.exists(file.path(path, "Code"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      return(getwd())
    }
    path <- parent
  }
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[[1]]) else getwd()
script_dir <- if (file.exists(script_path)) dirname(normalizePath(script_path)) else getwd()
repo_root <- find_repo_root(script_dir)
output_dir <- file.path(repo_root, "Figures", "software_compare")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

config <- list(
  seed = int_env("AR1_NIG_COMPARE_SEED", 234L),
  n = int_env("AR1_NIG_COMPARE_N", 500L),
  mu = num_env("AR1_NIG_COMPARE_MU", 3),
  sigma = num_env("AR1_NIG_COMPARE_SIGMA", 2),
  rho = num_env("AR1_NIG_COMPARE_RHO", 0.8),
  sigma_eps = num_env("AR1_NIG_COMPARE_SIGMA_EPS", 1),
  nu = num_env("AR1_NIG_COMPARE_NU", 0.4),
  ngme_iterations = int_env("AR1_NIG_COMPARE_NGME_ITER", 400L),
  ngme_batches = int_env("AR1_NIG_COMPARE_NGME_BATCHES", 20L),
  stan_iter = int_env("AR1_NIG_COMPARE_STAN_ITER", 1000L),
  stan_warmup = int_env("AR1_NIG_COMPARE_STAN_WARMUP", 500L),
  stan_chains = int_env("AR1_NIG_COMPARE_CHAINS", 4L),
  stan_cores = int_env("AR1_NIG_COMPARE_CORES", parallel::detectCores(logical = FALSE)),
  stan_adapt_delta = num_env("AR1_NIG_COMPARE_ADAPT_DELTA", 0.95),
  stan_stepsize = num_env("AR1_NIG_COMPARE_STAN_STEPSIZE", 1e-4),
  stan_max_treedepth = int_env("AR1_NIG_COMPARE_STAN_MAX_TREEDEPTH", 12L)
)
config$stan_warmup <- min(config$stan_warmup, config$stan_iter - 1L)
if (is.na(config$stan_cores)) {
  config$stan_cores <- 1L
}
config$stan_cores <- min(config$stan_chains, max(1L, config$stan_cores))
if (config$ngme_batches < 1L) {
  config$ngme_batches <- 1L
}
if (config$ngme_iterations %% config$ngme_batches != 0L) {
  config$ngme_iterations <- ceiling(config$ngme_iterations / config$ngme_batches) *
    config$ngme_batches
}
config$stan_total_samples <- (config$stan_iter - config$stan_warmup) * config$stan_chains
config$sgld_chains <- int_env("AR1_NIG_COMPARE_SGLD_CHAINS", config$stan_chains)
config$sgld_total_samples <- int_env("AR1_NIG_COMPARE_SGLD_TOTAL", config$stan_total_samples)
config$sgld_stepsize <- num_env("AR1_NIG_COMPARE_SGLD_STEPSIZE", 0.05)
config$sgld_batch <- int_env("AR1_NIG_COMPARE_SGLD_BATCHES", 20L)
config$sgld_n_gibbs_samples <- int_env("AR1_NIG_COMPARE_SGLD_GIBBS", 20L)
config$sgld_alpha <- num_env("AR1_NIG_COMPARE_SGLD_ALPHA", 0.6)
config$sgld_t0 <- num_env("AR1_NIG_COMPARE_SGLD_T0", 0)
config$sgld_start_sd <- num_env("AR1_NIG_COMPARE_SGLD_START_SD", 0.01)
config$sgld_burnin <- int_env("AR1_NIG_COMPARE_SGLD_BURNIN", 0L)
config$sgld_burnin_iter <- int_env("AR1_NIG_COMPARE_SGLD_BURNIN_ITER", 1L)
config$sgld_verbose <- bool_env("AR1_NIG_COMPARE_SGLD_VERBOSE", FALSE)
if (is.na(config$sgld_chains) || config$sgld_chains < 1L) {
  config$sgld_chains <- config$stan_chains
}
if (is.na(config$sgld_batch) || config$sgld_batch < 1L) {
  config$sgld_batch <- 1L
}
if (config$sgld_total_samples %% config$sgld_chains != 0L) {
  config$sgld_total_samples <- ceiling(config$sgld_total_samples / config$sgld_chains) *
    config$sgld_chains
}
config$sgld_samples_per_chain <- as.integer(config$sgld_total_samples / config$sgld_chains)
config$sgld_iterations <- config$sgld_samples_per_chain + config$sgld_burnin_iter - 1L
if (config$sgld_iterations < 1L) {
  config$sgld_iterations <- 1L
}
if (config$sgld_iterations %% config$sgld_batch != 0L) {
  config$sgld_batch <- 1L
}

first_named_value <- function(x, candidates) {
  for (candidate in candidates) {
    if (!is.null(x[[candidate]])) {
      return(as.numeric(x[[candidate]]))
    }
  }
  x_names <- names(x)
  for (candidate in candidates) {
    hit <- grep(paste0("^", candidate, "( |$|\\()"), x_names)
    if (length(hit) > 0) {
      return(as.numeric(x[[hit[[1]]]]))
    }
  }
  NA_real_
}

summarize_draws <- function(draws, parameters) {
  out <- data.frame(
    parameter = parameters,
    mean = NA_real_,
    sd = NA_real_,
    q025 = NA_real_,
    q975 = NA_real_
  )
  for (i in seq_along(parameters)) {
    values <- draws[[parameters[[i]]]]
    out$mean[i] <- mean(values)
    out$sd[i] <- stats::sd(values)
    out$q025[i] <- stats::quantile(values, 0.025)
    out$q975[i] <- stats::quantile(values, 0.975)
  }
  out
}

add_interval_method <- function(intervals, method) {
  intervals$method <- method
  intervals[, c("method", "parameter", "mean", "sd", "q025", "q975")]
}

summarize_tmb_intervals <- function(tmb_fit, parameters) {
  report_summary <- tmb_fit$summary
  out <- data.frame(
    parameter = parameters,
    mean = NA_real_,
    sd = NA_real_,
    q025 = NA_real_,
    q975 = NA_real_
  )

  for (i in seq_along(parameters)) {
    parameter <- parameters[[i]]
    row_id <- which(report_summary$parameter == parameter)
    if (length(row_id) > 0) {
      estimate <- report_summary$Estimate[row_id[[1]]]
      se <- report_summary$`Std. Error`[row_id[[1]]]
    } else {
      estimate <- tmb_fit$estimates[[parameter]]
      se <- NA_real_
    }

    out$mean[i] <- estimate
    out$sd[i] <- se
    out$q025[i] <- estimate + stats::qnorm(0.025) * se
    out$q975[i] <- estimate + stats::qnorm(0.975) * se
  }
  out
}

build_interval_table <- function(ngme_sampling, tmb_fit, stan_fit, parameters) {
  rbind(
    add_interval_method(summarize_draws(ngme_sampling$posts, parameters), "ngme2 SGLD"),
    add_interval_method(summarize_tmb_intervals(tmb_fit, parameters), "TMB"),
    add_interval_method(stan_fit$summary, "rstan")
  )
}

extract_ngme_posts <- function(samples) {
  samples <- as.data.frame(samples)
  column_for <- function(candidates) {
    for (candidate in candidates) {
      if (candidate %in% colnames(samples)) {
        return(candidate)
      }
    }
    for (candidate in candidates) {
      hit <- grep(paste0("^", candidate, "( |$|\\()"), colnames(samples), value = TRUE)
      if (length(hit) > 0) {
        return(hit[[1]])
      }
    }
    stop("Could not find posterior sample column for: ", paste(candidates, collapse = ", "))
  }

  columns <- c(
    mu = column_for(c("mu (ar1_field)", "mu (field1)", "mu")),
    sigma = column_for(c("sigma (ar1_field)", "sigma (field1)")),
    rho = column_for(c("rho (ar1_field)", "rho (field1)", "rho")),
    sigma_eps = column_for(c("sigma (data)", "sigma")),
    nu = column_for(c("nu (ar1_field)", "nu (field1)", "nu"))
  )
  posts <- samples[, columns, drop = FALSE]
  colnames(posts) <- names(columns)
  posts
}

posterior_to_long <- function(posts, method) {
  posts <- as.data.frame(posts)
  do.call(
    rbind,
    lapply(
      colnames(posts),
      function(parameter) {
        data.frame(
          method = method,
          parameter = parameter,
          value = posts[[parameter]],
          row.names = NULL
        )
      }
    )
  )
}

save_posterior_compare_plot <- function(ngme_posts, stan_posts, truth, path) {
  plot_data <- rbind(
    posterior_to_long(ngme_posts, "ngme2 SGLD"),
    posterior_to_long(stan_posts, "rstan")
  )
  truth_data <- data.frame(
    parameter = names(truth),
    true_value = as.numeric(truth),
    row.names = NULL
  )
  plot <- ggplot(plot_data, aes(x = value, color = method, fill = method)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    geom_vline(
      data = truth_data,
      aes(xintercept = true_value),
      color = "black",
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    facet_wrap(~parameter, scales = "free", ncol = 3) +
    scale_color_manual(values = c("ngme2 SGLD" = "#1b9e77", "rstan" = "#d95f02")) +
    scale_fill_manual(values = c("ngme2 SGLD" = "#1b9e77", "rstan" = "#d95f02")) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top") +
    labs(
      title = "Posterior Samples: ngme2 SGLD vs rstan",
      x = "Value",
      y = "Density",
      color = "Method",
      fill = "Method"
    )
  ggplot2::ggsave(path, plot, width = 12, height = 8, dpi = 150)
  plot
}

simulate_nig_ar1 <- function(config) {
  set.seed(config$seed)
  true_noise <- noise_nig(mu = config$mu, sigma = config$sigma, nu = config$nu)
  ar1_model <- f(
    seq_len(config$n),
    model = ar1(rho = config$rho),
    noise = true_noise
  )
  w <- simulate(ar1_model, seed = config$seed)[[1]]
  y <- w + rnorm(config$n, mean = 0, sd = config$sigma_eps)
  list(x = seq_len(config$n), W = w, Y = y, true_noise = true_noise)
}

save_data_plot <- function(data, path) {
  png(path, width = 1000, height = 520)
  on.exit(dev.off(), add = TRUE)
  plot(
    data$x, data$Y,
    type = "l",
    col = "gray35",
    xlab = "time",
    ylab = "value",
    main = "NIG AR1 Simulation"
  )
  lines(data$x, data$W, col = "steelblue", lwd = 2)
  legend(
    "topright",
    legend = c("Y", "latent W"),
    col = c("gray35", "steelblue"),
    lty = 1,
    lwd = c(1, 2),
    bty = "n"
  )
}

fit_with_ngme2 <- function(y, x, config) {
  time <- system.time({
    fit <- ngme(
      y ~ 0 + f(
        x,
        name = "ar1_field",
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
      data = data.frame(y = y, x = x),
      control_opt = control_opt(
        std_lim = 0.5,
        trend_lim = 0.05,
        print_check_info = TRUE,
        seed = config$seed,
        n_batch = config$ngme_batches,
        iterations = config$ngme_iterations,
        optimizer = precond_sgd(),
        rao_blackwellization = TRUE
      )
    )
  })

  field_result <- ngme_result(fit, "ar1_field")
  data_result <- ngme_result(fit, "data")
  estimates <- c(
    mu = first_named_value(field_result, "mu"),
    sigma = first_named_value(field_result, "sigma"),
    rho = first_named_value(field_result, "rho"),
    sigma_eps = first_named_value(data_result, "sigma"),
    nu = first_named_value(field_result, "nu")
  )

  trace_path <- file.path(output_dir, "traceplot_ngme2_ar1_nig.png")
  try(
    {
      trace <- ngme2::traceplot(
        fit,
        hline = c(config$rho, config$mu, config$sigma, config$nu, config$sigma_eps),
        ncol = 3
      )
      ggplot2::ggsave(trace_path, trace, width = 12, height = 8, dpi = 150)
    },
    silent = TRUE
  )

  list(
    fit = fit,
    time = time,
    estimates = estimates,
    field_result = field_result,
    data_result = data_result
  )
}

sample_with_ngme2 <- function(fit, config) {
  time <- system.time({
    samples <- compute_ngme_sgld_samples(
      fit = fit,
      iterations = config$sgld_iterations,
      optimizer = sgld(stepsize = config$sgld_stepsize),
      burnin = config$sgld_burnin,
      n_batch = config$sgld_batch,
      n_parallel_chain = config$sgld_chains,
      n_gibbs_samples = config$sgld_n_gibbs_samples,
      alpha = config$sgld_alpha,
      t0 = config$sgld_t0,
      start_sd = config$sgld_start_sd,
      burnin_iter = config$sgld_burnin_iter,
      seed = config$seed + 10000L,
      verbose = config$sgld_verbose,
      name = "all"
    )
  })
  ci <- ngme_sgld_ci(samples, lower = 0.025, upper = 0.975)
  posts <- extract_ngme_posts(ci$samples)
  if (nrow(posts) != config$sgld_total_samples) {
    warning(
      "ngme2 SGLD returned ", nrow(posts), " samples, expected ",
      config$sgld_total_samples, ".",
      call. = FALSE
    )
  }
  estimates <- stats::setNames(colMeans(posts), colnames(posts))

  list(
    samples = samples,
    ci = ci,
    posts = posts,
    time = time,
    estimates = estimates,
    n_samples = nrow(posts)
  )
}

tmb_cpp_code <- function() {
  paste(
    "#include <TMB.hpp>",
    "",
    "template <class Type>",
    "Type invgauss_lpdf(Type x, Type a, Type b) {",
    "  return Type(0.5) * log(b) - Type(0.5) * log(Type(2.0 * M_PI)) -",
    "    Type(1.5) * log(x) - (a * x) / Type(2.0) - b / (Type(2.0) * x) +",
    "    sqrt(a * b);",
    "}",
    "",
    "template <class Type>",
    "Type objective_function<Type>::operator()() {",
    "  DATA_VECTOR(y);",
    "  DATA_INTEGER(n);",
    "",
    "  PARAMETER(mu);",
    "  PARAMETER(log_sigma);",
    "  PARAMETER(rho_un);",
    "  PARAMETER(log_sigma_eps);",
    "  PARAMETER(log_nu);",
    "  PARAMETER_VECTOR(W);",
    "  PARAMETER_VECTOR(logV);",
    "",
    "  Type rho = tanh(rho_un);",
    "  Type sigma = exp(log_sigma);",
    "  Type sigma_eps = exp(log_sigma_eps);",
    "  Type nu = exp(log_nu);",
    "  vector<Type> V = exp(logV);",
    "",
    "  vector<Type> KW(n);",
    "  KW(0) = sqrt(Type(1) - rho * rho) * W(0);",
    "  for (int i = 1; i < n; ++i) {",
    "    KW(i) = W(i) - rho * W(i - 1);",
    "  }",
    "",
    "  Type nll = 0.0;",
    "  for (int i = 0; i < n; ++i) {",
    "    nll -= invgauss_lpdf(V(i), nu, nu);",
    "    nll -= logV(i);",
    "  }",
    "  for (int i = 0; i < n; ++i) {",
    "    nll -= dnorm(KW(i), -mu + mu * V(i), sigma * sqrt(V(i)), true);",
    "  }",
    "  for (int i = 0; i < n; ++i) {",
    "    nll -= dnorm(y(i), W(i), sigma_eps, true);",
    "  }",
    "",
    "  nll -= dnorm(mu, Type(0), Type(10), true);",
    "  nll -= dnorm(log_sigma, Type(0), Type(2), true);",
    "  nll -= dnorm(rho_un, Type(0), Type(2), true);",
    "  nll -= dnorm(log_sigma_eps, Type(0), Type(2), true);",
    "  nll -= dnorm(log_nu, Type(0), Type(2), true);",
    "",
    "  ADREPORT(rho);",
    "  ADREPORT(sigma);",
    "  ADREPORT(sigma_eps);",
    "  ADREPORT(nu);",
    "  REPORT(rho);",
    "  REPORT(sigma);",
    "  REPORT(sigma_eps);",
    "  REPORT(nu);",
    "",
    "  return nll;",
    "}",
    sep = "\n"
  )
}

fit_with_tmb <- function(y) {
  tmb_dir <- tempfile("ar1_nig_tmb_")
  dir.create(tmb_dir, recursive = TRUE, showWarnings = FALSE)
  cpp_path <- file.path(tmb_dir, "ar1_nig_tmb.cpp")
  writeLines(tmb_cpp_code(), cpp_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmb_dir)
  TMB::compile(basename(cpp_path))
  dyn.load(TMB::dynlib("ar1_nig_tmb"))

  data_list <- list(y = as.numeric(y), n = length(y))
  par_list <- list(
    mu = 0,
    log_sigma = 0,
    rho_un = atanh(0.5),
    log_sigma_eps = 0,
    log_nu = log(0.5),
    W = as.numeric(y),
    logV = rep(0, length(y))
  )
  obj <- TMB::MakeADFun(
    data = data_list,
    parameters = par_list,
    random = c("W", "logV"),
    DLL = "ar1_nig_tmb",
    silent = TRUE
  )

  time <- system.time({
    opt <- nlminb(
      start = obj$par,
      objective = obj$fn,
      gradient = obj$gr,
      control = list(eval.max = 10000, iter.max = 10000)
    )
  })
  sd_report <- TMB::sdreport(obj)
  report_summary <- as.data.frame(summary(sd_report))
  report_summary$parameter <- rownames(report_summary)

  fixed <- opt$par
  estimates <- c(
    mu = fixed[["mu"]],
    sigma = exp(fixed[["log_sigma"]]),
    rho = tanh(fixed[["rho_un"]]),
    sigma_eps = exp(fixed[["log_sigma_eps"]]),
    nu = exp(fixed[["log_nu"]])
  )

  list(
    opt = opt,
    sd_report = sd_report,
    summary = report_summary,
    time = time,
    estimates = estimates
  )
}

stan_code <- function() {
  paste(
    "functions {",
    "  // Log-density for log(V), including the Jacobian from V = exp(log(V)).",
    "  real invgauss_logv_lpdf_val(real log_x, real a, real b) {",
    "    return 0.5 * log(b) - 0.5 * log(2 * pi()) - 0.5 * log_x -",
    "      (a * exp(log_x)) / 2 - (b * exp(-log_x)) / 2 + sqrt(a * b);",
    "  }",
    "}",
    "data {",
    "  int<lower=1> n;",
    "  vector[n] y;",
    "}",
    "parameters {",
    "  real mu;",
    "  real log_sigma;",
    "  real rho_un;",
    "  real log_sigma_eps;",
    "  real log_nu;",
    "  vector[n] W;",
    "  vector<lower=-20, upper=20>[n] logV;",
    "}",
    "transformed parameters {",
    "  real<lower=-0.999, upper=0.999> rho;",
    "  real<lower=0> sigma;",
    "  real<lower=0> sigma_eps;",
    "  real<lower=0> nu;",
    "  vector[n] V;",
    "",
    "  rho = 0.999 * tanh(rho_un);",
    "  sigma = exp(log_sigma);",
    "  sigma_eps = exp(log_sigma_eps);",
    "  nu = exp(log_nu);",
    "  V = exp(logV);",
    "}",
    "model {",
    "  mu ~ normal(0, 10);",
    "  log_sigma ~ normal(0, 2);",
    "  rho_un ~ normal(0, 2);",
    "  log_sigma_eps ~ normal(0, 2);",
    "  log_nu ~ normal(0, 2);",
    "  logV ~ normal(0, 3);",
    "",
    "  for (i in 1:n) {",
    "    target += invgauss_logv_lpdf_val(logV[i], nu, nu);",
    "  }",
    "  W[1] ~ normal(",
    "    (-mu + mu * V[1]) / sqrt(1 - square(rho)),",
    "    sigma * sqrt(V[1]) / sqrt(1 - square(rho))",
    "  );",
    "  if (n > 1) {",
    "    for (i in 2:n) {",
    "      W[i] ~ normal(rho * W[i - 1] - mu + mu * V[i], sigma * sqrt(V[i]));",
    "    }",
    "  }",
    "  y ~ normal(W, sigma_eps);",
    "}",
    sep = "\n"
  )
}

fit_with_rstan <- function(y, config) {
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = config$stan_cores)

  model <- rstan::stan_model(model_code = stan_code())
  set.seed(config$seed + 20000L)
  init_fun <- function() {
    list(
      mu = config$mu + rnorm(1, 0, 0.1),
      log_sigma = log(config$sigma) + rnorm(1, 0, 0.1),
      rho_un = atanh(config$rho) + rnorm(1, 0, 0.05),
      log_sigma_eps = log(config$sigma_eps) + rnorm(1, 0, 0.1),
      log_nu = log(config$nu) + rnorm(1, 0, 0.1),
      W = as.numeric(y),
      logV = rep(0, length(y))
    )
  }

  time <- system.time({
    fit <- rstan::sampling(
      object = model,
      data = list(n = length(y), y = as.numeric(y)),
      chains = config$stan_chains,
      iter = config$stan_iter,
      warmup = config$stan_warmup,
      cores = config$stan_cores,
      seed = config$seed,
      init = init_fun,
      control = list(
        adapt_delta = config$stan_adapt_delta,
        stepsize = config$stan_stepsize,
        max_treedepth = config$stan_max_treedepth
      )
    )
  })

  draws <- as.data.frame(rstan::extract(
    fit,
    pars = c("mu", "sigma", "rho", "sigma_eps", "nu"),
    permuted = TRUE
  ))
  if (nrow(draws) == 0L) {
    stop(
      "rstan sampling returned zero posterior samples. ",
      "Run with AR1_NIG_COMPARE_CHAINS=1 to inspect the sampler error.",
      call. = FALSE
    )
  }
  summary_table <- summarize_draws(draws, c("mu", "sigma", "rho", "sigma_eps", "nu"))
  estimates <- stats::setNames(summary_table$mean, summary_table$parameter)

  trace_path <- file.path(output_dir, "traceplot_rstan_ar1_nig.png")
  try(
    {
      trace <- rstan::traceplot(
        fit,
        pars = c("mu", "sigma", "rho", "sigma_eps", "nu"),
        inc_warmup = FALSE
      )
      trace <- trace +
        geom_hline(
          data = data.frame(
            parameter = c("mu", "sigma", "rho", "sigma_eps", "nu"),
            true_value = c(config$mu, config$sigma, config$rho, config$sigma_eps, config$nu)
          ),
          aes(yintercept = true_value),
          color = "steelblue"
        )
      ggplot2::ggsave(trace_path, trace, width = 12, height = 8, dpi = 150)
    },
    silent = TRUE
  )

  list(
    fit = fit,
    time = time,
    estimates = estimates,
    summary = summary_table,
    samples = draws,
    n_samples = nrow(draws)
  )
}

safe_noise_kld <- function(data, truth, estimate_table) {
  result <- try(
    {
      noises <- list(true = data$true_noise)
      for (method in rownames(estimate_table)[rownames(estimate_table) != "true"]) {
        row <- estimate_table[method, ]
        noises[[method]] <- noise_nig(mu = row[["mu"]], sigma = row[["sigma"]], nu = row[["nu"]])
      }
      do.call(compare_noise_kld, noises)
    },
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  result
}

save_noise_plot <- function(data, estimate_table, path) {
  try(
    {
      noises <- list(true = data$true_noise)
      for (method in rownames(estimate_table)[rownames(estimate_table) != "true"]) {
        row <- estimate_table[method, ]
        noises[[method]] <- noise_nig(mu = row[["mu"]], sigma = row[["sigma"]], nu = row[["nu"]])
      }
      png(path, width = 900, height = 650)
      do.call(plot, c(noises, list(xlim = c(-5, 8))))
      dev.off()
    },
    silent = TRUE
  )
}

write_results <- function(
    output_file,
    config,
    truth,
    estimate_table,
    error_table,
    interval_table,
    times,
    ngme_fit,
    ngme_sampling,
    tmb_fit,
    stan_fit,
    kld_result,
    posterior_plot_path) {
  package_versions <- vapply(
    required_packages,
    function(pkg) as.character(utils::packageVersion(pkg)),
    character(1)
  )

  content <- c(
    "# NIG AR1 Software Comparison",
    "",
    paste("**Date:**", Sys.Date()),
    "",
    "## Configuration",
    "```",
    paste(capture.output(str(config)), collapse = "\n"),
    "```",
    "",
    "## Package Versions",
    paste("- **", names(package_versions), ":** ", package_versions, sep = ""),
    "",
    "## True Parameters",
    "```",
    paste(capture.output(print(truth)), collapse = "\n"),
    "```",
    "",
    "## Execution Time",
    knitr::kable(times, format = "markdown", digits = 3),
    "",
    "## Posterior Sample Counts",
    knitr::kable(
      data.frame(
        method = c("ngme2 SGLD", "rstan"),
        samples = c(ngme_sampling$n_samples, stan_fit$n_samples)
      ),
      format = "markdown"
    ),
    "",
    "## Point Estimates",
    knitr::kable(estimate_table, format = "markdown", digits = 4),
    "",
    "## Estimate Error",
    knitr::kable(error_table, format = "markdown", digits = 4),
    "",
    "## Posterior Intervals",
    knitr::kable(interval_table, format = "markdown", digits = 4),
    "",
    "TMB intervals use the delta-method normal approximation from `sdreport()`.",
    "",
    "## ngme2 Summary",
    "```",
    paste(capture.output(print(summary(ngme_fit$fit))), collapse = "\n"),
    "```",
    "",
    "### ngme2 SGLD Runtime",
    "```",
    paste(capture.output(print(ngme_sampling$time)), collapse = "\n"),
    "```",
    "",
    "## TMB Optimizer",
    "```",
    paste(capture.output(print(tmb_fit$opt)), collapse = "\n"),
    "```",
    "",
    "## TMB Delta-Method Summary",
    "```",
    paste(capture.output(print(tmb_fit$summary)), collapse = "\n"),
    "```",
    "",
    "## rstan Summary",
    "```",
    paste(
      capture.output(
        print(
          stan_fit$fit,
          pars = c("mu", "sigma", "rho", "sigma_eps", "nu"),
          digits = 3
        )
      ),
      collapse = "\n"
    ),
    "```",
    "",
    "## Posterior Comparison Plot",
    paste("Saved to:", posterior_plot_path)
  )
  if (!is.null(kld_result)) {
    content <- c(
      content,
      "",
      "## NIG Noise KLD",
      "```",
      paste(capture.output(print(kld_result)), collapse = "\n"),
      "```"
    )
  }

  writeLines(content, output_file)
}

run_comparison <- function(config) {
  data <- simulate_nig_ar1(config)
  save_data_plot(data, file.path(output_dir, "ar1_nig_data.png"))

  cat("Fitting NIG AR1 with ngme2...\n")
  ngme_fit <- fit_with_ngme2(data$Y, data$x, config)

  cat("Sampling NIG AR1 posterior with ngme2 SGLD...\n")
  ngme_sampling <- sample_with_ngme2(ngme_fit$fit, config)

  cat("Fitting NIG AR1 with TMB...\n")
  tmb_fit <- fit_with_tmb(data$Y)

  cat("Fitting NIG AR1 with rstan...\n")
  stan_fit <- fit_with_rstan(data$Y, config)

  truth <- c(
    mu = config$mu,
    sigma = config$sigma,
    rho = config$rho,
    sigma_eps = config$sigma_eps,
    nu = config$nu
  )
  estimate_table <- rbind(
    true = truth,
    ngme2 = ngme_sampling$estimates[names(truth)],
    TMB = tmb_fit$estimates[names(truth)],
    rstan = stan_fit$estimates[names(truth)]
  )
  error_table <- sweep(estimate_table[-1, , drop = FALSE], 2, truth, "-")
  interval_table <- build_interval_table(
    ngme_sampling = ngme_sampling,
    tmb_fit = tmb_fit,
    stan_fit = stan_fit,
    parameters = names(truth)
  )
  times <- data.frame(
    method = c("ngme2 fit", "ngme2 SGLD", "ngme2 total", "TMB", "rstan"),
    user = c(
      ngme_fit$time[["user.self"]],
      ngme_sampling$time[["user.self"]],
      ngme_fit$time[["user.self"]] + ngme_sampling$time[["user.self"]],
      tmb_fit$time[["user.self"]],
      stan_fit$time[["user.self"]]
    ),
    system = c(
      ngme_fit$time[["sys.self"]],
      ngme_sampling$time[["sys.self"]],
      ngme_fit$time[["sys.self"]] + ngme_sampling$time[["sys.self"]],
      tmb_fit$time[["sys.self"]],
      stan_fit$time[["sys.self"]]
    ),
    elapsed = c(
      ngme_fit$time[["elapsed"]],
      ngme_sampling$time[["elapsed"]],
      ngme_fit$time[["elapsed"]] + ngme_sampling$time[["elapsed"]],
      tmb_fit$time[["elapsed"]],
      stan_fit$time[["elapsed"]]
    )
  )
  kld_result <- safe_noise_kld(data, truth, estimate_table)
  save_noise_plot(data, estimate_table, file.path(output_dir, "ar1_nig_noise_compare.png"))
  posterior_plot_path <- file.path(output_dir, "posterior_samples_ngme2_sgld_vs_rstan.png")
  save_posterior_compare_plot(ngme_sampling$posts, stan_fit$samples, truth, posterior_plot_path)

  print(round(estimate_table, 4))
  print(round(error_table, 4))
  print(knitr::kable(interval_table, format = "simple", digits = 4))
  print(times)
  cat("ngme2 SGLD samples:", ngme_sampling$n_samples, "\n")
  cat("rstan samples:", stan_fit$n_samples, "\n")

  result_file <- file.path(repo_root, "Code", "software_compare", "ar1_nig_rstan_tmb_ngme2_result.md")
  write_results(
    output_file = result_file,
    config = config,
    truth = truth,
    estimate_table = estimate_table,
    error_table = error_table,
    interval_table = interval_table,
    times = times,
    ngme_fit = ngme_fit,
    ngme_sampling = ngme_sampling,
    tmb_fit = tmb_fit,
    stan_fit = stan_fit,
    kld_result = kld_result,
    posterior_plot_path = posterior_plot_path
  )

  cat("Saved result markdown to:", result_file, "\n")
  cat("Saved figures to:", output_dir, "\n")

  invisible(list(
    data = data,
    ngme_fit = ngme_fit,
    ngme_sampling = ngme_sampling,
    tmb_fit = tmb_fit,
    stan_fit = stan_fit,
    estimate_table = estimate_table,
    error_table = error_table,
    interval_table = interval_table,
    times = times,
    kld_result = kld_result
  ))
}

skip_run <- tolower(Sys.getenv("AR1_NIG_COMPARE_SKIP_RUN", unset = "false")) %in% c("1", "true", "yes")
if (!skip_run) {
  run_comparison(config)
}
