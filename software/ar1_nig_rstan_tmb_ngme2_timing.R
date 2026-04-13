# Runtime-only benchmark for ar1_nig_rstan_tmb_ngme2.R.
#
# Default run:
#   Rscript Code/software_compare/ar1_nig_rstan_tmb_ngme2_timing.R
#
# Short smoke run:
#   AR1_NIG_TIMING_REPS=1 AR1_NIG_COMPARE_N=80 AR1_NIG_COMPARE_STAN_ITER=400 \
#   AR1_NIG_COMPARE_STAN_WARMUP=200 AR1_NIG_COMPARE_NGME_ITER=80 \
#   AR1_NIG_COMPARE_CHAINS=2 Rscript Code/software_compare/ar1_nig_rstan_tmb_ngme2_timing.R

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x)) y else x
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
this_file <- if (length(script_arg) > 0L) {
  sub("^--file=", "", script_arg[[1]])
} else {
  sys.frame(1)$ofile %||% getwd()
}
script_dir <- if (file.exists(this_file)) dirname(normalizePath(this_file)) else getwd()
main_script <- file.path(script_dir, "ar1_nig_rstan_tmb_ngme2.R")
if (!file.exists(main_script)) {
  main_script <- file.path(getwd(), "Code", "software_compare", "ar1_nig_rstan_tmb_ngme2.R")
}
if (!file.exists(main_script)) {
  stop("Cannot find ar1_nig_rstan_tmb_ngme2.R", call. = FALSE)
}

int_env_local <- function(name, default) {
  as.integer(Sys.getenv(name, unset = as.character(default)))
}

bool_env_local <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  value %in% c("1", "true", "yes")
}

old_skip_run <- Sys.getenv("AR1_NIG_COMPARE_SKIP_RUN", unset = NA_character_)
Sys.setenv(AR1_NIG_COMPARE_SKIP_RUN = "true")
source(main_script)
if (is.na(old_skip_run)) {
  Sys.unsetenv("AR1_NIG_COMPARE_SKIP_RUN")
} else {
  Sys.setenv(AR1_NIG_COMPARE_SKIP_RUN = old_skip_run)
}

n_reps <- int_env_local("AR1_NIG_TIMING_REPS", 10L)
if (is.na(n_reps) || n_reps < 1L) {
  stop("AR1_NIG_TIMING_REPS must be a positive integer.", call. = FALSE)
}

vary_seed <- bool_env_local("AR1_NIG_TIMING_VARY_SEED", FALSE)
verbose <- bool_env_local("AR1_NIG_TIMING_VERBOSE", FALSE)
timing_dir <- Sys.getenv(
  "AR1_NIG_TIMING_OUTPUT_DIR",
  unset = file.path(repo_root, "Code", "software_compare")
)
dir.create(timing_dir, recursive = TRUE, showWarnings = FALSE)
raw_file <- file.path(timing_dir, "ar1_nig_rstan_tmb_ngme2_timing_raw.csv")
summary_file <- file.path(timing_dir, "ar1_nig_rstan_tmb_ngme2_timing_result.md")

output_dir <- tempfile("ar1_nig_timing_figures_")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

quiet_eval <- function(expr) {
  expr_sub <- substitute(expr)
  if (verbose) {
    return(eval.parent(expr_sub))
  }
  value <- NULL
  invisible(capture.output(
    value <- suppressWarnings(eval.parent(expr_sub)),
    type = "output"
  ))
  value
}

time_call <- function(expr) {
  expr_sub <- substitute(expr)
  value <- NULL
  call_time <- system.time({
    if (verbose) {
      value <- eval.parent(expr_sub)
    } else {
      invisible(capture.output(
        value <- suppressWarnings(eval.parent(expr_sub)),
        type = "output"
      ))
    }
  })
  list(value = value, call_time = call_time)
}

timing_row <- function(rep_id, method, reported_time, call_time) {
  data.frame(
    rep = rep_id,
    method = method,
    reported_user = unname(reported_time[["user.self"]]),
    reported_system = unname(reported_time[["sys.self"]]),
    reported_elapsed = unname(reported_time[["elapsed"]]),
    call_user = unname(call_time[["user.self"]]),
    call_system = unname(call_time[["sys.self"]]),
    call_elapsed = unname(call_time[["elapsed"]]),
    row.names = NULL
  )
}

run_timing_once <- function(rep_id, base_config) {
  run_config <- base_config
  if (vary_seed) {
    run_config$seed <- base_config$seed + rep_id - 1L
  }

  cat(sprintf("[%02d/%02d] simulate data\n", rep_id, n_reps))
  data <- quiet_eval(simulate_nig_ar1(run_config))

  cat(sprintf("[%02d/%02d] ngme2 fit\n", rep_id, n_reps))
  ngme_call <- time_call(fit_with_ngme2(data$Y, data$x, run_config))
  ngme_fit <- ngme_call$value

  cat(sprintf("[%02d/%02d] ngme2 SGLD\n", rep_id, n_reps))
  sgld_call <- time_call(sample_with_ngme2(ngme_fit$fit, run_config))
  ngme_sampling <- sgld_call$value

  cat(sprintf("[%02d/%02d] TMB\n", rep_id, n_reps))
  tmb_call <- time_call(fit_with_tmb(data$Y))
  tmb_fit <- tmb_call$value

  cat(sprintf("[%02d/%02d] rstan\n", rep_id, n_reps))
  stan_call <- time_call(fit_with_rstan(data$Y, run_config))
  stan_fit <- stan_call$value

  rows <- rbind(
    timing_row(rep_id, "ngme2 fit", ngme_fit$time, ngme_call$call_time),
    timing_row(rep_id, "ngme2 SGLD", ngme_sampling$time, sgld_call$call_time),
    timing_row(
      rep_id,
      "ngme2 total",
      ngme_fit$time + ngme_sampling$time,
      ngme_call$call_time + sgld_call$call_time
    ),
    timing_row(rep_id, "TMB", tmb_fit$time, tmb_call$call_time),
    timing_row(rep_id, "rstan", stan_fit$time, stan_call$call_time)
  )

  rm(data, ngme_fit, ngme_sampling, tmb_fit, stan_fit)
  gc(verbose = FALSE)
  rows
}

summarize_timing <- function(raw) {
  methods <- unique(raw$method)
  do.call(rbind, lapply(methods, function(method) {
    rows <- raw[raw$method == method, , drop = FALSE]
    data.frame(
      method = method,
      n = nrow(rows),
      reported_elapsed_mean = mean(rows$reported_elapsed),
      reported_elapsed_sd = stats::sd(rows$reported_elapsed),
      call_elapsed_mean = mean(rows$call_elapsed),
      call_elapsed_sd = stats::sd(rows$call_elapsed),
      reported_user_mean = mean(rows$reported_user),
      reported_user_sd = stats::sd(rows$reported_user),
      reported_system_mean = mean(rows$reported_system),
      reported_system_sd = stats::sd(rows$reported_system),
      row.names = NULL
    )
  }))
}

raw_timing <- do.call(rbind, lapply(seq_len(n_reps), run_timing_once, base_config = config))
summary_timing <- summarize_timing(raw_timing)

utils::write.csv(raw_timing, raw_file, row.names = FALSE)

content <- c(
  "# NIG AR1 Runtime Benchmark",
  "",
  paste("**Date:**", Sys.Date()),
  "",
  paste("**Replicates:**", n_reps),
  paste("**Vary seed:**", vary_seed),
  "",
  "## Timing Summary",
  knitr::kable(summary_timing, format = "markdown", digits = 3),
  "",
  "Reported time follows the timing fields produced inside `ar1_nig_rstan_tmb_ngme2.R`.",
  "Call time measures the full wrapper call for each method, including setup inside that method.",
  "",
  "## Raw Timing File",
  raw_file,
  "",
  "## Configuration",
  "```",
  paste(capture.output(str(config)), collapse = "\n"),
  "```"
)
writeLines(content, summary_file)

print(knitr::kable(summary_timing, format = "simple", digits = 3))
cat("Saved timing summary to:", summary_file, "\n")
cat("Saved raw timing rows to:", raw_file, "\n")
