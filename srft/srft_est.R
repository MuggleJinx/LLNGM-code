seed <- 123
set.seed(seed)
library(tidyverse)
library(ngme2)
load("srft/srft_data.rda")

# update the data
srft_data$log_egfr <- log(srft_data$egfr)
head(srft_data)

n_sample <- 500
# sample n_sample id, make sure the observations are great than 10
rs_id <- srft_data %>%
  group_by(id) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs > 10) %>%
  sample_n(n_sample) %>%
  pull(id)

srft_data_sub <- srft_data %>% filter(id %in% rs_id)
dim(srft_data_sub)
range(srft_data_sub$log_egfr)

# plot the data of random 7 patients
n_plot <- 7
plot_data <- srft_data_sub %>%
  filter(id %in% sample(unique(srft_data_sub$id), n_plot)) %>%
  ggplot(aes(x = fu, y = log_egfr, color = factor(id))) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Follow up time (years)",
    y = "log(eGFR)"
  ) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none")
plot_data

ggsave("Figures/srft_data_sub.png", plot_data, width = 10, height = 6)


######### fit the model #########
# define the mesh
meshs <- ngme_make_mesh_repls(
  data = srft_data_sub,
  map = ~fu,
  replicate = ~id,
  mesh_type = "regular"
)
length(meshs)

control_opt <- control_opt(
  optimizer = adamW(),
  estimation = TRUE,
  iterations = 3000,
  print_check_info = TRUE,
  n_batch = 20,
  n_parallel_chain = 4,
  rao_blackwellization = TRUE,
  verbose = TRUE,
  seed = seed
)


time_gauss <- system.time({
  fit_gauss <- ngme(
    log_egfr ~
      sex + bage +
      f(id, model = iid(), noise = noise_normal(sigma = 4)) +
      f(fu,
        model = matern(mesh = meshs, kappa = 2),
        noise = noise_normal(),
        replicate = id
      ),
    control_ngme = control_ngme(
      beta = c(1, -0.07, 0)
    ),
    data = srft_data_sub,
    control_opt = control_opt
  )
})[[3]]
fit_gauss
traceplot(fit_gauss, "field1")
traceplot(fit_gauss, "field2")
traceplot(fit_gauss)
save(fit_gauss, file = "model-fits/srft_fit-fit_gauss.rda")
load("model-fits/srft_fit-fit_gauss.rda")


gauss_samples <- compute_ngme_sgld_samples(
  fit = fit_gauss,
  iterations = 1000,
  optimizer = sgld(stepsize = 0.0005),
  burnin = 10,
  n_batch = 20,
  n_parallel_chain = 4,
  alpha = 0.55,
  t0 = 0,
  start_sd = 0.01,
  burnin_iter = 0,
  seed = seed,
  verbose = TRUE,
  name = "all"
)
gauss_refit <- attr(gauss_samples, "refit")
traceplot(gauss_refit)

gauss_ci <- ngme_sgld_ci(
  gauss_samples,
  lower = 0.025, upper = 0.975
)
# The posterior mean and 95% credible interval for the regression coefficients
gauss_ci$estimates
gauss_ci$ci



###### NIG model ######

control_opt_nig <- control_opt(
  estimation = TRUE,
  optimizer = adamW(),
  iterations = 2000,
  print_check_info = TRUE,
  n_batch = 20,
  n_parallel_chain = 4,
  rao_blackwellization = TRUE,
  verbose = TRUE,
  seed = seed
)

time_nig <- system.time({
  fit_nig <- ngme(
    log_egfr ~
      sex + bage +
      f(id, model = iid()) +
      f(fu,
        model = matern(mesh = meshs),
        noise = noise_nig(),
        replicate = id
      ),
    data = srft_data_sub,
    start = fit_gauss,
    control_opt = control_opt_nig
  )
})[[3]]
fit_nig
traceplot(fit_nig, "field1")
traceplot(fit_nig, "field2")
traceplot(fit_nig)
save(fit_nig, file = "model-fits/srft_fit-fit_nig.rda")
load("model-fits/srft_fit-fit_nig.rda")


nig_samples <- compute_ngme_sgld_samples(
  fit = fit_nig,
  iterations = 1000,
  optimizer = sgld(stepsize = 0.0005),
  burnin = 10,
  n_batch = 20,
  n_parallel_chain = 4,
  alpha = 0.55,
  t0 = 0,
  start_sd = 0.01,
  burnin_iter = 0,
  seed = seed,
  verbose = TRUE,
  name = "all"
)
nig_refit <- attr(nig_samples, "refit")
traceplot(nig_refit)

nig_ci <- ngme_sgld_ci(
  nig_samples,
  lower = 0.025, upper = 0.975
)
# The posterior mean and 95% credible interval for the regression coefficients
nig_ci$estimates
nig_ci$ci





time_cv <- system.time({
  cv <- cross_validation(
    list(
      gauss = fit_gauss,
      nig = fit_nig
    ),
    k = 10,
    N = 4,
    n_gibbs_samples = 200,
    print = TRUE,
    thining_gap = 1,
    seed = seed,
    parallel = FALSE
  )
})[[3]]
cv
save(cv, file = "model-fits/srft_fit-cv.rda")
load("model-fits/srft_fit-cv.rda")

# save fit_gauss, fit_nig, cv to report.md
file_addr <- "srft/srft_result_500.md"

md_content <- c(
  "# SRFT Model Fitting Results",
  "",
  paste("**Date:**", Sys.Date()),
  "",
  "**Package Versions:**",
  paste("- **ngme2:**", as.character(packageVersion("ngme2"))),
  "",
  "## Gaussian Model",
  "```",
  paste(capture.output(print(fit_gauss)), collapse = "\n"),
  "```",
  "",
  "### SGLD Estimates",
  "```",
  paste(capture.output(print(gauss_ci$estimates)), collapse = "\n"),
  "```",
  "",
  "### SGLD Credible Intervals",
  "```",
  paste(capture.output(print(gauss_ci$ci)), collapse = "\n"),
  "```",
  "",
  "## NIG Model",
  "```",
  paste(capture.output(print(fit_nig)), collapse = "\n"),
  "```",
  "",
  "### SGLD Estimates",
  "```",
  paste(capture.output(print(nig_ci$estimates)), collapse = "\n"),
  "```",
  "",
  "### SGLD Credible Intervals",
  "```",
  paste(capture.output(print(nig_ci$ci)), collapse = "\n"),
  "```",
  "",
  "## Cross Validation Results",
  "```",
  paste(capture.output(print(cv$mean.scores)), collapse = "\n"),
  "```",
  "",
  "## Time Results",
  "```",
  paste(capture.output(print(time_gauss)), collapse = "\n"),
  paste(capture.output(print(time_nig)), collapse = "\n"),
  paste(capture.output(print(time_cv)), collapse = "\n"),
  "```"
)

writeLines(md_content, file_addr)
