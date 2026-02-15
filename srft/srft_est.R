seed <- 123
library(INLA)
library(inlabru)
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
  iterations = 10000,
  print_check_info = TRUE,
  n_batch = 5,
  n_parallel_chain = 4,
  rao_blackwellization = TRUE,
  verbose = TRUE,
  seed = seed
)
# --------------------- compare with INLA -----------------

# ran <- range(srft_data_sub$fu); ran
# mesh = fmesher::fm_mesh_1d(seq(ran[1], ran[2], length=100))

# # Fit INLA model equivalent to fit_gauss
# spde <- inla.spde2.pcmatern(
#   # Mesh and smoothness parameter
#   mesh = mesh, alpha = 2,
#   # P(practic.range < 0.3) = 0.5
#   prior.range = c(0.3, 0.05),
#   # P(sigma > 1) = 0.01
#   prior.sigma = c(10, 0.001)
# )

# bru_fit <- bru(
#   log_egfr ~ sex + bage +
#     Intercept(1) +
#     f(id, model = "iid") +
#     spde(fu,
#       model = spde,
#       replicate = id
#     ),
#   data = srft_data_sub
# )
# summary(bru_fit)

time_gauss <- system.time({
  fit_gauss <- ngme(
    log_egfr ~
      sex + bage +
      f(id, model = iid()) +
      f(fu,
        model = matern(mesh = meshs, kappa = 2),
        noise = noise_normal(),
        replicate = id
      ),
    data = srft_data_sub,
    control_opt = control_opt
  )
})[[3]]
fit_gauss
traceplot(fit_gauss, "field1")
traceplot(fit_gauss, "field2")
traceplot(fit_gauss)
save(fit_gauss, file = "Code/srft_fit/fit_gauss.rda")
load("Code/srft_fit/fit_gauss.rda")


control_opt_nig <- control_opt(
  optimizer = adamW(),
  estimation = TRUE,
  iterations = 12000,
  print_check_info = TRUE,
  n_batch = 60,
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
        model = matern(mesh = meshs, kappa = 2),
        noise = noise_nig(),
        replicate = id
      ),
    data = srft_data_sub,
    control_opt = control_opt_nig
  )
})[[3]]
fit_nig
traceplot(fit_nig, "field1")
traceplot(fit_nig, "field2")
traceplot(fit_nig)
save(fit_nig, file = "Code/srft_fit/fit_nig.rda")
load("Code/srft_fit/fit_nig.rda")

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
save(cv, file = "Code/srft_fit/cv.rda")
load("Code/srft_fit/cv.rda")

# save fit_gauss, fit_nig, cv to report.md
file_addr <- "Code/srft_fit/srft_result_500.md"
if (!file.exists(file_addr)) {
  file.create(file_addr)
} else {
  file.remove(file_addr)
  file.create(file_addr)
}

# Capture and write fit_gauss output
cat("# SRFT Model Fitting Results\n\n", file = file_addr, append = TRUE)
cat("## Gaussian Model (fit_gauss)\n\n", file = file_addr, append = TRUE)
cat("```\n", file = file_addr, append = TRUE)
cat(capture.output(print(fit_gauss)), sep = "\n", file = file_addr, append = TRUE)
cat("\n```\n\n", file = file_addr, append = TRUE)

# Capture and write fit_nig output
cat("## NIG Model (fit_nig)\n\n", file = file_addr, append = TRUE)
cat("```\n", file = file_addr, append = TRUE)
cat(capture.output(print(fit_nig)), sep = "\n", file = file_addr, append = TRUE)
cat("\n```\n\n", file = file_addr, append = TRUE)

# Capture and write cv output
cat("## Cross Validation Results\n\n", file = file_addr, append = TRUE)
cat("```\n", file = file_addr, append = TRUE)
cat(capture.output(print(cv$mean.scores)), sep = "\n", file = file_addr, append = TRUE)
cat("\n```\n\n", file = file_addr, append = TRUE)

# Time results
cat("## Time Results\n\n", file = file_addr, append = TRUE)
cat("```\n", file = file_addr, append = TRUE)
cat(capture.output(print(time_gauss)), sep = "\n", file = file_addr, append = TRUE)
cat(capture.output(print(time_nig)), sep = "\n", file = file_addr, append = TRUE)
cat(capture.output(print(time_cv)), sep = "\n", file = file_addr, append = TRUE)
cat("\n```\n\n", file = file_addr, append = TRUE)
