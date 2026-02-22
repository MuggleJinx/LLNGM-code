set.seed(1)
load("climate/precip.data.Rdata")

library(ngme2)
library(knitr)
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


fit_gauss <- readRDS("model-fits/climate-fit-gauss.rds")
fit_gauss_ns <- readRDS("model-fits/climate-fit-gauss-ns.rds")
fit_nig <- readRDS("model-fits/climate-fit-nig.rds")
fit_nig_ns <- readRDS("model-fits/climate-fit-nig-ns.rds")

df_1989 <- df[df$year == 11, ]
cv_result <- ngme2::cross_validation(
  list(
    gauss_ns = fit_gauss_ns,
    gauss = fit_gauss,
    nig = fit_nig,
    nig_ns = fit_nig_ns
  ),
  data = df_1989,
  type = "k-fold",
  seed = 1,
  k = 10,
  n_gibbs_samples = 100,
  print = TRUE,
  parallel = FALSE
)

print(cv_result)
saveRDS(cv_result, "model-fits/climate-fit-cv.rds")
# cv_result <- readRDS("model-fits/climate-fit-cv.rds")

print(knitr::kable(cv_result$mean.scores,
  caption = "Mean Cross-Validation Scores",
  digits = 5,
  format = "simple"
))

cat("\nMean Cross-Validation Scores:\n")
print(cv_result$mean.scores, digits = 4, row.names = TRUE)
