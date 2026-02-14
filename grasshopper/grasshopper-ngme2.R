library(ngme2)
library(ggplot2)
seed <- 123
set.seed(seed)

datasets <- list(
  Lynx.Idaho = list(
    Time = c(
      1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1970, 1971,
      1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    Observed = c(
      346, 675, 802, 1478, 1173, 756, 861, 972, 854, 1161, 1318, 901, 901,
      1173, 608, 811, 903, 584, 1179, 1020, 1129, 966
    )
  ),
  Lynx.Florida = list(
    Time = c(
      1946, 1947, 1948, 1949, 1950, 1954, 1955, 1956, 1957, 1958,
      1959, 1960, 1961, 1963, 1964, 1965, 1966, 1967, 1968, 1975, 1976, 1977,
      1978, 1979, 1980, 1981
    ),
    Observed = c(
      672, 1028, 538, 566, 300, 400, 400, 400, 400, 300, 250,
      450, 450, 13, 23, 23, 2, 400, 20, 389, 537, 983, 1698, 1132, 1702, 1031
    )
  ),
  Lynx.California = list(
    Time = c(
      1934, 1935, 1936, 1938, 1940, 1941, 1942, 1943, 1944, 1945,
      1946, 1947, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1956, 1957, 1958,
      1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,
      1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    Observed = c(
      1994, 1436, 1290, 2292, 2776, 3239, 1923, 2898, 2063, 1730,
      1072, 689, 169, 375, 293, 239, 336, 223, 228, 276, 202, 142, 175, 304, 205,
      295, 361, 221, 221, 241, 244, 381, 588, 319, 588, 686, 1244, 1393, 2203,
      3618, 4445, 6928, 7809, 9595, 9337
    )
  ),
  Lynx.Michigan = list(
    Time = c(
      1936, 1937, 1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946,
      1947, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1956, 1957, 1958, 1962,
      1963, 1964, 1966, 1969, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    Observed = c(
      1134, 811, 598, 528, 529, 375, 2538, 2802, 2910, 2363,
      2174, 2063, 1547, 1753, 1443, 1836, 696, 847, 880, 762, 200, 588, 494, 265,
      400, 300, 341, 331, 386, 597, 223, 200
    )
  ),
  Lynx.Maine = list(
    Time = c(
      1934, 1935, 1936, 1937, 1942, 1943, 1944, 1945, 1946, 1947,
      1948, 1949, 1950, 1951, 1952, 1953, 1954, 1956, 1957, 1958, 1959, 1961,
      1962, 1963, 1964, 1965, 1966, 1968, 1970, 1971, 1972, 1973, 1974, 1975,
      1976, 1977, 1978, 1979, 1980, 1981
    ),
    Observed = c(
      644, 911, 687, 400, 133, 105, 184, 1044, 181, 178, 489, 100,
      263, 83, 106, 795, 667, 695, 263, 198, 221, 278, 231, 588, 269, 152, 233, 153,
      730, 654, 641, 573, 544, 373, 436, 389, 278, 318, 381, 345
    )
  ),
  Lynx.Wisconsin = list(
    Time = c(
      1934, 1935, 1936, 1937, 1938, 1940, 1941, 1942, 1943, 1944,
      1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1954, 1956, 1959, 1960,
      1969, 1970, 1971, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    Observed = c(
      302, 428, 513, 461, 593, 180, 283, 191, 765, 384, 1048, 577,
      427, 437, 482, 525, 724, 740, 524, 321, 479, 869, 148, 148, 147, 205, 223,
      275, 163, 223, 131, 81, 168
    )
  ),
  grasshopper = list(
    Time = c(
      1948, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973,
      1974, 1975, 1977, 1978, 1979, 1980, 1981, 1983, 1984, 1985, 1986, 1987, 1988,
      1989, 1990
    ),
    Observed = c(
      5.7981, 7.7194, 4.8022, 3.9397, 11.8806, 10.7568, 8.9586,
      10.6619, 6.5895, 4.4905, 3.0684, 6.9973, 5.3986, 4.2777, 6.1166, 7.2989,
      5.0850, 4.8298, 5.3997, 4.7679, 4.5073, 1.9714, 4.1007, 5.6403, 3.0492,
      2.8144, 4.4071, 2.4121, 3.2233, 1.4236, 2.3404, 10.5283, 7.6872, 2.7305,
      3.4570, 5.4336, 3.1487, 3.8315, 4.4805
    )
  )
)
Time <- c(
  1948, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
  1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973,
  1974, 1975, 1977, 1978, 1979, 1980, 1981, 1983, 1984, 1985, 1986, 1987, 1988,
  1989, 1990
)

Time_union_minus <- setdiff(1948:1990, Time)
Time_union_minus

# Select dataset to use (change this to select different datasets)
# Selected_dataset <- datasets$Lynx.Maine
Selected_dataset <- datasets$grasshopper
Time.t <- Selected_dataset$Time
range(Time.t)
Observed.t <- Selected_dataset$Observed
n <- length(Time.t)
length(Time.t)

data <- data.frame(Year = Time.t, Number = Observed.t)
library(ggplot2)
gg <- ggplot(data, aes(x = Year, y = Number)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("Abundance") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
gg
ggsave("Figures/grasshopper.png", width = 6, height = 4)


# Set control parameters for ngme
control <- control_opt(
  start_sd = 0.1,
  iterations = 100000,
  rao_blackwell = TRUE,
  # optimizer = precond_sgd(preconditioner = "full"),
  optimizer = adamW(),
  seed = seed
)


###### Model for the original scale data ######
time_ar1_gauss_original <- system.time({
  ar1_gauss_original <- ngme(
    y ~ 1 + f(x, model = ar1(), name = "latent"),
    data = data.frame(y = Observed.t, x = Time.t),
    control_opt = control
  )
})
ar1_gauss_original
p <- traceplot(ar1_gauss_original)
ggsave("Figures/bio/bio_grasshopper_traceplot_gaussian.png", p, width = 8, height = 6)


time_ar1_nig_original <- system.time({
  ar1_nig_original <- ngme(
    y ~ 1 + f(x, model = ar1(), noise = noise_nig(), name = "latent"),
    data = data.frame(y = Observed.t, x = Time.t),
    control_opt = control
  )
})
ar1_nig_original
p <- traceplot(ar1_nig_original)
ggsave("Figures/bio/bio_grasshopper_traceplot_nig.png", p, width = 8, height = 6)

baseline_original <- ngme(
  y ~ 1,
  data = data.frame(y = Observed.t, x = Time.t),
  control_opt = control
)

###### Cross-validation ######
splits <- make_time_series_cv_index(
  time_idx = 1:length(Time.t),
  train_length = min(10, floor(n * 0.7)) # Adjust train_length based on dataset size
)

cv_time_original <- system.time({
  cv_results_original <- ngme2::cross_validation(
    list(
      ar1_gaussian = ar1_gauss_original,
      ar1_nig = ar1_nig_original,
      # ar1_nig_mn = ar1_nig_mn_original,
      baseline = baseline_original
    ),
    type = "custom",
    n_gibbs_samples = 1000,
    n_burnin = 200,
    train_idx = splits$train,
    test_idx = splits$test,
    N_sim = 10,
    seed = seed,
    print = TRUE
  )
})
cv_results_original

# Add fitting times to the mean scores table
times <- c(
  ar1_gauss_original = time_ar1_gauss_original[3],
  ar1_nig_original = time_ar1_nig_original[3],
  cv_time_original = cv_time_original[3]
)
times

save(ar1_gauss_original, ar1_nig_original, times, cv_results_original, file = "Code/bio/bio_fit_results.RData")
load("Code/bio/bio_fit_results.RData")
ar1_gauss_original
ar1_nig_original


##### Plot rolling window prediction ######
n <- length(Time.t)
ord <- order(Time.t)

n_train <- 10
eval_idx <- ord[(n_train + 1):n]
x_eval_years <- Time.t[eval_idx] # 对应的年份（不等间隔 OK）

gauss_2.5q <- double(n - n_train)
gauss_97.5q <- double(n - n_train)
nig_2.5q <- double(n - n_train)
nig_97.5q <- double(n - n_train)

for (seed in 1:1) {
  for (k in seq_along(eval_idx)) {
    idx <- eval_idx[k]
    r <- which(ord == idx)

    train_idx <- ord[(r - n_train):(r - 1)]

    burnin_size <- 1000
    sampling_size <- 30000

    gauss <- predict(
      ar1_gauss_original,
      map = list(latent = Time.t[idx]),
      train_idx = train_idx,
      burnin_size = burnin_size,
      sampling_size = sampling_size,
      estimator = c("0.025q", "0.975q"),
      seed = seed
    )
    gauss_2.5q[k] <- gauss$`0.025q`
    gauss_97.5q[k] <- gauss$`0.975q`

    nig <- predict(
      ar1_nig_original,
      map = list(latent = Time.t[idx]),
      train_idx = train_idx,
      burnin_size = burnin_size,
      sampling_size = sampling_size,
      estimator = c("0.025q", "0.975q"),
      seed = seed
    )
    nig_2.5q[k] <- nig$`0.025q`
    nig_97.5q[k] <- nig$`0.975q`
  }
}


# Create a data frame for the observed data
observed_data <- data.frame(
  Year = Time.t[(n_train + 1):n],
  Abundance = Observed.t[(n_train + 1):n]
)

# Create a data frame for the predicted data with confidence intervals
predicted_data <- data.frame(
  Year = rep(x_eval_years, 2),
  Model = rep(c("NIG (95% PI)", "Gaussian (95% PI)"), each = length(x_eval_years)),
  Lower = c(nig_2.5q, gauss_2.5q),
  Upper = c(nig_97.5q, gauss_97.5q)
)

# Plot using ggplot2
ggplot() +
  geom_line(data = observed_data, aes(x = Year, y = Abundance, linetype = "Observed Data"), color = "black", size = 1.5) +
  geom_point(data = observed_data, aes(x = Year, y = Abundance), color = "black", size = 2) +
  geom_ribbon(data = predicted_data, aes(x = Year, ymin = Lower, ymax = Upper, fill = Model), alpha = 0.3) +
  geom_line(data = predicted_data, aes(x = Year, y = Lower, color = Model), linetype = "dashed", size = 1) +
  geom_line(data = predicted_data, aes(x = Year, y = Upper, color = Model), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("blue", "green")) +
  scale_fill_manual(values = c("blue", "green")) +
  scale_linetype_manual(values = c("solid"), name = "", labels = c("Observed Data")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  )
ggsave("Figures/grasshopper_predictions.png", width = 6, height = 4)


# Observe the latent states
# pred_ar1_nig_original <- predict(ar1_nig_original, map = list(latent = Time.t), estimator = c("mean"))
# pred_ar1_gauss_original <- predict(ar1_gauss_original, map = list(latent = Time.t), estimator = c("mean"))

# # plot the predictions
# plot(Time.t, Observed.t, type = "l", col = "black", lwd = 2, xlab = "Year", ylab = "Abundance")
# lines(Time.t, pred_ar1_nig_original$mean, col = "blue", lwd = 2)
# lines(Time.t, pred_ar1_gauss_original$mean, col = "green", lwd = 2)
# legend("topright", legend = c("Observed Data", "NIG", "Gaussian"), col = c("black", "blue", "green"), lwd = 2)
# points(Time.t, Observed.t, col = "black", pch = 19)
