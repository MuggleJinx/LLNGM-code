# Simulation Results

**Date:** 2026-02-26

**Package Versions:**
- **ngme2:** 0.9.3
- **TMB:** 1.9.17
- **tmbstan:** 1.0.91

## ngme
### Execution Time (estimation)
```
   user  system elapsed 
 30.641   1.688   8.026 
```

### Execution Time (sampling)
```
   user  system elapsed 
 42.344   3.854   6.376 
```

### Estimates
```
  rho (field1)    mu (field1) sigma (field1)    nu (field1)          sigma 
     0.7953811      3.0344927      1.7181896      0.3622390      1.0166439 
```

### Credible Intervals
```
                   lower     upper
rho (field1)   0.7731853 0.8156028
mu (field1)    2.7288005 3.3437509
sigma (field1) 1.1451837 2.2074557
nu (field1)    0.2390470 0.4781543
sigma          0.8527929 1.1926027
```

## MCMC
### Execution Time
```
   user  system elapsed 
 92.698   0.809  36.477 
```

### Fit Summary
```
Inference for Stan model: ar1_nig.
4 chains, each with iter=2000; warmup=1000; thin=1; 
post-warmup draws per chain=1000, total post-warmup draws=4000.

                mean se_mean    sd   2.5%    25%    50%    75%  97.5% n_eff  Rhat
mu             3.140   0.163 0.392  2.471  2.828  3.121  3.431  3.921     6 1.493
log_sigma      0.235   0.301 0.589 -0.982 -0.179  0.442  0.715  0.951     4 2.112
rho_un         1.089   0.003 0.032  1.026  1.067  1.089  1.110  1.152   127 1.037
log_sigma_eps  0.040   0.025 0.130 -0.285 -0.024  0.060  0.130  0.236    28 1.116
log_nu        -0.894   0.089 0.311 -1.510 -1.109 -0.888 -0.672 -0.313    12 1.245

Samples were drawn using NUTS(diag_e) at Thu Feb 26 15:13:02 2026.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at 
convergence, Rhat=1).
```

### 95 CI
```
               mean         sd      q025      q975
mu        3.1396552 0.39178180 2.4710915 3.9208967
sigma     1.4650829 0.68831386 0.3746482 2.5877377
sigma_eps 1.0497272 0.12794841 0.7521945 1.2666108
nu        0.4288613 0.13272846 0.2209689 0.7314414
rho       0.7960818 0.01183141 0.7722152 0.8184461
```

## TMBStan(Laplace)
### Execution Time
```
    user   system  elapsed 
1198.293    9.097  340.685 
```

### Fit Summary
```
Inference for Stan model: ar1_nig.
4 chains, each with iter=2000; warmup=1000; thin=1; 
post-warmup draws per chain=1000, total post-warmup draws=4000.

                mean se_mean    sd   2.5%    25%    50%    75%  97.5% n_eff  Rhat
mu             3.299   0.002 0.099  3.116  3.233  3.294  3.361  3.503  3594 1.000
log_sigma     -2.341   0.021 1.057 -4.870 -2.989 -2.175 -1.559 -0.726  2571 1.000
rho_un         1.105   0.000 0.030  1.047  1.085  1.105  1.124  1.164  4148 1.000
log_sigma_eps  0.332   0.001 0.062  0.209  0.291  0.331  0.372  0.455  4102 0.999
log_nu        -0.837   0.002 0.101 -1.024 -0.909 -0.840 -0.770 -0.631  3737 1.001

Samples were drawn using NUTS(diag_e) at Thu Feb 26 15:22:01 2026.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at 
convergence, Rhat=1).
```

### 95 CI
```
               mean         sd       q025      q975
mu        3.2993154 0.09900733 3.11641036 3.5027918
sigma     0.1490522 0.12928538 0.00767037 0.4838982
sigma_eps 1.3959875 0.08679943 1.23260363 1.5762312
nu        0.4352255 0.04470212 0.35917499 0.5321339
rho       0.8020398 0.01064840 0.78073741 0.8224937
```

## Comparison
### Parameter Estimates


|            |     mu|  sigma|    rho| sigma_eps|     nu|
|:-----------|------:|------:|------:|---------:|------:|
|true_est    | 3.0000| 2.0000| 0.8000|    1.0000| 0.4000|
|ngme_est    | 3.0345| 1.7182| 0.7954|    1.0008| 0.3622|
|stan_est    | 3.1397| 1.2644| 0.7964|    1.0413| 0.4090|
|tmbstan_est | 3.2993| 0.0962| 0.8023|    1.3933| 0.4330|

### KLD Result
```
Noise KLD Comparison
===================
Reference: true 

KLD values (lower is closer to reference):
  ngme: 0.011033 <- CLOSEST
  stan: 0.074859
  tmb: 2.026020

Closest to reference: ngme 
```
