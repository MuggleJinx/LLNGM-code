# NIG AR1 Runtime Benchmark

**Date:** 2026-04-13

**Replicates:** 10
**Vary seed:** TRUE

## Timing Summary
|method      |  n| reported_elapsed_mean| reported_elapsed_sd| call_elapsed_mean| call_elapsed_sd| reported_user_mean| reported_user_sd| reported_system_mean| reported_system_sd|
|:-----------|--:|---------------------:|-------------------:|-----------------:|---------------:|------------------:|----------------:|--------------------:|------------------:|
|ngme2 fit   | 10|                 1.583|               0.664|             2.189|           0.670|              5.527|            2.507|                0.896|              0.392|
|ngme2 SGLD  | 10|                 6.018|               0.280|             6.164|           0.291|             21.506|            0.105|                5.001|              0.181|
|ngme2 total | 10|                 7.601|               0.684|             8.353|           0.709|             27.033|            2.550|                5.897|              0.394|
|TMB         | 10|                 0.273|               0.024|            19.386|           0.465|              0.267|            0.023|                0.006|              0.002|
|rstan       | 10|                41.205|               7.538|            45.026|           9.560|              3.831|            0.314|                0.317|              0.030|

Reported time follows the timing fields produced inside `ar1_nig_rstan_tmb_ngme2.R`.
Call time measures the full wrapper call for each method, including setup inside that method.

## Raw Timing File
/Users/jinx0a/Repo/Methodology-project/Code/software_compare/ar1_nig_rstan_tmb_ngme2_timing_raw.csv

## Configuration
```
List of 30
 $ seed                  : int 234
 $ n                     : int 500
 $ mu                    : num 3
 $ sigma                 : num 2
 $ rho                   : num 0.8
 $ sigma_eps             : num 1
 $ nu                    : num 0.4
 $ ngme_iterations       : int 400
 $ ngme_batches          : int 20
 $ stan_iter             : int 1000
 $ stan_warmup           : int 500
 $ stan_chains           : int 4
 $ stan_cores            : int 4
 $ stan_adapt_delta      : num 0.95
 $ stan_stepsize         : num 1e-04
 $ stan_max_treedepth    : int 12
 $ stan_total_samples    : int 2000
 $ sgld_chains           : int 4
 $ sgld_total_samples    : int 2000
 $ sgld_stepsize         : num 0.05
 $ sgld_batch            : int 20
 $ sgld_n_gibbs_samples  : int 20
 $ sgld_alpha            : num 0.6
 $ sgld_t0               : num 0
 $ sgld_start_sd         : num 0.01
 $ sgld_burnin           : int 0
 $ sgld_burnin_iter      : int 1
 $ sgld_verbose          : logi FALSE
 $ sgld_samples_per_chain: int 500
 $ sgld_iterations       : int 500
```
