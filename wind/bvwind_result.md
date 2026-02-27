# Bivariate Wind Model Results

**Date:** 2026-02-27

**Package Versions:**
- **ngme2:** 0.9.3

## Gaussian Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)_u_wind         lon_u_wind (Intercept)_v_wind         lat_v_wind 
            3.1400            -0.0985            -8.8490             0.2285 

Models: 
$field1
  Model type: Tensor product
      first: AR(1)
          rho = 0.717
      second: Bivariate Matern model
          theta = 0 (fixed)
          rho = 0.348
          sd1 = 1.84
          sd2 = 2.66
          u_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.249
          v_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.135
  Noise type: NORMAL
  Noise parameters: 
      sigma = 1

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.124


```
**Fitting Time:**
- **Gaussian:** 1.483926 mins

## NIG Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)_u_wind         lon_u_wind (Intercept)_v_wind         lat_v_wind 
             3.187             -0.102             -8.996              0.234 

Models: 
$field1
  Model type: Tensor product
      first: AR(1)
          rho = 0.731
      second: Bivariate Matern model
          theta = -0.285
          rho = 0.27
          sd1 = 1.71
          sd2 = 2.41
          u_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.24
          v_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.144
  Noise type: NIG
  Noise parameters: 
      mu = -0.0161
      sigma = 1
      nu = 0.514 (lower bound 0.5)

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0363


```
**Fitting Time:**
- **NIG:** 7.280567 mins

## Cross Validation Results
```
$mean.scores
               MAE      MSE  neg.CRPS neg.sCRPS
bv_gauss 0.8321676 1.045948 0.6364401  1.138407
bv_nig   0.8086304 1.113181 0.6210141  1.115680

$sd.scores
                 MAE        MSE   neg.CRPS   neg.sCRPS
bv_gauss 0.009058166 0.02266383 0.01380328 0.008406491
bv_nig   0.011868250 0.04499339 0.01576624 0.009613860

```
