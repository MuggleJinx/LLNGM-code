# Bivariate Wind Model Results

**Date:** 2026-02-26

**Package Versions:**
- **ngme2:** 0.9.3

## Gaussian Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)_u_wind         lon_u_wind (Intercept)_v_wind         lat_v_wind 
            2.9595            -0.0934            -8.5506             0.2227 

Models: 
$field1
  Model type: Tensor product
      first: AR(1)
          rho = 0.712
      second: Bivariate Matern model
          theta = 0 (fixed)
          rho = 0.342
          sd1 = 1.84
          sd2 = 2.51
          u_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.25
          v_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.144
  Noise type: NORMAL
  Noise parameters: 
      sigma = 1

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.16


```
**Fitting Time:**
- **Gaussian:** 3.382029 mins

## NIG Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)_u_wind         lon_u_wind (Intercept)_v_wind         lat_v_wind 
            3.0275            -0.0968            -8.9534             0.2339 

Models: 
$field1
  Model type: Tensor product
      first: AR(1)
          rho = 0.733
      second: Bivariate Matern model
          theta = -0.264
          rho = 0.223
          sd1 = 1.85
          sd2 = 2.47
          u_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.256
          v_wind: Matern
              alpha = 2 (fixed)
              kappa = 0.175
  Noise type: NIG
  Noise parameters: 
      mu = -0.016
      sigma = 1
      nu = 0.056

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0435


```
**Fitting Time:**
- **NIG:** 12.37966 mins

## Cross Validation Results
```
$mean.scores
               MAE      MSE  neg.CRPS neg.sCRPS
bv_gauss 0.8372242 1.055818 0.6374504  1.140199
bv_nig   0.8502079 1.261055 0.6103421  1.122826

$sd.scores
                MAE        MSE   neg.CRPS   neg.sCRPS
bv_gauss 0.01138651 0.02634074 0.01279839 0.009154727
bv_nig   0.05089126 0.22244085 0.05214398 0.025298196

```
