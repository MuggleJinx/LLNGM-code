# Climate Model Results

**Date:** 2026-02-26

**Package Versions:**
- **ngme2:** 0.9.3

## Gaussian Model
```
*** Ngme object ***

Fixed effects: 
  None

Models: 
$matern
  Model type: Matern
      alpha = 2 (fixed)
      kappa = 0.674
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.182

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.015


Number of global replicates is 10 
```

## NIG Model
```
*** Ngme object ***

Fixed effects: 
  None

Models: 
$matern
  Model type: Matern
      alpha = 2 (fixed)
      kappa = 0.685
  Noise type: NIG
  Noise parameters: 
      mu = -0.0348
      sigma = 0.191
      nu = 4.38

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0134


Number of global replicates is 10 
```

## Non-Stationary Gaussian Model
```
*** Ngme object ***

Fixed effects: 
  None

Models: 
$matern
  Model type: Matern
      alpha = 2 (fixed)
       theta_kappa =  -0.1238, -0.6339,  1.6161,  0.4576,  0.7360, -0.0464, ... 
  Noise type: NORMAL
  Noise parameters: 
      theta_sigma = -1.7981,  0.0523,  0.5970,  0.3505,  0.7976,  0.0646,  0.4321, -0.1378,  0.2146, -0.0761, -0.0178, -0.1931, -0.0250, -0.2541, -0.2466, -0.1882,  0.1860,  0.0224

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0133


Number of global replicates is 10 
```

## Non-Stationary NIG Model
```
*** Ngme object ***

Fixed effects: 
  None

Models: 
$matern
  Model type: Matern
      alpha = 2 (fixed)
       theta_kappa =  -1.746697, -0.479606,  1.203287, -0.330049, -0.488020, -0.372653, ... 
  Noise type: NIG
  Noise parameters: 
      theta_mu = -0.1922,  0.0516,  0.2156, -0.1709,  0.0773, -0.1035,  0.2981, -0.0950,  0.0209, -0.0517, -0.0183, -0.4296, -0.0332,  0.1084, -0.0667, -0.1891, -0.1656, -0.1492
      theta_sigma = -5.291,  7.771, -1.176, -1.058,  0.320,  0.540, -0.659,  2.366,  1.262,  0.303, -1.008,  0.447,  2.788, -0.498,  0.631,  0.345,  0.319, -0.309
      theta_nu =  0.4095,  3.3861, -1.1684,  0.0595,  0.0462,  0.4346, -0.9106,  0.5868, -0.3024,  0.0730, -0.0707,  0.0363,  0.6468,  0.6404, -0.0807,  0.9635,  0.6409,  0.3110

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0157


Number of global replicates is 10 
```

## Cross Validation Results
```
$mean.scores
                MAE          MSE   neg.CRPS  neg.sCRPS
gauss_ns 0.01916557 0.0006516331 0.01366538 -0.8108199
gauss    0.01919584 0.0006509577 0.01395793 -0.7805739
nig      0.01920792 0.0006579794 0.01389134 -0.7868884
nig_ns   0.01951273 0.0006710442 0.01387055 -0.8001768

$sd.scores
                  MAE          MSE     neg.CRPS   neg.sCRPS
gauss_ns 0.0001071771 6.299802e-06 0.0001363635 0.004649612
gauss    0.0001063488 5.277657e-06 0.0001255187 0.004483975
nig      0.0001228959 8.023830e-06 0.0001370808 0.005156660
nig_ns   0.0001405015 1.014096e-05 0.0001523825 0.005108442

```
