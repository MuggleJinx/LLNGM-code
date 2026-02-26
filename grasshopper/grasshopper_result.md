# Grasshopper Model Results

**Date:** 2026-02-26

**Package Versions:**
- **ngme2:** 0.9.3

## Gaussian Model
### Execution Time
```
   user  system elapsed 
  7.249   1.314   2.802 
```

### SGLD Estimates
```
  rho (latent) sigma (latent)          sigma    (Intercept)        scale_t 
      0.312020       2.125804       0.166408       5.348890      -1.053938 
```

### SGLD Credible Intervals
```
                     lower      upper
rho (latent)    0.11195210  0.5270271
sigma (latent)  1.68620699  2.6616589
sigma           0.08053905  0.3260080
(Intercept)     4.68689177  6.1382439
scale_t        -1.26129722 -0.8838975
```

## NIG Model
### Execution Time
```
   user  system elapsed 
 75.430  13.548  22.928 
```

### SGLD Estimates
```
  rho (latent)    mu (latent) sigma (latent)    nu (latent)          sigma    (Intercept)        scale_t 
     0.3678548      2.4125027      0.4720788      1.3264534      0.8439761      5.2046251     -0.8579225 
```

### SGLD Credible Intervals
```
                    lower      upper
rho (latent)    0.1457250  0.5445908
mu (latent)     1.7491359  3.1031127
sigma (latent)  0.2141268  0.9282249
nu (latent)     0.7998648  2.5733415
sigma           0.5326643  1.9504309
(Intercept)     4.6361710  5.6290340
scale_t        -1.0089124 -0.7867609
```

## Cross Validation Results
### Execution Time
```
   user  system elapsed 
253.309  66.822 277.603 
```

### Result
```
$mean.scores
                  MAE      MSE  neg.CRPS neg.sCRPS
ar1_gaussian 1.414613 3.600801 1.0315382  1.368168
ar1_nig      1.382462 3.603605 0.9638413  1.337475
baseline     1.479771 4.021913 1.0863589  1.395721

$sd.scores
                     MAE        MSE    neg.CRPS   neg.sCRPS
ar1_gaussian 0.061899810 0.17722824 0.065286377 0.027664220
ar1_nig      0.297397813 0.92900599 0.258828449 0.110823356
baseline     0.003680223 0.01089356 0.002823487 0.001498412

```

## Summary of Times
```
ar1_gauss_original.elapsed   ar1_nig_original.elapsed   cv_time_original.elapsed 
                     2.802                     22.928                    277.603 
```
