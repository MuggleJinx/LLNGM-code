# SRFT Model Fitting Results

**Date:** 2026-02-26

**Package Versions:**
- **ngme2:** 0.9.3

## Gaussian Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)         sex        bage 
    4.72623    -0.07079    -0.00919 

Models: 
$field1
  Model type: IID model
      No parameter.
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0212

$field2
  Model type: Matern
      alpha = 2 (fixed)
      kappa = 1.15
  Number of replicates: 500

  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.763

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.211


```

### SGLD Estimates
```
sigma (field1) kappa (field2) sigma (field2)          sigma fixed effect 1 fixed effect 2 fixed effect 3 
    0.02548866     1.17127420     0.78208953     0.21098695     4.06915414    -0.06986359    -0.00920378 
```

### SGLD Credible Intervals
```
                      lower        upper
sigma (field1)  0.020353375  0.030865296
kappa (field2)  1.102348629  1.256890952
sigma (field2)  0.713199666  0.870168243
sigma           0.206851653  0.214856587
fixed effect 1  3.997575003  4.137010226
fixed effect 2 -0.075510056 -0.063347620
fixed effect 3 -0.009511863 -0.009007607
```

## NIG Model
```
*** Ngme object ***

Fixed effects: 
(Intercept)         sex        bage 
     4.8550     -0.0800     -0.0105 

Models: 
$field1
  Model type: IID model
      No parameter.
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.0125

$field2
  Model type: Matern
      alpha = 2 (fixed)
      kappa = 1.14
  Number of replicates: 500

  Noise type: NIG
  Noise parameters: 
      mu = -0.0522
      sigma = 1.23
      nu = 0.0734

Measurement noise: 
  Noise type: NORMAL
  Noise parameters: 
      sigma = 0.179


```

### SGLD Estimates
```
sigma (field1) kappa (field2)    mu (field2) sigma (field2)    nu (field2)          sigma fixed effect 1 
    0.01574740     1.14917224    -0.05030032     1.26478468     0.07472447     0.17893978     4.11322551 
fixed effect 2 fixed effect 3 
   -0.08261573    -0.01053342 
```

### SGLD Credible Intervals
```
                     lower        upper
sigma (field1)  0.01192812  0.021548904
kappa (field2)  1.06547318  1.236587754
mu (field2)    -0.10315866 -0.001950579
sigma (field2)  1.13154849  1.388894099
nu (field2)     0.06073805  0.091983511
sigma           0.17521140  0.182319748
fixed effect 1  4.04970505  4.179136822
fixed effect 2 -0.08934146 -0.078620005
fixed effect 3 -0.01065735 -0.010372431
```

## Cross Validation Results
```
            MAE        MSE  neg.CRPS neg.sCRPS
gauss 0.1423666 0.04666888 0.1127857 0.2721769
nig   0.1309870 0.03752606 0.1000244 0.2057215
```

