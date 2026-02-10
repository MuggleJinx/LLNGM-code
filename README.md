# LLNGM-code: R Scripts for LLnGM Paper

This repository contains the R code to reproduce the results in the paper:  
**"A Unified, Efficient, and Non-Gaussian Statistical Modeling Framework"** *Authors: David Bolin, Xiaotian Jin, Alexandre B. Simas, and Jonas Wallin.*

## Project Overview

This research introduces **Linear Latent Non-Gaussian Models (LLnGMs)**, a framework that extends traditional Latent Gaussian Models (LGMs) to capture skewness and heavy tails using the Generalized Hyperbolic (GH) distribution family.

This repository specifically focuses on:
1.  **Applications**: Implementing real-world data analysis (Longitudinal, Spatial, and Spatio-temporal).
2.  **Software Comparison**: Benchmarking the performance of our R package `ngme2` against `Stan` and `TMB`.

---

## Repository Structure

* `applications/`: Reproduction scripts for Section 5:
    * `grasshopper.R`: Wildlife population dynamics (NIG-AR1).
    * `kidney_function.R`: Longitudinal eGFR data for 22,910 patients.
    * `precipitation.R`: Non-stationary spatial modeling of US climate data.
    * `wind_bivariate.R`: Bivariate spatio-temporal modeling of wind vectors.
* `comparison/`: Benchmark scripts for Section 6:
    * `benchmark_ar1_nig.R`: Compares `ngme2` vs. `Stan` vs. `TMB` (Table 7).
* `data/`: Scripts for data preprocessing and links to public datasets (e.g., NCEP/NCAR).

---

## Software: ngme2

The core framework is implemented in the `ngme2` R package. It provides a comprehensive set of tools for modeling non-Gaussian processes with a high-performance C++ backend.

- **Official Website:** [https://davidbolin.github.io/ngme2/](https://davidbolin.github.io/ngme2/)
- **GitHub Repository:** [https://github.com/davidbolin/ngme2](https://github.com/davidbolin/ngme2)

### Installation

You can install the **stable version** of `ngme2` directly from the official repository:

```R
install.packages("ngme2", repos = "[https://davidbolin.github.io/ngme2/](https://davidbolin.github.io/ngme2/)")
```
