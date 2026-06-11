# Spatio-Temporal Generalized Linear Modeling 
of Climatic Influences on Malaria Incidence 
in Ghana

## Overview

This repository contains the complete R code 
for the MSc thesis titled **"Spatio-Temporal 
Generalized Linear Modeling of Climatic 
Influences on Malaria Incidence in Ghana 
(2020--2024)"**. The analysis applies a 
Bayesian spatio-temporal Negative Binomial 
model estimated via Integrated Nested Laplace 
Approximation to examine the lagged effects 
of nine climatic predictors on monthly malaria 
incidence across Ghana's 16 administrative 
regions.

---

## Repository Contents

| File | Description |
|---|---|
| `malaria_spatio_temporal_analysis.R` | Complete analysis pipeline |
| `README.md` | Repository documentation |

---

## Analysis Pipeline

The script proceeds through the following 
sequential stages:

1. **Data Loading and Merging** — Malaria 
incidence data from DHIMS-2 and climatic 
data from Visual Crossing merged by region, 
month, and year

2. **Exploratory Visualisation** — Monthly 
malaria incidence and rainfall plots for 
all 16 regions

3. **Lag Determination** — Pearson 
correlation analysis for nine climatic 
predictors at lags 0 to 3 months

4. **Multicollinearity Diagnostics** — 
Two-stage procedure comprising inter-lag 
correlation screening and Generalised 
Variance Inflation Factor analysis

5. **Poisson and Negative Binomial Models** 
— Overdispersion testing, model fitting, 
and Poisson versus Negative Binomial 
comparison

6. **Bayesian Spatio-Temporal Model** — 
INLA model with CAR spatial prior, RW1 
temporal prior, and spatio-temporal 
interaction term

7. **Prior Sensitivity Analysis** — Three 
prior sensitivity tests and two structural 
specification tests with WAIC, DIC, and 
Log CPO comparison

8. **CPO Model Validation** — Conditional 
Predictive Ordinate statistics, PIT 
histogram, and Kolmogorov-Smirnov 
calibration test

9. **Risk Mapping** — Annual posterior 
relative risk maps for 2020--2024

10. **Exceedance Probability Maps** — 
Posterior probability that RR exceeds 
1.5 per region per year

11. **LISA Hotspot Detection** — Local 
Indicators of Spatial Association cluster 
maps for 2020--2024

12. **Spatio-Temporal Diagnostics** — 
Fitted versus observed plot, posterior 
temporal random effect, and posterior 
spatial random effect map

---

## Requirements

### R Version
R version 4.0.0 or higher

### Required Packages

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "lubridate",
  "ggplot2",
  "patchwork",
  "MASS",
  "AER",
  "car",
  "corrplot",
  "spdep",
  "sf",
  "janitor",
  "stringr",
  "RColorBrewer",
  "tmap",
  "classInt"
))

# Install INLA separately
install.packages("INLA", 
  repos = "https://inla.r-inla-download.org/R/stable")
```

---

## Data Requirements

The following data files are required 
to run the analysis:

| File | Description | Source |
|---|---|---|
| `PASCAL DATA.xlsx` | Monthly malaria incidence by region | DHIMS-2, Ghana Health Service |
| `CLIMATIC 2020.xls` | Daily climatic data 2020 | Visual Crossing Weather API |
| `CLIMATIC 2021.xls` | Daily climatic data 2021 | Visual Crossing Weather API |
| `CLIMATIC 2022.xlsx` | Daily climatic data 2022 | Visual Crossing Weather API |
| `CLIMATIC 2023.xlsx` | Daily climatic data 2023 | Visual Crossing Weather API |
| `CLIMATIC 2024.xlsx` | Daily climatic data 2024 | Visual Crossing Weather API |
| `District_272.shp` | Ghana administrative regions shapefile | Ghana Statistical Service |

---

## How to Run

1. Clone the repository:
```bash
git clone https://github.com/YourUsername/
Malaria-Spatio-Temporal-Ghana.git
```

2. Place all data files in the same 
directory as the R script

3. Set your working directory in R:
```r
setwd("path/to/your/folder")
```

4. Run the complete script:
```r
source("malaria_spatio_temporal_analysis.R")
```

---

## Outputs

Running the script produces the following 
output files:

| File | Description |
|---|---|
| `nb_diagnostic_plots.png` | Residual diagnostic plots |
| `pit_histogram.png` | PIT calibration histogram |
| `coefficient_stability.png` | Prior sensitivity plot |
| `rr_maps_2020_2024.png` | Annual relative risk maps |
| `exceedance_maps_2020_2024.png` | Exceedance probability maps |
| `lisa_maps_2020_2024.png` | LISA cluster maps |
| `spatio_temporal_diagnostics.png` | Model diagnostic plots |
| `model_comparison_full.csv` | WAIC DIC Log CPO comparison |
| `cpo_by_region.csv` | CPO diagnostics by region |
| `cpo_by_year.csv` | CPO diagnostics by year |
| `fixed_effects.csv` | Posterior fixed effect estimates |
| `hyperparameters.csv` | Posterior hyperparameter estimates |

---

## Parameter Settings

All prior specifications and tuning 
decisions are documented directly in 
the R script with inline comments. 
Key parameter settings:

| Parameter | Value | Justification |
|---|---|---|
| Spatial PC prior | sigma = 0.5, alpha = 0.01 | Weakly informative |
| Temporal PC prior | sigma = 0.1, alpha = 0.01 | Weakly informative |
| Interaction PC prior | sigma = 0.5, alpha = 0.01 | Weakly informative |
| NB dispersion prior | LogGamma(1, 0.00005) | Weakly informative |
| INLA strategy | Gaussian approximation | Computational efficiency |
| Multicollinearity threshold | VIF > 10 | Fox 2016 |
| Lag correlation threshold | r > 0.70 | Dormann 2013 |

---

## Author

**Pascal Antwi**  
MSc Statistics  
Strathmore University  
June 2026  

---
 
## Citation

If you use this code please cite:

Antwi, P. (2026). Spatio-Temporal
Generalized Linear Modeling of Climatic
Influences on Malaria Incidence in Ghana.
MSc Thesis, Strathmore University

---

## License

This code is made available for 
academic and research purposes. 
Please cite appropriately if used 
in your own work.


