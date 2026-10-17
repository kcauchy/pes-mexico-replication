# Replication Study: Environmental Effectiveness and Poverty Alleviation in Mexico's PES Program

[![R Version](https://img.shields.io/badge/R-%E2%89%A54.3.0-blue.svg)](https://www.r-project.org/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

An end-to-end econometric replication of:
> **Alix-Garcia, J. M., Sims, K. R., & Yañez-Pagans, P. (2015).**  
> *Only One Tree from Each Seed? Environmental Effectiveness and Poverty Alleviation in Mexico's Payments for Ecosystem Services Program.*  
> **American Economic Journal: Economic Policy**, 7(4), 1–40. [https://doi.org/10.1257/pol.20130139](https://doi.org/10.1257/pol.20130139)

---

## 📌 Project Overview & Identification Strategy

This study evaluates Mexico's *Pago por Servicios Ambientales Hidrológicos* (PSAH)—a nationwide Payments for Ecosystem Services (PES) program designed to simultaneously curb deforestation and alleviate rural poverty. 

Because program participation was non-random and driven by spatial and socioeconomic self-selection, empirical identification combines:
1. **Covariate Pre-Matching:** Nearest-neighbor Mahalanobis distance matching with replacement within administrative cohorts, balancing geographic slope, elevation, distance to markets, baseline NDVI vegetation index, and municipal poverty indices.
2. **High-Dimensional Panel Fixed Effects:** Difference-in-Differences (DiD) estimators absorbing point/household fixed effects ($\alpha_i$) alongside saturated state-by-year interaction fixed effects ($\alpha_{st}$), with standard errors clustered at the parcel or ejido level.

All models incorporate the authors' base-10 Inverse Hyperbolic Sine (IHS) transformation:
$$\text{IHS}(x) = \log_{10}\left(x + \sqrt{x^2 + 1}\right)$$

---

## 🔬 Core Replicated Results

| Table | Topic | Sample & Method | Key Policy Parameter | Replicated Estimate (SE) | Published Estimate |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **Table 2** | Environmental Covariate Balance | Matched points ($N=196,164$) | Imbens-Wooldridge Normalized Diff | All covariates $\le \Vert{}0.13\Vert{}$ | All covariates $\le \Vert{}0.13\Vert{}$ |
| **Table 4** | Socioeconomic Baseline Balance | Ejido panel ($N=1,096$ households) | Normalized Differences (2007) | Max $\Vert{}\Delta\Vert{} = 0.188$ | Controlled balance |
| **Table 5** | Avoided Deforestation (NDVI) | Point FE + State $\times$ Year FE | Beneficiary effect ($\beta_{\text{recipL}}$) | **+0.186\*\*\*** (0.072) | **+0.186\*\*\*** (0.072) |
| **Table 5** | Deforestation Dynamic Trend | Point FE + Time trend | Time $\times$ Beneficiary | **+0.026\*\*** (0.013) | **+0.026\*\*** (0.013) |
| **Table 8** | Living Standards (Durables) | Household FE, weighted | Beneficiary ($\beta_{\text{did}}$) | **+0.160** (0.123) | **+0.160** (0.123) |
| **Table 9** | Productive Capital: Cattle | Household FE, weighted | Cattle Accumulation ($\beta_{\text{did}}$) | **+0.045\*\*** (0.021) | **+0.045\*\*** (0.021) |
| **Table 10** | High School Attendance (15–17) | Individual FE, weighted | School Attendance ($\beta_{\text{did}}$) | **+0.103\*** (0.060) | **+0.103\*** (0.060) |
| **Table 10** | College Attendance (Poor 18–22)| Individual FE, weighted | Beneficiary $\times$ Baseline Poor | **+0.115\*** (0.064) | **+0.115\*** (0.064) |

*All standard errors clustered at parcel (`polyfid`) or community (`folio_conafor`) level.*

---

## 🗂️ Repository Architecture

```text
pes-mexico-replication/
├── README.md                  # Project overview and executive summary
├── Replication_Report.Rmd     # Master dynamic replication document
├── Replication_Report.html    # Compiled standalone HTML presentation report
├── pes-mexico-replication.Rproj # RStudio project environment
├── data/
│   ├── raw/                   # Raw Stata .dta datasets (download instructions below)
│   └── processed/             # Cleaned panels & long-format RDS files
├── R/
│   ├── 00_setup.R             # Master configuration, packages, and helper functions
│   ├── 01_data_prep.R         # Panel reshaping (wide-to-long) & demeaned interactions
│   ├── 02_matching.R          # Tables 2 & 4 baseline balance & normalized differences
│   ├── 03_env_models.R        # Table 5 dry-season NDVI panel fixed effects
│   ├── 04_socio_models.R      # Tables 8, 9, 12 living standards & asset accumulation
│   └── 05_school_models.R     # Tables 7 & 10 youth schooling enrollment fixed effects
└── output/
    └── tables/                # Standalone serialized model objects (.rds)

```

---

## 🚀 Reproduction Instructions

### 1. Data Access

The raw datasets are publicly archived through the [openICPSR Data Repository (openicpsr-112702)](https://www.google.com/search?q=https://www.openicpsr.org/openicpsr/project/112702/version/V1/view).

To replicate locally, download the repository and place the six original `.dta` files into `data/raw/`:

* `allpnts-foronetree_final.dta` (336.4 MB)
* `matchedpnts-maha-wreplace_final.dta` (85.4 MB)
* `onetree_data_eji021114v2.dta`
* `onetree_data_priv021114v2.dta`
* `onetree_dataedu_eji021114.dta`
* `onetree_dataedu_priv021114.dta`

### 2. Execution Order

Open `pes-mexico-replication.Rproj` in RStudio and execute the pipeline sequentially:

```R
# 1. Environment and helper functions
source("R/00_setup.R")

# 2. Panel ingestion and reshaping
source("R/01_data_prep.R")

# 3. Covariate matching diagnostics
source("R/02_matching.R")

# 4. Environmental panel models (Table 5)
source("R/03_env_models.R")

# 5. Socioeconomic asset models (Tables 8, 9, 12)
source("R/04_socio_models.R")

# 6. Education enrollment models (Tables 7, 10)
source("R/05_school_models.R")

```

### 3. Compiling the Dynamic Report

To generate the complete `Replication_Report.html` presentation with formatted regression tables and methodological notes:

```R
rmarkdown::render("Replication_Report.Rmd")

```

---

## 🛠️ Required Packages

* `tidyverse` (data wrangling & reshaping)
* `haven` (Stata `.dta` ingestion)
* `fixest` (high-dimensional fixed-effects estimation with clustering)
* `modelsummary` & `tinytable` (publication-ready regression summaries)
* `rmarkdown` & `knitr` (dynamic reporting)