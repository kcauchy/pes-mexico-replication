# ==============================================================================
# 00_setup.R: Master configuration, libraries, and custom functions
# ==============================================================================

# 1. Load required libraries
suppressPackageStartupMessages({
  library(here)         # Relative project paths
  library(haven)        # Stata imports
  library(tidyverse)    # Data wrangling (dplyr, tidyr, purrr, ggplot2)
  library(fixest)       # Fast fixed-effects (feols)
  library(MatchIt)      # Matching estimation
  library(modelsummary) # Publication tables
})

# 2. Define authors' base-10 Inverse Hyperbolic Sine (IHS) transformation
# Standard IHS uses log(x + sqrt(x^2 + 1)); the authors explicitly use log10
ihs <- function(x) {
  log10(x + sqrt(x^2 + 1))
}

# 3. Define Imbens & Wooldridge (2007) Normalized Difference function
calc_norm_diff <- function(x, treat) {
  x1 <- x[treat == 1]
  x0 <- x[treat == 0]
  
  mean_diff <- mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)
  pooled_sd <- sqrt(var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE))
  
  return(mean_diff / pooled_sd)
}

# 4. Verify relative root and data availability
message("Project root set to: ", here::here())

raw_files <- list.files(here::here("data", "raw"), pattern = "\\.dta$")
message("Raw Stata datasets found in data/raw/:")
print(raw_files)