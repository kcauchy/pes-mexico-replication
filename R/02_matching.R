# ==============================================================================
# 02_matching.R: Baseline descriptive statistics and normalized differences
# ==============================================================================

source(here::here("R", "00_setup.R"))

message("\n>>> Replicating Environmental Covariate Balance (Table 2)...")

# 1. Ingest environmental points datasets
raw_all_pnts <- haven::read_dta(here::here("data", "raw", "allpnts-foronetree_final.dta"))
matched_pnts <- haven::read_dta(here::here("data", "raw", "matchedpnts-maha-wreplace_final.dta"))

# Baseline environmental variables from Points-Matching-vfinal.do
env_vars <- c(
  "pendien",    # Slope
  "mex_mde",    # Elevation
  "dloc5000",   # Distance to nearest locality > 5000 pop
  "im_00",      # Municipal poverty index (2000)
  "common",     # Common property indicator (ejido/community)
  "sobreexp",   # Overexploited aquifer
  "z_disp",     # Water availability/scarcity
  "montana",    # Priority mountain area
  "anp",        # Protected natural area
  "majindig",   # Majority indigenous population
  "bos_mesofi", # Cloud forest (bosque mesofilo)
  "mndvi"       # Baseline NDVI (vegetation index)
)

# ------------------------------------------------------------------------------
# 2. Environmental Normalized Differences (Full Unmatched vs Matched Sample)
# ------------------------------------------------------------------------------
# Filter to baseline cross-section within applicant boundaries (outside == 0)
env_applicant_baseline <- raw_all_pnts %>%
  filter(outside == 0, year == 3)

# Filter matched sample at baseline
env_matched_baseline <- matched_pnts %>%
  filter(outside == 0, year == 3)

# Compute Normalized Differences for Unmatched Applicant Sample
norm_diff_unmatched <- sapply(env_vars, function(v) {
  if (v %in% names(env_applicant_baseline)) {
    calc_norm_diff(env_applicant_baseline[[v]], env_applicant_baseline$treated)
  } else {
    NA_real_
  }
})

# Compute Normalized Differences for Matched Mahalanobis Sample
norm_diff_matched <- sapply(env_vars, function(v) {
  if (v %in% names(env_matched_baseline)) {
    calc_norm_diff(env_matched_baseline[[v]], env_matched_baseline$treated)
  } else {
    NA_real_
  }
})

# Compile Table 2 Environmental Covariate Balance summary
env_balance_table <- tibble(
  Variable = env_vars,
  NormDiff_Unmatched = round(norm_diff_unmatched, 2),
  NormDiff_Matched   = round(norm_diff_matched, 2)
)

print(env_balance_table)
saveRDS(env_balance_table, here::here("output", "tables", "table2_env_balance.rds"))

# ------------------------------------------------------------------------------
# 3. Socioeconomic Baseline Balance (Table 4 - Complete Sample)
# ------------------------------------------------------------------------------
message("\n>>> Replicating Socioeconomic Covariate Balance (Table 4)...")

eji_panel <- readRDS(here::here("data", "processed", "eji_hh_panel.rds"))

# Restrict to baseline year 2007 and complete sample
eji_baseline_comp <- eji_panel %>%
  filter(year == 2007, complete_sample == 1)

socio_vars <- c("index_food", "indexcons", "indexhome", "tot_ganado", 
                "smallanimals", "liveinfra", "aginput", "agequip", 
                "elevmn", "slopemn", "d5000", "mpov05", "area", "ejidatario", "hhsize")

# Compute Baseline Means, SDs, and Normalized Differences by ben_original
socio_summary <- lapply(socio_vars, function(v) {
  x <- eji_baseline_comp[[v]]
  treat <- eji_baseline_comp$ben_original
  
  mean_c <- mean(x[treat == 0], na.rm = TRUE)
  sd_c   <- sd(x[treat == 0], na.rm = TRUE)
  mean_t <- mean(x[treat == 1], na.rm = TRUE)
  sd_t   <- sd(x[treat == 1], na.rm = TRUE)
  nd     <- calc_norm_diff(x, treat)
  
  tibble(
    Variable = v,
    Control_Mean = round(mean_c, 3),
    Control_SD   = round(sd_c, 3),
    Treat_Mean   = round(mean_t, 3),
    Treat_SD     = round(sd_t, 3),
    Norm_Diff    = round(nd, 3)
  )
}) %>% bind_rows()

print(socio_summary, n = 15)
saveRDS(socio_summary, here::here("output", "tables", "table4_socio_balance.rds"))

message("\n>>> Covariate balance checking complete! Balance tables saved to output/tables/.")