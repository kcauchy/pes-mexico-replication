# ==============================================================================
# 01_data_prep.R: Ingestion, IHS transformations, panel reshaping & interactions
# ==============================================================================

# Ensure core setup, functions, and packages are loaded
source(here::here("R", "00_setup.R"))

message("\n>>> Starting Data Ingestion and Preparation...")

# ------------------------------------------------------------------------------
# 1. EJIDO HOUSEHOLD DATA (onetree_data_eji021114v2.dta)
# ------------------------------------------------------------------------------
message("--- Processing Ejido Households ---")
raw_eji_hh <- haven::read_dta(here::here("data", "raw", "onetree_data_eji021114v2.dta"))

# 1.1 Complete Information Indicator per Stata do-file
raw_eji_hh <- raw_eji_hh %>%
  mutate(
    complete_sample = if_else(
      !is.na(index_food) & !is.na(indexcons2007) & !is.na(indexcons2011) &
        !is.na(indexhome2007) & !is.na(indexhome2011) &
        !is.na(tot_ganado2007) & !is.na(tot_ganado2011) &
        !is.na(smallanimals2007) & !is.na(smallanimals2011) &
        !is.na(liveinfra2007) & !is.na(liveinfra2011) &
        !is.na(aginput2007) & !is.na(aginput2011) &
        !is.na(agequip2007) & !is.na(agequip2011) &
        !is.na(d5000) & !is.na(hhsize) & !is.na(mpov05) &
        !is.na(elevmn) & !is.na(ejidatario) & !is.na(highrisk),
      1L, 0L
    )
  )

# 1.2 Baseline IHS transformations on cross-sectional covariates
ihs_covars_eji <- c("area", "d5000", "elevmn", "slopemn", "tot_ganado2007", 
                    "tot_ganado2011", "smallanimals2007", "smallanimals2011", 
                    "index_food", "mpov05")

eji_hh_clean <- raw_eji_hh %>%
  mutate(across(all_of(intersect(ihs_covars_eji, names(.))), ~ ihs(.x)))

# 1.3 Reshape wide panel variables to long (2007 vs 2011)
# Note: Handles wide variables if unstacked
wide_stems_eji <- c("indexcons", "indexhome", "tot_ganado", "smallanimals", 
                    "liveinfra", "aginput", "agequip")

# Verify if reshape is needed or if dataset has year column
if (!"year" %in% names(eji_hh_clean)) {
  eji_panel <- eji_hh_clean %>%
    pivot_longer(
      cols = matches("^(indexcons|indexhome|tot_ganado|smallanimals|liveinfra|aginput|agequip)(2007|2011)$"),
      names_to = c(".value", "year"),
      names_pattern = "(.*)(2007|2011)$"
    ) %>%
    mutate(year = as.integer(year))
} else {
  eji_panel <- eji_hh_clean
}

# 1.4 Panel Treatment Coding, Demeaned Interactions, and Identifiers
eji_panel <- eji_panel %>%
  mutate(
    # In DiD panel regressions: benef = 0 in baseline (2007), 1 in 2011 if treated
    ben_original = benef,
    benef_did = if_else(year == 2007, 0, benef),
    
    # Non-member indicator
    nonmem = if_else(ejidatario == 1, 0, 1),
    
    # Poverty baseline threshold
    poor = as.integer(indexall2007 < median(indexall2007, na.rm = TRUE)),
    
    # Continuous payment coding: 0 at baseline or for untreated
    lpayment_ph_clean = case_when(
      year == 2007 ~ 0,
      ben_original == 0 & year == 2011 ~ 0,
      TRUE ~ lpayment_ph
    ),
    
    # Apply IHS transformation to panel outcome indices
    indexcons_ihs = ihs(indexcons),
    indexhome_ihs = ihs(indexhome)
  )

# Demeaned interactions for panel fixed effects
mean_benef_eji <- mean(eji_panel$benef_did, na.rm = TRUE)
mean_pov_eji   <- mean(eji_panel$poor, na.rm = TRUE)
mean_risk_eji  <- mean(eji_panel$highrisk, na.rm = TRUE)
mean_non_eji   <- mean(eji_panel$nonmem, na.rm = TRUE)

eji_panel <- eji_panel %>%
  mutate(
    benef_nonmem   = (benef_did - mean_benef_eji) * (nonmem - mean_non_eji),
    benef_non      = benef_did * nonmem,
    benef_ejid     = benef_did * ejidatario,
    benef_pov      = (benef_did - mean_benef_eji) * (poor - mean_pov_eji),
    benef_highrisk = (benef_did - mean_benef_eji) * (highrisk - mean_risk_eji),
    lpay_ejid      = lpayment_ph_clean * ejidatario,
    lpay_pov       = lpayment_ph_clean * poor,
    lpay_highrisk  = lpayment_ph_clean * highrisk
  )

saveRDS(eji_panel, here::here("data", "processed", "eji_hh_panel.rds"))
message("Saved: data/processed/eji_hh_panel.rds (", nrow(eji_panel), " rows)")


# ------------------------------------------------------------------------------
# 2. PRIVATE HOUSEHOLD DATA (onetree_data_priv021114v2.dta)
# ------------------------------------------------------------------------------
message("\n--- Processing Private Households ---")
raw_priv_hh <- haven::read_dta(here::here("data", "raw", "onetree_data_priv021114v2.dta"))

# 2.1 Complete Information Indicator
raw_priv_hh <- raw_priv_hh %>%
  mutate(
    complete_sample = if_else(
      !is.na(index_food) & !is.na(indexcons2007) & !is.na(indexcons2011) &
        !is.na(indexhome2007) & !is.na(indexhome2011) &
        !is.na(tot_ganado2007) & !is.na(tot_ganado2011) &
        !is.na(smallanimals2007) & !is.na(smallanimals2011) &
        !is.na(liveinfra2007) & !is.na(liveinfra2011) &
        !is.na(aginput2007) & !is.na(aginput2011) &
        !is.na(agequip2007) & !is.na(agequip2011) &
        !is.na(d5000) & !is.na(hhsize) & !is.na(mpov05) & !is.na(elevmn),
      1L, 0L
    ),
    poor = as.integer(indexall2007 < median(indexall2007, na.rm = TRUE))
  )

# 2.2 Baseline IHS transformations
ihs_covars_priv <- c("area", "d5000", "elevmn", "slopemn", "index_food", "mpov05")
priv_hh_clean <- raw_priv_hh %>%
  mutate(across(all_of(intersect(ihs_covars_priv, names(.))), ~ ihs(.x)))

# 2.3 Reshape wide panel variables to long (2007 vs 2011)
priv_panel <- priv_hh_clean %>%
  pivot_longer(
    cols = matches("^(tot_ganado|smallanimals|liveinfra|aginput|agequip|indexcons|indexhome)(2007|2011)$"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)(2007|2011)$"
  ) %>%
  mutate(
    year = as.integer(year),
    ben_original = benef,
    benef_did = if_else(year == 2007, 0, benef),
    
    # Continuous payment coding
    lpayment_ph_clean = case_when(
      year == 2007 ~ 0,
      ben_original == 0 & year == 2011 ~ 0,
      TRUE ~ lpayment_ph
    ),
    
    # Apply IHS transformation to outcomes
    indexcons_ihs   = ihs(indexcons),
    indexhome_ihs   = ihs(indexhome),
    tot_ganado_ihs  = ihs(tot_ganado),
    smallanimals_ihs = ihs(smallanimals)
  )

# Demeaned interactions
mean_benef_priv <- mean(priv_panel$benef_did, na.rm = TRUE)
mean_pov_priv   <- mean(priv_panel$poor, na.rm = TRUE)
mean_risk_priv  <- mean(priv_panel$highrisk, na.rm = TRUE)

priv_panel <- priv_panel %>%
  mutate(
    benef_pov      = (benef_did - mean_benef_priv) * (poor - mean_pov_priv),
    benef_highrisk = (benef_did - mean_benef_priv) * (highrisk - mean_risk_priv),
    lpay_pov       = lpayment_ph_clean * poor,
    lpay_highrisk  = lpayment_ph_clean * highrisk
  )

saveRDS(priv_panel, here::here("data", "processed", "priv_hh_panel.rds"))
message("Saved: data/processed/priv_hh_panel.rds (", nrow(priv_panel), " rows)")


# ------------------------------------------------------------------------------
# 3. EDUCATION DATASETS (EJIDOS & PRIVATE)
# ------------------------------------------------------------------------------
message("\n--- Processing Education Panels ---")

# 3.1 Ejido Education Data
raw_edu_eji <- haven::read_dta(here::here("data", "raw", "onetree_dataedu_eji021114.dta"))

edu_eji_panel <- raw_edu_eji %>%
  mutate(unique_code = paste0(folio_captura, as.character(num_miembro))) %>%
  pivot_longer(
    cols = c(student2007, student2011),
    names_to = "year",
    names_prefix = "student",
    values_to = "student"
  ) %>%
  mutate(
    year = as.integer(year),
    ben_original = benef,
    benef_did = if_else(year == 2007, 0, benef),
    lpayment_ph_clean = case_when(
      year == 2007 ~ 0,
      ben_original == 0 & year == 2011 ~ 0,
      TRUE ~ lpayment_ph
    )
  )

# Interactions
mean_ben_edueji <- mean(edu_eji_panel$benef_did, na.rm = TRUE)
mean_pov_edueji <- mean(edu_eji_panel$poor_matchcomp, na.rm = TRUE)
mean_rsk_edueji <- mean(edu_eji_panel$highrisk_matchcomp, na.rm = TRUE)

edu_eji_panel <- edu_eji_panel %>%
  mutate(
    benef_eji  = benef_did * ejidatario,
    benef_pov  = (benef_did - mean_ben_edueji) * (poor_matchcomp - mean_pov_edueji),
    benef_risk = (benef_did - mean_ben_edueji) * (highrisk_matchcomp - mean_rsk_edueji),
    lpay_eji   = lpayment_ph_clean * ejidatario,
    lpay_pov   = lpayment_ph_clean * poor_matchcomp,
    lpay_risk  = lpayment_ph_clean * highrisk_matchcomp
  )

saveRDS(edu_eji_panel, here::here("data", "processed", "edu_eji_panel.rds"))
message("Saved: data/processed/edu_eji_panel.rds (", nrow(edu_eji_panel), " rows)")

# 3.2 Private Education Data
raw_edu_priv <- haven::read_dta(here::here("data", "raw", "onetree_dataedu_priv021114.dta"))

edu_priv_panel <- raw_edu_priv %>%
  mutate(unique_code = paste0(folio_captura, as.character(num_miembro))) %>%
  pivot_longer(
    cols = c(student2007, student2011),
    names_to = "year",
    names_prefix = "student",
    values_to = "student"
  ) %>%
  mutate(
    year = as.integer(year),
    ben_original = benef,
    benef_did = if_else(year == 2007, 0, benef),
    lpayment_ph_clean = case_when(
      year == 2007 ~ 0,
      ben_original == 0 & year == 2011 ~ 0,
      TRUE ~ lpayment_ph
    )
  )

mean_ben_edupriv <- mean(edu_priv_panel$benef_did, na.rm = TRUE)
mean_pov_edupriv <- mean(edu_priv_panel$poor_comp, na.rm = TRUE)
mean_rsk_edupriv <- mean(edu_priv_panel$highrisk_comp, na.rm = TRUE)

edu_priv_panel <- edu_priv_panel %>%
  mutate(
    benef_pov  = (benef_did - mean_ben_edupriv) * (poor_comp - mean_pov_edupriv),
    benef_risk = (benef_did - mean_ben_edupriv) * (highrisk_comp - mean_rsk_edupriv),
    lpay_pov   = lpayment_ph_clean * poor_comp,
    lpay_risk  = lpayment_ph_clean * highrisk_comp
  )

saveRDS(edu_priv_panel, here::here("data", "processed", "edu_priv_panel.rds"))
message("Saved: data/processed/edu_priv_panel.rds (", nrow(edu_priv_panel), " rows)")

message("\n>>> Data preparation complete! All panels saved to data/processed/.")