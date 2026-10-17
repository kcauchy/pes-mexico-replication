# ==============================================================================
# 03_env_models.R: Environmental Panel Regressions (Table 5 Replications)
# ==============================================================================

source(here::here("R", "00_setup.R"))

message("\n>>> Loading Matched Points Panel Data...")
matched_df <- haven::read_dta(here::here("data", "raw", "matchedpnts-maha-wreplace_final.dta"))

# Verify data columns and types
matched_df <- matched_df %>%
  mutate(
    id      = as.factor(id),
    polyfid = as.factor(polyfid),
    state   = as.factor(state),
    year    = as.integer(year),
    vegcat  = as.factor(vegcat)
  )

# ------------------------------------------------------------------------------
# 1. Point Fixed Effects Models (Table 5, Columns 1 to 3)
# ------------------------------------------------------------------------------
message("--- Estimating Point Fixed Effects Models (Cols 1-3) ---")

# Col 1: Point FE, no time trend
# Stata: xtreg mndvi recipL lndry lnfull st_dev_rf hurricane i.year*i.state, fe i(id) robust cluster(polyfid)
m1_point <- feols(
  mndvi ~ recipL + lndry + lnfull + st_dev_rf + hurricane | 
    id + state^year,
  data = matched_df,
  cluster = ~polyfid
)

# Col 2: Point FE with time trend interaction
# Stata: xtreg mndvi time time_recipL lndry lnfull st_dev_rf hurricane i.year*i.state, fe i(id) robust cluster(polyfid)
m2_point <- feols(
  mndvi ~ time + time_recipL + lndry + lnfull + st_dev_rf + hurricane | 
    id + state^year,
  data = matched_df,
  cluster = ~polyfid
)

# Col 3: Point FE with cumulative payment years
# Stata: xtreg mndvi recipL recipL_recipyrs lndry lnfull st_dev_rf hurricane i.year*i.state, fe i(id) robust cluster(polyfid)
m3_point <- feols(
  mndvi ~ recipL + recipL_recipyrs + lndry + lnfull + st_dev_rf + hurricane | 
    id + state^year,
  data = matched_df,
  cluster = ~polyfid
)

# ------------------------------------------------------------------------------
# 2. Parcel Fixed Effects Models (Table 5, Columns 4 to 6)
# ------------------------------------------------------------------------------
message("--- Estimating Parcel Fixed Effects Models (Cols 4-6) ---")

# Subset post-2004 sample (year > 3 per Stata do-file)
matched_df_sub <- matched_df %>% filter(year > 3)

# Col 4: Parcel FE, controls, no time trend
# Stata: xtreg mndvi recipL mndvi2003 lnslope lnelev dloc5000 im_00 bos_mesofi sobreexp z_disp montana majindig i.vegcat lndry lnfull st_dev_rf hurricane i.year*i.state if year > 3, fe i(polyfid) robust cluster(polyfid)
m4_poly <- feols(
  mndvi ~ recipL + mndvi2003 + lnslope + lnelev + dloc5000 + im_00 + 
    bos_mesofi + sobreexp + z_disp + montana + majindig + i(vegcat) + 
    lndry + lnfull + st_dev_rf + hurricane | 
    polyfid + state^year,
  data = matched_df_sub,
  cluster = ~polyfid
)

# Col 5: Parcel FE with time trend interaction
# Stata: xtreg mndvi time time_recipL mndvi2003 lnslope lnelev dloc5000 im_00 bos_mesofi sobreexp z_disp montana majindig i.vegcat lndry lnfull st_dev_rf hurricane i.year*i.state if year > 3, fe i(polyfid) robust cluster(polyfid)
m5_poly <- feols(
  mndvi ~ time + time_recipL + mndvi2003 + lnslope + lnelev + dloc5000 + im_00 + 
    bos_mesofi + sobreexp + z_disp + montana + majindig + i(vegcat) + 
    lndry + lnfull + st_dev_rf + hurricane | 
    polyfid + state^year,
  data = matched_df_sub,
  cluster = ~polyfid
)

# Col 6: Parcel FE with cumulative payment years
# Stata: xtreg mndvi recipL recipL_recipyrs mndvi2003 lnslope lnelev dloc5000 im_00 bos_mesofi sobreexp z_disp montana majindig i.vegcat lndry lnfull st_dev_rf hurricane i.year*i.state if year > 3, fe i(polyfid) robust cluster(polyfid)
m6_poly <- feols(
  mndvi ~ recipL + recipL_recipyrs + mndvi2003 + lnslope + lnelev + dloc5000 + im_00 + 
    bos_mesofi + sobreexp + z_disp + montana + majindig + i(vegcat) + 
    lndry + lnfull + st_dev_rf + hurricane | 
    polyfid + state^year,
  data = matched_df_sub,
  cluster = ~polyfid
)

# ------------------------------------------------------------------------------
# 3. Compile and Export Table 5
# ------------------------------------------------------------------------------
message("\n--- Table 5 Estimation Results ---")

models_list <- list(
  "(1) Point FE"        = m1_point,
  "(2) Point Trend"     = m2_point,
  "(3) Point Years"     = m3_point,
  "(4) Parcel FE"       = m4_poly,
  "(5) Parcel Trend"    = m5_poly,
  "(6) Parcel Years"    = m6_poly
)

# Format summary table highlighting key policy parameters
tab5 <- modelsummary::modelsummary(
  models_list,
  coef_map = c(
    "recipL"          = "Beneficiary (recipL)",
    "time_recipL"     = "Time x Beneficiary",
    "recipL_recipyrs" = "Beneficiary x Years Enrolled"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
)

print(tab5)

# Save models and table output
saveRDS(models_list, here::here("output", "tables", "table5_models.rds"))
message("\n>>> Environmental estimations complete! Model objects saved to output/tables/table5_models.rds.")