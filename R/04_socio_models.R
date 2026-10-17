# ==============================================================================
# 04_socio_models.R: Household Socioeconomic Panels (Tables 8, 9, and 12)
# ==============================================================================

source(here::here("R", "00_setup.R"))

message("\n>>> Loading Processed Ejido Household Panel...")
eji_panel <- readRDS(here::here("data", "processed", "eji_hh_panel.rds"))

# Restrict to complete information sample per Stata do-file
eji_comp <- eji_panel %>%
  filter(complete_sample == 1) %>%
  mutate(
    folio_captura = as.factor(folio_captura),
    folio_conafor = as.factor(folio_conafor),
    year          = as.factor(year)
  )

# ==============================================================================
# 1. TABLE 8: Living Standards Indices (Household & Year Fixed Effects)
# ==============================================================================
message("\n--- Estimating Table 8: Consumer Durables & Housing Indices ---")

# Columns 1 & 3: Average effects on Consumer Durables and Housing
m_cons_avg <- feols(
  indexcons ~ benef_did | folio_captura + year,
  data = eji_comp,
  weights = ~weight,
  cluster = ~folio_conafor
)

m_home_avg <- feols(
  indexhome ~ benef_did | folio_captura + year,
  data = eji_comp,
  weights = ~weight,
  cluster = ~folio_conafor
)

# Columns 2 & 4: Ejidatario vs Non-Member Separation
m_cons_mem <- feols(
  indexcons ~ benef_did + benef_nonmem | folio_captura + year,
  data = eji_comp,
  weights = ~weight,
  cluster = ~folio_conafor
)

m_home_mem <- feols(
  indexhome ~ benef_did + benef_nonmem | folio_captura + year,
  data = eji_comp,
  weights = ~weight,
  cluster = ~folio_conafor
)

tab8_models <- list(
  "Durables (Avg)"  = m_cons_avg,
  "Durables (Dist)" = m_cons_mem,
  "Housing (Avg)"   = m_home_avg,
  "Housing (Dist)"  = m_home_mem
)

# Print Table 8 to Console
print(modelsummary::modelsummary(
  tab8_models,
  coef_map = c(
    "benef_did"    = "Beneficiary",
    "benef_nonmem" = "Beneficiary x Non-Member"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title = "Table 8: Impacts on Living Standards Indices",
  output = "markdown"
))

# Save Table 8 model objects
saveRDS(tab8_models, here::here("output", "tables", "table8_models.rds"))
message("Saved: output/tables/table8_models.rds")

# ==============================================================================
# 2. TABLE 9: Productive Investments (Household & Year Fixed Effects)
# ==============================================================================
message("\n--- Estimating Table 9: Productive Investment Assets ---")

prod_outcomes <- c("tot_ganado", "smallanimals", "liveinfra", "aginput", "agequip")
prod_names    <- c("Cattle", "Small Animals", "Livestock Infra", "Ag Inputs", "Ag Equipment")

tab9_models <- list()

for (i in seq_along(prod_outcomes)) {
  outcome <- prod_outcomes[i]
  label   <- prod_names[i]
  
  fml <- as.formula(paste(outcome, "~ benef_did | folio_captura + year"))
  
  tab9_models[[label]] <- feols(
    fml,
    data = eji_comp,
    weights = ~weight,
    cluster = ~folio_conafor
  )
}

# Print Table 9 to Console
print(modelsummary::modelsummary(
  tab9_models,
  coef_map = c("benef_did" = "Beneficiary"),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title = "Table 9: Impacts on Productive Investments",
  output = "markdown"
))

# Save Table 9 model objects
saveRDS(tab9_models, here::here("output", "tables", "table9_models.rds"))
message("Saved: output/tables/table9_models.rds")

# ==============================================================================
# 3. TABLE 12: Heterogeneity Across Poverty & Deforestation Risk
# ==============================================================================
message("\n--- Estimating Table 12: Heterogeneity in Productive Investments ---")

tab12_poverty <- list()
tab12_risk    <- list()

for (i in seq_along(prod_outcomes)) {
  outcome <- prod_outcomes[i]
  label   <- prod_names[i]
  
  # Panel A: Poverty interaction
  fml_pov <- as.formula(paste(outcome, "~ benef_did + benef_pov | folio_captura + year"))
  tab12_poverty[[label]] <- feols(
    fml_pov,
    data = eji_comp,
    weights = ~weight,
    cluster = ~folio_conafor
  )
  
  # Panel B: Deforestation risk interaction
  fml_risk <- as.formula(paste(outcome, "~ benef_did + benef_highrisk | folio_captura + year"))
  tab12_risk[[label]] <- feols(
    fml_risk,
    data = eji_comp,
    weights = ~weight,
    cluster = ~folio_conafor
  )
}

# Print Table 12 Panel A (Poverty) to Console
message("\n--- Table 12 Panel A: Poverty Heterogeneity ---")
print(modelsummary::modelsummary(
  tab12_poverty,
  coef_map = c(
    "benef_did" = "Beneficiary",
    "benef_pov" = "Beneficiary x Poor"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
))

# Print Table 12 Panel B (Deforestation Risk) to Console
message("\n--- Table 12 Panel B: Deforestation Risk Heterogeneity ---")
print(modelsummary::modelsummary(
  tab12_risk,
  coef_map = c(
    "benef_did"      = "Beneficiary",
    "benef_highrisk" = "Beneficiary x High Risk"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
))

# Save Table 12 model objects as a named list (both panels)
tab12_models <- list(poverty = tab12_poverty, risk = tab12_risk)
saveRDS(tab12_models, here::here("output", "tables", "table12_models.rds"))
message("Saved: output/tables/table12_models.rds")

message("\n>>> Script 04 complete! Tables 8, 9, and 12 saved individually to output/tables/.")