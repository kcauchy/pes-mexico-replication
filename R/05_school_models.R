# ==============================================================================
# 05_school_models.R: Education Panel Models (Tables 7 and 10)
# ==============================================================================

source(here::here("R", "00_setup.R"))

message("\n>>> Loading Education Panel Datasets...")
edu_eji_raw  <- readRDS(here::here("data", "processed", "edu_eji_panel.rds"))
edu_priv_raw <- readRDS(here::here("data", "processed", "edu_priv_panel.rds"))

# ------------------------------------------------------------------------------
# 1. SAMPLE FILTERING & DEMEANED INTERACTIONS PER STATA DO-FILE
# ------------------------------------------------------------------------------
# Ejidos: restrict to complete information and matched sample
edu_eji <- edu_eji_raw %>%
  filter(complete_sample == 1, matchedcomp_sample == 1) %>%
  mutate(
    unique_code   = as.factor(unique_code),
    folio_conafor = as.factor(folio_conafor),
    year          = as.factor(year),
    
    # Re-center interactions within the matched complete sample
    mean_ben  = mean(benef_did, na.rm = TRUE),
    mean_pov  = mean(poor_matchcomp, na.rm = TRUE),
    mean_risk = mean(highrisk_matchcomp, na.rm = TRUE),
    benef_pov  = (benef_did - mean_ben) * (poor_matchcomp - mean_pov),
    benef_risk = (benef_did - mean_ben) * (highrisk_matchcomp - mean_risk)
  )

# Private properties: restrict to complete information sample
edu_priv <- edu_priv_raw %>%
  filter(complete_sample == 1) %>%
  mutate(
    unique_code   = as.factor(unique_code),
    folio_conafor = as.factor(folio_conafor),
    year          = as.factor(year),
    
    # Re-center interactions within the complete sample
    mean_ben  = mean(benef_did, na.rm = TRUE),
    mean_pov  = mean(poor_comp, na.rm = TRUE),
    mean_risk = mean(highrisk_comp, na.rm = TRUE),
    benef_pov  = (benef_did - mean_ben) * (poor_comp - mean_pov),
    benef_risk = (benef_did - mean_ben) * (highrisk_comp - mean_risk)
  )

# ------------------------------------------------------------------------------
# 2. TABLE 7: BASELINE (2007) SCHOOL ATTENDANCE BALANCE
# ------------------------------------------------------------------------------
message("\n--- Table 7: Baseline Education Descriptive Balance ---")

# Cohort baseline subsets
eji_1214_base <- edu_eji %>% filter(year == "2007", age >= 12 & age <= 14)
eji_1517_base <- edu_eji %>% filter(year == "2007", age >= 15 & age <= 17)
eji_1822_base <- edu_eji %>% filter(year == "2007", age >= 18 & age <= 22)
priv_1222_base <- edu_priv %>% filter(year == "2007", age >= 12 & age <= 22)

cohort_list_base <- list(
  "Ejidos (12-14)"  = eji_1214_base,
  "Ejidos (15-17)"  = eji_1517_base,
  "Ejidos (18-22)"  = eji_1822_base,
  "Private (12-22)" = priv_1222_base
)

tab7_balance <- lapply(names(cohort_list_base), function(nm) {
  df <- cohort_list_base[[nm]]
  c_vals <- df$student[df$ben_original == 0]
  t_vals <- df$student[df$ben_original == 1]
  
  mean_c <- mean(c_vals, na.rm = TRUE)
  sd_c   <- sd(c_vals, na.rm = TRUE)
  mean_t <- mean(t_vals, na.rm = TRUE)
  sd_t   <- sd(t_vals, na.rm = TRUE)
  nd     <- calc_norm_diff(df$student, df$ben_original)
  
  tibble(
    Sample       = nm,
    Control_Mean = round(mean_c, 3),
    Control_SD   = round(sd_c, 3),
    Treat_Mean   = round(mean_t, 3),
    Treat_SD     = round(sd_t, 3),
    Norm_Diff    = round(nd, 3),
    N_Obs        = nrow(df)
  )
}) %>% bind_rows()

print(tab7_balance)
saveRDS(tab7_balance, here::here("output", "tables", "table7_school_balance.rds"))
message("Saved: output/tables/table7_school_balance.rds")

# ------------------------------------------------------------------------------
# 3. TABLE 10: SCHOOL ATTENDANCE PANEL ESTIMATIONS (INDIVIDUAL & YEAR FE)
# ------------------------------------------------------------------------------
message("\n--- Estimating Table 10: School Attendance Panel Fixed Effects ---")

# Define cohorts for panel regressions
cohorts <- list(
  "(1) Ejidos 12-14" = edu_eji %>% filter(age >= 12 & age <= 14),
  "(2) Ejidos 15-17" = edu_eji %>% filter(age >= 15 & age <= 17),
  "(3) Ejidos 18-22" = edu_eji %>% filter(age >= 18 & age <= 22),
  "(4) Priv 12-22"   = edu_priv %>% filter(age >= 12 & age <= 22)
)

tab10_avg  <- list()
tab10_pov  <- list()
tab10_risk <- list()

for (nm in names(cohorts)) {
  sub_df <- cohorts[[nm]]
  
  # Stata: Ejidos cluster at id_prop (folio_conafor); Private uses robust (cluster at id/unique_code)
  clust_var <- if (grepl("Ejidos", nm)) ~folio_conafor else ~unique_code
  
  # Panel A: Average Impacts
  tab10_avg[[nm]] <- feols(
    student ~ benef_did | unique_code + year,
    data = sub_df,
    weights = ~weight,
    cluster = clust_var
  )
  
  # Panel B: Poverty Heterogeneity
  tab10_pov[[nm]] <- feols(
    student ~ benef_did + benef_pov | unique_code + year,
    data = sub_df,
    weights = ~weight,
    cluster = clust_var
  )
  
  # Panel C: Deforestation Risk Heterogeneity
  tab10_risk[[nm]] <- feols(
    student ~ benef_did + benef_risk | unique_code + year,
    data = sub_df,
    weights = ~weight,
    cluster = clust_var
  )
}

# ------------------------------------------------------------------------------
# 4. PRINT FORMATTED TABLE 10 PANELS TO CONSOLE
# ------------------------------------------------------------------------------
message("\n--- Table 10 Panel A: Average Impacts on School Attendance ---")
print(modelsummary::modelsummary(
  tab10_avg,
  coef_map = c("benef_did" = "Beneficiary"),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
))

message("\n--- Table 10 Panel B: Poverty Heterogeneity ---")
print(modelsummary::modelsummary(
  tab10_pov,
  coef_map = c(
    "benef_did" = "Beneficiary",
    "benef_pov" = "Beneficiary x Poor"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
))

message("\n--- Table 10 Panel C: Deforestation Risk Heterogeneity ---")
print(modelsummary::modelsummary(
  tab10_risk,
  coef_map = c(
    "benef_did"  = "Beneficiary",
    "benef_risk" = "Beneficiary x High Risk"
  ),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output = "markdown"
))

# Save Table 10 model objects as a structured list
tab10_models <- list(avg = tab10_avg, poverty = tab10_pov, risk = tab10_risk)
saveRDS(tab10_models, here::here("output", "tables", "table10_school_models.rds"))
message("\nSaved: output/tables/table10_school_models.rds")

message("\n>>> Script 05 complete! Tables 7 and 10 saved individually to output/tables/.")