rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(tidyverse, lfe, stargazer, conflicted)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

## Get data

df <- read_rds("data/data_main.rds") %>%
  filter(year > 1960) %>%
  select(
    flats_constr_public_pc,
    treated_median_resid_1963,
    incr_flats_after71_pct,
    year, county.id, post, Bezirk
  ) %>%
  mutate(period_bezirk = paste0(year, "_", Bezirk))

## Define specific variables and subsets ##

dv <- "flats_constr_public_pc"
tv <- "treated_median_resid_1963"

# Create full dataset with treatment interaction term
df_full <- df %>%
  mutate(
    treat_temp = .[[tv]],
    treat_temp_post = treat_temp * post
  )

# Create dataset removing the outlier county * year observation
df_rem_outlier <- df %>%
  filter(!incr_flats_after71_pct == max(df$incr_flats_after71_pct, na.rm = TRUE)) %>%
  mutate(
    treat_temp = .[[tv]],
    treat_temp_post = treat_temp * post
  )

## Estimate required models directly ##

# --- Full Sample Models ---
# Model 1: No FE (Original Model 1)
model1_full <- lfe::felm(as.formula(paste0(dv, " ~ post + treat_temp + treat_temp_post | 0 | 0 | county.id")), data = df_full)
# Model 2: 2WFE (Year + County) (Original Model 3)
model2_full <- lfe::felm(as.formula(paste0(dv, " ~ treat_temp_post | year + county.id | 0 | county.id")), data = df_full)
# Model 3: 2WFE (Period*Bezirk + County) (Original Model 5)
model3_full <- lfe::felm(as.formula(paste0(dv, " ~ treat_temp_post | period_bezirk + county.id | 0 | county.id")), data = df_full)

# --- Remove Outlier Sample Models ---
# Model 4: No FE (Original Model 1)
model1_rem <- lfe::felm(as.formula(paste0(dv, " ~ post + treat_temp + treat_temp_post | 0 | 0 | county.id")), data = df_rem_outlier)
# Model 5: 2WFE (Year + County) (Original Model 3)
model2_rem <- lfe::felm(as.formula(paste0(dv, " ~ treat_temp_post | year + county.id | 0 | county.id")), data = df_rem_outlier)
# Model 6: 2WFE (Period*Bezirk + County) (Original Model 5)
model3_rem <- lfe::felm(as.formula(paste0(dv, " ~ treat_temp_post | period_bezirk + county.id | 0 | county.id")), data = df_rem_outlier)

## Create Model List for Stargazer ##
# Order: Full (No FE, 2WFE, Per*Bez FE), Rem Outlier (No FE, 2WFE, Per*Bez FE)
mlist <- list(model1_full, model2_full, model3_full, model1_rem, model2_rem, model3_rem)

## Calculate DV Mean/SD for add.lines ##
# Stargazer doesn't add these automatically
dv_mean_full <- mean(df_full[[dv]], na.rm = TRUE)
dv_sd_full <- sd(df_full[[dv]], na.rm = TRUE)
dv_mean_rem <- mean(df_rem_outlier[[dv]], na.rm = TRUE)
dv_sd_rem <- sd(df_rem_outlier[[dv]], na.rm = TRUE)

## Generate LaTeX table
stargazer(mlist,
  keep = "treat_temp_post",
  covariate.labels = "Petitions * post 1971",
  omit.table.layout = "s",
  add.lines = list(
    c("County FE", "No", "Yes", "Yes", "No", "Yes", "Yes"),
    c("Period FE", "No", "Yes", "No", "No", "Yes", "No"),
    c("Period * district FE", "No", "No", "Yes", "No", "No", "Yes"),
    c("Sample", rep(c("Full", "95\\%"), each = 3)),
    c("DV mean", round(c(rep(dv_mean_full, 3), rep(dv_mean_rem, 3)), 3)),
    c("DV SD", round(c(rep(dv_sd_full, 3), rep(dv_sd_rem, 3)), 3))
  ),
  title = "Effect of petitions on flat construction",
  font.size = "small",
  omit.stat = c("adj.rsq", "ser", "f"),
  report = "vcsp"
)
