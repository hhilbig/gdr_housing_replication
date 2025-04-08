# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(
  tidyverse,
  stargazer,
  lfe
)


## Load results for different residualization types

# Main results
out <- read_rds("results/est_2wfe.rds") %>%
  filter(resid_type == "Main") %>%
  filter(str_detect(type, "median")) %>%
  filter(year_start == "1963") %>%
  arrange(ss, model)

# Results using census 1971 residualization
out_census71 <- read_rds("results/est_2wfe.rds") %>%
  filter(resid_type == "census71") %>%
  filter(str_detect(type, "median")) %>%
  filter(year_start == "1963") %>%
  arrange(ss, model)

## Pseudo models for stargazer structure
mlist_6 <- rep(list(lm(iris$Sepal.Length ~ iris$Sepal.Width - 1)), 6) # For the first table (6 models)
coef_df <- out # Use main results for the first table

## Generate main LaTeX table (6 models)
stargazer(mlist_6,
  keep = 1, covariate.labels = "Above avg. petitions * post 1971",
  omit.table.layout = "s",
  coef = coef_df$estimate,
  se = coef_df$std.error,
  p = coef_df$p.value,
  add.lines = list(
    c("County FE", rep(c("No", "Yes", "Yes"), 2)),
    c("Period FE", rep(c("No", "Yes", "No"), 2)),
    c("Period * district FE", rep(c("No", "No", "Yes"), 2)),
    c("Sample", rep(c("Full", "No outliers"), each = 3)),
    c("DV mean", round(coef_df$dv_mean, 3)),
    c("DV SD", round(coef_df$dv_sd, 3)),
    c("R$^2$", round(coef_df$rsq, 3)),
    c("N", format(coef_df$n, big.mark = ","))
  ),
  style = "qje",
  dep.var.labels = "\\parbox{6cm}{DV: annual flat construction, per 1,000 capita}",
  title = "Effect of petitions on flat construction",
  label = "tab:reg_main",
  font.size = "small"
)

# Same table : Census 1971 residualization (first 3 models) -----------------

# Select first 3 models from Census 1971 results
out_71_subset <- out_census71 %>% slice(1:3)
mlist_3 <- rep(list(lm(iris$Sepal.Length ~ iris$Sepal.Width - 1)), 3) # For the second table (3 models)


stargazer(mlist_3, # Use list of 3 pseudo models
  keep = 1, covariate.labels = "Above avg. petitions * post 1971",
  omit.table.layout = "s",
  coef = out_71_subset$estimate,
  se = out_71_subset$std.error,
  p = out_71_subset$p.value,
  add.lines = list(
    c("County FE", "No", "Yes", "Yes"),
    c("Period FE", "No", "Yes", "No"),
    c("Period * district FE", "No", "No", "Yes"),
    c("Residualization", rep("Census 1971", 3)), # Only Census 1971
    c("DV mean", round(out_71_subset$dv_mean, 3)),
    c("DV SD", round(out_71_subset$dv_sd, 3)),
    c("R$^2$", round(out_71_subset$rsq, 3)),
    c("N", format(out_71_subset$n, big.mark = ","))
  ),
  style = "qje",
  dep.var.labels = "\\parbox{6cm}{DV: annual flat construction, per 1,000 capita}",
  title = "Effect of petitions on flat construction (Census 1971 Residualization)",
  label = "tab:reg_main_resid71", # Changed label
  font.size = "small"
)
