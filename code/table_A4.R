# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(tidyverse, lfe, stargazer, conflicted)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
## Get data

df <- read_rds("data/data_main.rds") %>%
  filter(year > 1960)
# Load non-housing treated counties

treated_nonhousing <- read_rds("data/treated_nonhousing.rds")

# Merge

df <- df %>%
  left_join(treated_nonhousing, by = "county.id")

## Define SPECIFIC treatment and outcome variables needed for the table

outcome_var <- "flats_constr_public_pc"
treat_var <- "treated_median_nonhousing2_1963"

## Aggregate (Only keep necessary variables)

df <- df %>%
  group_by(county.id, period_agg_2years, Bezirk) %>%
  summarise(
    across(all_of(c(treat_var, outcome_var, "post", "incr_flats_after71_pct")),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop" # Avoid warning message
  ) %>%
  mutate(period_bezirk = paste0(period_agg_2years, "_", Bezirk))

## Define subsets ##
ss_list <- list(
  Full = df,
  `Rem outlier` = df %>% filter(!incr_flats_after71_pct > quantile(df$incr_flats_after71_pct, 0.95, na.rm = T))
)

## --- Model Estimation ---

model_list <- list()
stats_list <- list() # To store stats for add.lines

for (ss_name in names(ss_list)) {
  df_ss <- ss_list[[ss_name]]

  # Create treatment interaction term
  df_ss$treat_temp <- df_ss[[treat_var]]
  df_ss$treat_temp_post <- df_ss$treat_temp * df_ss$post

  # Calculate DV mean and SD for this subset
  dv_mean <- mean(df_ss[[outcome_var]], na.rm = TRUE)
  dv_sd <- sd(df_ss[[outcome_var]], na.rm = TRUE)

  # --- Estimate Models ---

  # Model 1: No FE
  model_formula1 <- as.formula(paste0(
    outcome_var,
    " ~ post + treat_temp + treat_temp_post",
    "| 0 | 0 | county.id"
  ))
  model_est1 <- felm(model_formula1, data = df_ss)

  # Model 2: 2WFE
  model_formula2 <- as.formula(paste0(
    outcome_var, " ~ treat_temp_post",
    "| period_agg_2years + county.id | 0 | county.id"
  ))
  model_est2 <- felm(model_formula2, data = df_ss)

  # Model 3: 2WFE, Period*Bezirk FE
  model_formula3 <- as.formula(paste0(
    outcome_var, " ~ treat_temp_post ",
    "| period_bezirk + county.id | 0 | county.id"
  ))
  model_est3 <- felm(model_formula3, data = df_ss)

  # --- Store Models and Stats ---
  model_list <- c(model_list, list(model_est1, model_est2, model_est3))

  stats_list[[paste0(ss_name, "_1")]] <- list(N = model_est1$N, R2 = summary(model_est1)$r.squared, dv_mean = dv_mean, dv_sd = dv_sd)
  stats_list[[paste0(ss_name, "_2")]] <- list(N = model_est2$N, R2 = summary(model_est2)$r.squared, dv_mean = dv_mean, dv_sd = dv_sd)
  stats_list[[paste0(ss_name, "_3")]] <- list(N = model_est3$N, R2 = summary(model_est3)$r.squared, dv_mean = dv_mean, dv_sd = dv_sd)
}

# The model_list now contains the 6 required felm objects in the correct order

## --- Generate LaTeX table using stargazer ---

# Prepare statistics for add.lines from the stats_list
# Order should match model_list: Full (1, 2, 3), Rem outlier (1, 2, 3)
ordered_stats <- stats_list[c("Full_1", "Full_2", "Full_3", "Rem outlier_1", "Rem outlier_2", "Rem outlier_3")]

n_obs <- sapply(ordered_stats, function(x) format(x$N, big.mark = ","))
r_sq <- sapply(ordered_stats, function(x) round(x$R2, 3))
dv_means <- sapply(ordered_stats, function(x) round(x$dv_mean, 3))[c(1, 4)] # Only need one per sample
dv_sds <- sapply(ordered_stats, function(x) round(x$dv_sd, 3))[c(1, 4)] # Only need one per sample


stargazer(model_list, # Pass the list of felm objects directly
  keep = "treat_temp_post", # Keep only the interaction term
  covariate.labels = "Non-housing petitions * post 1971",
  omit.stat = c("adj.rsq", "ser"), # Omit default stats we add manually or don't need
  omit.table.layout = "s", # Keep layout simple
  # No need for coef, se, p arguments anymore
  add.lines = list(
    c("County FE", rep(c("No", "Yes", "Yes"), 2)),
    c("Period FE", rep(c("No", "Yes", "No"), 2)),
    c("Period \\times district FE", rep(c("No", "No", "Yes"), 2)), # Escaped asterisk
    c("Sample", rep(c("Full", "No outliers"), each = 3)),
    c("DV mean", c(rep(dv_means[1], 3), rep(dv_means[2], 3))),
    c("DV SD", c(rep(dv_sds[1], 3), rep(dv_sds[2], 3))),
    c("R$^2$", r_sq),
    c("N", n_obs)
  ),
  style = "qje",
  dep.var.labels.include = FALSE, # Remove default DV label
  column.labels = NULL, # Use numbers instead of model names
  notes = paste("DV:", outcome_var, "(per 1,000 capita)"), # Add DV explanation in notes
  notes.align = "l",
  title = "Effect of non-housing petitions on flat construction (placebo)",
  label = "tab:reg_placebo",
  font.size = "small"
)
