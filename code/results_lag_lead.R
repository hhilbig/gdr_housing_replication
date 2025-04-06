# Clear environment
rm(list = ls())

# Load packages
pacman::p_load(tidyverse, fastDummies, lfe, fixest)

# Load helper functions
source("code/functions.R")

## Get data

df <- read_rds("data/data_main.rds") %>%
  filter(year > 1960) %>%
  group_by(county.id) %>%
  mutate(flats71 = qual_flats_capita[year == 1971] * 1000) %>%
  mutate(births_pc_71 = births_pc[year == 1971]) %>%
  ungroup()

## Merge

## Define treatment variables

# Only keep the outcome used in figure_2_A10_A12_A13.R
outcomevars <- c("flats_constr_public_pc")

# Explicitly define only the needed treatment variables
treatvars <- c(
  "treated_mean_resid_1963",
  "treated_median_resid_1963",
  "treated_cont_resid_1963",
  "treated_mean_resid_1971",
  "treated_median_resid_1971",
  "treated_cont_resid_1971"
)

othervars <- colnames(df) %>%
  .[str_detect(., "after71")] %>%
  c("flats71", "births_pc_71") %>%
  c("sed_share46") %>%
  c("sed_members_popshare_1971")

## Aggregate

df <- df %>%
  group_by(county.id, period_agg_2years) %>%
  summarise_at(vars(one_of(c(treatvars, outcomevars, othervars, "post"))),
    mean,
    na.rm = T
  )

## Data

df <- df %>%
  dplyr::select(
    one_of(treatvars),
    one_of(outcomevars),
    one_of(othervars),
    period_agg_2years,
    county.id,
    post
  ) %>%
  mutate(period = as.numeric(factor(period_agg_2years)))

## Nr of pre and post treatment periods

id_treat <- df %>%
  filter(post == 1) %>%
  filter(period == min(period)) %>%
  pull(period) %>%
  unique()

# Modify main loop to use df directly and simplify outcome handling
out <- pblapply(treatvars, function(tv) {
  # Remove ss_id and df_ss assignment based on ss_list
  # ss_id <- 1 # Hardcode ss_id
  # df_ss <- ss_list[[ss_id]]

  # Use df directly, perhaps copy to avoid modifying original df in loop? (using df_ss convention)
  df_ss <- df
  df_ss$treat_temp <- df_ss %>% pull(!!tv)

  ## Create Period*Treated Indicator Variables

  df_ss <- fastDummies::dummy_cols(df_ss, select_columns = "period") %>%
    mutate_at(vars(contains("period_")), ~ . * treat_temp)

  ## Generate Model Formulas

  periodvars <- names(df_ss)[str_detect(names(df_ss), "period_")] %>%
    .[!str_detect(., "period_agg_2years")]

  ## Exclude last pre-treatment period

  periodvars <- periodvars[-which(periodvars == paste0(
    "period_",
    id_treat - 1
  ))]

  # Simplify: Remove lapply as outcomevars has only one element
  # results <- lapply(outcomevars, function(dv) {
  dv <- outcomevars[1] # Get the single outcome variable name
  model_formula1 <- as.formula(paste0(
    dv, " ~",
    paste0(periodvars, collapse = "+"),
    "| period + county.id | 0 | county.id"
  ))


  model_est1 <- felm(model_formula1, data = df_ss) %>%
    broom::tidy(conf.int = T) %>%
    filter(str_detect(term, "period")) %>%
    mutate(outcome = paste(dv)) %>%
    mutate(covars = "No") %>%
    mutate(ss = "Full") # Hardcode ss

  # Assign model_est1 directly to results
  results <- model_est1

  ## Rename the period variables

  results <- results %>%
    mutate(period = parse_number(term) - (id_treat))

  ## Rename Outcomes for Plotting

  results <- results %>%
    mutate(outcome = as.factor(outcome))

  ## Rename Periods for Plotting

  results <- results %>%
    mutate(period = as.numeric(period)) %>%
    mutate(treatvar = !!tv)

  results
}) %>%
  reduce(rbind) %>%
  separate(
    col = treatvar, sep = -5,
    into = c("type", "year_start")
  ) %>%
  mutate(year_start = as.numeric(substr(year_start, 2, 5))) %>%
  mutate(type = dplyr::recode(type,
    `treated_mean_resid` = "Split based\non mean",
    `treated_median_resid` = "Split based\non median",
    `treated_cont_resid` = "Continuous"
  ))

## Save estimates

write_rds(out, "results/est_lag_lead.rds")

# Account for housing demand --------------------------------

# Vars (baseline)
# Births pc 71 is births pc 71
# Flats71 is flats in 71 (stock) pc

c("flats71", "births_pc71")

out_d <- pblapply(treatvars, function(tv) {
  # Remove ss_id and df_ss based on ss_list
  # cat(tv, ss_id, "\n")
  cat(tv, "\n") # Simplified logging

  # Use df directly
  df_ss <- df
  df_ss$treat_temp <- df_ss %>% pull(!!tv)

  # Formula

  form <- ".[outcomevars] ~ i(period_agg_2years, treat_temp, ref = -1) +  i(period_agg_2years, flats71, ref = -1) + i(period_agg_2years, births_pc_71, ref = -1) | period + county.id" %>%
    as.formula()

  # Estimate

  mod <- feols(form, cluster = ~county.id, data = df_ss) %>%
    tidy_feols() %>%
    mutate(which_var = case_when(
      str_detect(term, "flats71") ~ "Flats 71",
      str_detect(term, "births") ~ "Births p.c.",
      TRUE ~ "Main treatment"
    )) %>%
    mutate(term = str_remove_all(term, "period_agg_2years::")) %>%
    mutate(term = str_remove_all(term, ":treat_temp|:flats71|:births_pc_71")) %>%
    mutate(term = as.numeric(term)) %>%
    mutate(term = term + 7) %>%
    mutate(term = paste0("period_", term)) %>%
    # Fix outcome assignment: use outcomevars[1] instead of undefined dv
    # Use ss = "Full" directly instead of ss_labels[ss_id]
    mutate(outcome = outcomevars[1], covars = "Housing stock, births p.c.", ss = "Full")

  # Return

  mod
  # }) %>% reduce(rbind) # Remove reduce as there's only one element now

  ## Rename the period variables
  # results variable is now mod directly
  results <- mod %>% # results is now just mod
    mutate(period = parse_number(term) - (id_treat))

  ## Rename Outcomes for Plotting

  results <- results %>%
    mutate(outcome = as.factor(outcome))

  ## Rename Periods for Plotting

  results <- results %>%
    mutate(period = as.numeric(period)) %>%
    mutate(treatvar = !!tv)

  results
}) %>%
  reduce(rbind) %>%
  separate(
    col = treatvar, sep = -5,
    into = c("type", "year_start")
  ) %>%
  mutate(year_start = as.numeric(substr(year_start, 2, 5))) %>%
  mutate(type = dplyr::recode(type,
    `treated_mean_resid` = "Split based\non mean",
    `treated_median_resid` = "Split based\non median",
    `treated_cont_resid` = "Continuous"
  ))


# Save this

write_rds(out_d, "results/est_lag_lead_demand.rds")

# Account for party strength ------------------------------------------------

# Relevant var: sed_share46 (should be interacted with period)

out_p <- pblapply(treatvars, function(tv) {
  # Remove ss_id and df_ss based on ss_list
  # cat(tv, ss_id, "\n") # ss_id is fixed at 1 from above
  cat(tv, "\n") # Simplified logging

  # Use df directly
  df_ss <- df
  df_ss$treat_temp <- df_ss %>% pull(!!tv)

  # Formula

  form <- ".[outcomevars] ~ i(period_agg_2years, treat_temp, ref = -1)+ i(period_agg_2years, sed_share46, ref = -1) +
    i(period_agg_2years, sed_members_popshare_1971, ref = -1) | period + county.id" %>%
    as.formula()

  # Estimate

  mod <- feols(form, cluster = ~county.id, data = df_ss) %>%
    tidy_feols() %>%
    mutate(which_var = case_when(
      str_detect(term, "sed_share46") ~ "SED share 1946",
      TRUE ~ "Main treatment"
    )) %>%
    mutate(term = str_remove_all(term, "period_agg_2years::")) %>%
    mutate(term = str_remove_all(term, ":treat_temp|:flats71|:births_pc_71")) %>%
    mutate(term = as.numeric(term)) %>%
    mutate(term = term + 7) %>%
    mutate(term = paste0("period_", term)) %>%
    # Fix outcome assignment: use outcomevars[1] instead of undefined dv
    # Use ss = "Full" directly instead of ss_labels[ss_id]
    mutate(outcome = outcomevars[1], covars = "SED share 1946", ss = "Full")

  # Return

  mod
  # }) %>% reduce(rbind) # Remove reduce as there's only one element now

  ## Rename the period variables
  # results variable is now mod directly
  results <- mod %>% # results is now just mod
    mutate(period = parse_number(term) - (id_treat))

  ## Rename Outcomes for Plotting

  results <- results %>%
    mutate(outcome = as.factor(outcome))

  ## Rename Periods for Plotting

  results <- results %>%
    mutate(period = as.numeric(period)) %>%
    mutate(treatvar = !!tv)

  results
}) %>%
  reduce(rbind) %>%
  separate(
    col = treatvar, sep = -5,
    into = c("type", "year_start")
  ) %>%
  mutate(year_start = as.numeric(substr(year_start, 2, 5))) %>%
  mutate(type = dplyr::recode(type,
    `treated_mean_resid` = "Split based\non mean",
    `treated_median_resid` = "Split based\non median",
    `treated_cont_resid` = "Continuous"
  ))

# Save results

write_rds(
  out_p,
  "results/est_lag_lead_party.rds"
)
