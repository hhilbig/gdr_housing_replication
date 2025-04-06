# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

# Load packages
pacman::p_load(
  tidyverse,
  lfe,
  broom,
  pbapply
)

## Get data
df_base <- read_rds("data/data_main.rds") %>%
  filter(year > 1960) %>%
  # Select only necessary columns upfront
  select(county.id, year, period_agg_2years, Bezirk, post, flats_constr_public_pc)

## Get the alternative treatments (based on imputation year)
alt_treat <- read_rds("data/treatments_alternative_imputation_year.rds") %>%
  # Keep only county identifier and treatments starting in 1963
  select(county.id, matches("treated_(mean|median|cont)_resid_1963_"))

# Define treatment variables based on columns read
treatvars <- names(alt_treat)[-1] # Exclude county.id
outcomevar <- "flats_constr_public_pc" # Single outcome variable

## Join data
df <- df_base %>%
  left_join(alt_treat, by = "county.id")

## Aggregate data
# Aggregate panel data to the period level for analysis
# This assumes treatment variables are constant within a county
df_agg <- df %>%
  group_by(county.id, period_agg_2years, Bezirk) %>%
  # Summarise outcome and post indicator
  # Keep treatment variables (already summarised at county level)
  summarise(
    across(all_of(c(outcomevar, "post")), mean, na.rm = TRUE),
    across(all_of(treatvars), first), # Take first value of treatment (constant per county)
    .groups = "drop"
  ) %>%
  # Add necessary factor/interaction variables for fixed effects
  mutate(
    county.id = factor(county.id),
    period_agg_2years = factor(period_agg_2years),
    period_bezirk = interaction(period_agg_2years, Bezirk)
  )

## Loop through treatment variables and estimate models
out_list <- pblapply(treatvars, function(tv) {
  # Prepare data for this treatment variable
  df_loop <- df_agg %>%
    mutate(
      treat_temp = .data[[tv]],
      treat_temp_post = treat_temp * post
    )

  # Define formulas for models 1, 3, 5
  formula1 <- as.formula(paste0(outcomevar, " ~ post + treat_temp + treat_temp_post | 0 | 0 | county.id"))
  formula3 <- as.formula(paste0(outcomevar, " ~ treat_temp_post | county.id + period_agg_2years | 0 | county.id"))
  formula5 <- as.formula(paste0(outcomevar, " ~ treat_temp_post | county.id + period_bezirk | 0 | county.id"))

  # Estimate models and tidy results
  est1 <- lfe::felm(formula1, data = df_loop) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "treat_temp_post") %>%
    mutate(model = "1 No FE", treatvar = tv)
  est3 <- lfe::felm(formula3, data = df_loop) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "treat_temp_post") %>%
    mutate(model = "3 2WFE", treatvar = tv)
  est5 <- lfe::felm(formula5, data = df_loop) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "treat_temp_post") %>%
    mutate(model = "5 2WFE, Period*Bezirk FE", treatvar = tv)

  # Combine results for this treatment variable
  bind_rows(est1, est3, est5)
})

# Combine results from all treatment variables
out <- bind_rows(out_list) %>%
  # Extract imputation year and treatment type from variable name
  separate(treatvar, into = c("type_long", "year_imp"), sep = "_1963_", remove = FALSE) %>%
  mutate(
    year_imp = as.numeric(year_imp),
    type = case_when(
      str_detect(type_long, "mean") ~ "Split based\non mean",
      str_detect(type_long, "median") ~ "Split based\non median",
      str_detect(type_long, "cont") ~ "Continuous\ntreatment",
      TRUE ~ NA_character_
    ),
    # Add model labels for plotting facets
    model_label = case_when(
      model == "1 No FE" ~ "No fixed effects",
      model == "3 2WFE" ~ "County & period\nfixed effects",
      model == "5 2WFE, Period*Bezirk FE" ~ "County & period*district\nfixed effects",
      TRUE ~ model # Fallback
    )
  )

## Set factor levels for plotting order
# Define desired order
type_levels <- c("Split based\non mean", "Split based\non median", "Continuous\ntreatment")
model_levels <- c("No fixed effects", "County & period\nfixed effects", "County & period*district\nfixed effects")

out_plot <- out %>%
  mutate(
    type = factor(type, levels = type_levels),
    model_label = factor(model_label, levels = model_levels),
    # Ensure imputation year is treated as a categorical factor for the axis
    year_imp = factor(year_imp)
  ) %>%
  # Keep only necessary columns for plotting
  select(estimate, conf.low, conf.high, type, model_label, year_imp)

## Plot this
pd <- position_dodge(0.4)

ggplot(
  out_plot,
  aes(
    x = year_imp, y = estimate, # year_imp is now the categorical axis
    ymin = conf.low, ymax = conf.high
  )
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(width = 0, position = pd, linewidth = 0.4, color = "grey20") +
  geom_point(shape = 21, position = pd, fill = "white", size = 1.8, stroke = 0.4) +
  coord_flip() + # Flip coordinates to match original plot
  facet_grid(type ~ model_label, scales = "free_y") + # Facet by type and model
  theme_bw(base_size = 10) +
  ylab("Estimated effect on\nflats constructed / 1,000 capita") +
  xlab("Imputed construction year") +
  theme(
    legend.position = "none", # No legend needed
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text.y = element_text(angle = 0), # Horizontal type labels
    strip.text.x = element_text(angle = 0) # Horizontal model labels
  )
