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
df <- read_rds("data/data_main.rds") %>%
  filter(year > 1960) %>%
  mutate(
    prot_share_post = prot_share * post, # Interaction term (though unused in final models)
    period = as.numeric(factor(period_agg_2years)), # Ensure period is numeric factor for FE
    period_bezirk = interaction(period_agg_2years, Bezirk) # Interaction FE term
  )

## Define treatment variables
outcomevars <- "flats_constr_public_pc"
treatvars <- "treated_mean_resid_1963"

df_agg <- df %>%
  group_by(county.id, period_agg_2years, Bezirk) %>%
  # Keep necessary variables for estimation and grouping
  summarise(
    across(all_of(c(treatvars, outcomevars, "post")), mean, na.rm = TRUE),
    # Need period and period_bezirk for FEs if they are not constant within group
    period = first(period),
    period_bezirk = first(period_bezirk),
    .groups = "drop"
  ) %>%
  # Ensure necessary columns exist after aggregation for felm
  mutate(
    county.id = factor(county.id),
    period_agg_2years = factor(period_agg_2years),
    period_bezirk = factor(period_bezirk)
  )

## Data for estimation (Using aggregated data)
df_est <- df_agg %>%
  # Define treatment interaction term dynamically within the loop later
  dplyr::select(
    all_of(treatvars),
    all_of(outcomevars),
    period_agg_2years,
    county.id,
    post,
    Bezirk,
    period, # Keep for FE
    period_bezirk # Keep for interaction FE
  )

## Get all bezirke
bz_list <- df_est$Bezirk %>%
  unique() %>%
  na.omit()
# Sort the list alphabetically
bz_list_sorted <- sort(bz_list)

## Define subsets ##
# Create list of dataframes, each excluding one Bezirk
ss_list <- pblapply(bz_list, function(b) df_est %>% filter(Bezirk != b))
names(ss_list) <- bz_list # Name list elements for easier tracking

## Loop
out <- pblapply(treatvars, function(tv) {
  # Use names(ss_list) which are the Bezirk labels
  pblapply(names(ss_list), function(ss_label) {
    df_ss <- ss_list[[ss_label]]

    ## Treated and treated * post
    # Ensure the treatment variable exists and create interaction
    df_ss <- df_ss %>%
      mutate(
        treat_temp = .data[[tv]],
        treat_temp_post = treat_temp * post
      )

    ## Get results for the single outcome variable
    dv <- outcomevars[1] # Only one outcome now

    # Model 1: No FE
    model_formula1 <- as.formula(paste0(
      dv,
      " ~ post + treat_temp + treat_temp_post",
      "| 0 | 0 | county.id" # Cluster SEs by county
    ))
    model_est1 <- lfe::felm(model_formula1, data = df_ss) %>%
      tidy(conf.int = TRUE) %>%
      filter(term == "treat_temp_post") %>%
      mutate(
        outcome = dv,
        treatvar = tv,
        ss = ss_label,
        model = "1 No FE"
      )

    # Model 3: 2WFE (County & Period)
    model_formula3 <- as.formula(paste0(
      dv, " ~ treat_temp_post",
      "| county.id + period_agg_2years | 0 | county.id" # Add FEs
    ))
    model_est3 <- lfe::felm(model_formula3, data = df_ss) %>%
      broom::tidy(conf.int = TRUE) %>%
      filter(term == "treat_temp_post") %>%
      mutate(
        outcome = dv,
        treatvar = tv,
        ss = ss_label,
        model = "3 2WFE"
      )

    # Model 5: 2WFE (County & Period*Bezirk) - Requires period_bezirk column
    # Check if period_bezirk exists and has variation
    if ("period_bezirk" %in% names(df_ss) && n_distinct(df_ss$period_bezirk, na.rm = TRUE) > 1) {
      model_formula5 <- as.formula(paste0(
        dv, " ~ treat_temp_post ",
        "| county.id + period_bezirk | 0 | county.id" # Use interaction FE
      ))
      model_est5 <- lfe::felm(model_formula5, data = df_ss) %>%
        tidy(conf.int = TRUE) %>%
        filter(term == "treat_temp_post") %>%
        mutate(
          outcome = dv,
          treatvar = tv,
          ss = ss_label,
          model = "5 2WFE, Period*Bezirk FE"
        )
    } else {
      # Handle cases where period_bezirk FE cannot be estimated (e.g., only one district left)
      # Create a dummy tibble with NA estimate
      model_est5 <- tibble(
        term = "treat_temp_post", estimate = NA_real_, std.error = NA_real_,
        statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
        outcome = dv, treatvar = tv, ss = ss_label, model = "5 2WFE, Period*Bezirk FE"
      )
      warning(paste("Skipping Period*Bezirk FE for excluded Bezirk:", ss_label, "- insufficient variation."))
    }


    ## Combine only the necessary models
    bind_rows(model_est1, model_est3, model_est5)
  }) %>% bind_rows() # Combine results across different excluded Bezirke
}) %>%
  bind_rows() %>% # Combine results across different treatment variables (if multiple)
  # Process treatment variable info (simplified as only one is used now)
  mutate(
    type = "Split based\non mean", # Hardcoded based on treatvars
    year_start = 1963 # Hardcoded based on treatvars
  )

## Clean up model names for plotting
out_plot <- out %>%
  # filter(str_detect(outcome, "public")) %>% # Already filtered by outcomevars selection
  # No need to filter out protest models, they weren't estimated
  mutate(model_label = case_when( # Create labels for plot facets
    model == "1 No FE" ~ "No FE",
    model == "3 2WFE" ~ "County & period FE",
    model == "5 2WFE, Period*Bezirk FE" ~ "County FE & \nperiod * district FE",
    TRUE ~ model # Fallback
  )) %>%
  # Ensure factor levels match the plot order
  mutate(model_label = factor(model_label, levels = c(
    "No FE",
    "County & period FE",
    "County FE & \nperiod * district FE"
  ))) %>%
  # Ensure ss (Bezirk label) is factor for discrete axis, ordered alphabetically
  mutate(ss = factor(ss, levels = bz_list_sorted)) # Use sorted list for levels
## ##

pd <- position_dodge(0.75) # Keeping original dodge

## Plot version for paper
p1 <- ggplot(
  out_plot %>% filter(term == "treat_temp_post"), # Ensure filtering just the interaction term
  aes(
    x = ss, y = estimate,
    ymin = conf.low, ymax = conf.high # Using standard broom names
  )
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(
    width = 0, # Consider a small width if points overlap, e.g., width = 0.1
    position = pd,
    # Make error bars slightly thinner and potentially grey
    linewidth = 0.4, color = "grey20"
  ) +
  geom_point(
    shape = 21,
    position = pd,
    fill = "white",
    size = 1.8, # Slightly smaller points
    stroke = 0.4 # Thinner point outline
  ) +
  theme_bw(base_size = 10) + # Use theme_bw with a base font size
  # Use model_label for facetting
  facet_grid(model_label ~ ., scales = "free_y", space = "free_y") +
  ylab("Estimated effect on\navg. annual flats constructed / 1,000 capita") +
  xlab("Omitted administrative district") + # Updated label slightly
  theme(
    legend.position = "none", # No legend needed
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Rotate x labels
    strip.background = element_rect(fill = "grey90", color = NA), # Facet label background
    strip.text.y = element_text(angle = 0, hjust = 0), # Adjust facet label text
    panel.spacing.y = unit(0.5, "lines") # Add a bit space between facets
  )
p1
