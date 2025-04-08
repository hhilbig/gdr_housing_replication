# Clear environment
rm(list = ls())

# Load packages
if (!require(pacman)) install.packages("pacman") # Ensure pacman is available
pacman::p_load(tidyverse, conflicted)

# --- Helper Functions ---

# Function to process raw results data
process_results <- function(res_df, dict) {
  # Define the baseline row structure
  baseline_rows <- data.frame(
    period = -1, estimate = 0,
    conf.low = 0, conf.high = 0,
    year_start = rep(c(1963, 1971), each = 3),
    type = rep(c("Split based\non median", "Split based\non mean", "Continuous"), 2),
    ss = "Full",
    outcome = unique(res_df$outcome)[1] # Takes the first outcome found
  )

  # Add baseline rows, join with period dictionary, filter, and factorize
  res_df %>%
    bind_rows(baseline_rows) %>%
    left_join(dict %>% ungroup() %>% select(period, period_proper)) %>%
    # Ensure period_proper exists and filter out the specific [1961, NA] case if present
    filter(!is.na(period_proper), period_proper != "[1961, NA]") %>%
    filter(!period_proper == "1961") %>%
    mutate(period_proper = factor(period_proper)) # Convert period_proper to factor
}


# Function to create lag-lead plots
plot_laglead <- function(data, y_lab = "Estimated effect on\nflats constructed / 1,000 capita", x_breaks, x_labels, pd = position_dodge(0.75)) {
  ggplot(
    data,
    aes(
      x = as.numeric(period_proper), y = estimate,
      ymin = conf.low, ymax = conf.high
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
    geom_vline(xintercept = 5, linetype = "dotted", color = "grey40") + # Assuming 5 is the relevant intercept
    geom_errorbar(width = 0, position = pd) +
    geom_point(shape = 21, position = pd, fill = "white", size = 3) +
    theme_bw() +
    ylab(y_lab) +
    xlab("Periods (each period = 2 years)") +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    theme(legend.position = "bottom", axis.title.x = element_blank()) # Common theme adjustments
}

# --- Load and Prepare Base Data ---

# Load main results
out_raw <- read_rds("results/est_lag_lead.rds")

# Load housing data to create period dictionary
df <- read_rds("data/construction_data.rds") %>%
  filter(year > 1960) # Only need years > 1960 for periods

# Create period dictionary mapping aggregated periods to years
dict_df <- df %>%
  distinct(period_agg_2years, year) %>%
  filter(!is.na(period_agg_2years), period_agg_2years != 1961) %>% # Ensure no NA periods AND explicitly remove 1961
  group_by(period_agg_2years) %>%
  summarise(
    y1 = min(year),
    y2 = max(year),
    .groups = "drop"
  ) %>%
  mutate(
    period_proper = ifelse(y1 == y2, as.character(y1), paste0("[", y1, ", ", y2, "]")),
    period = period_agg_2years
  )

# Process main results
out <- process_results(out_raw, dict_df)

# Define plot position dodge and common x-axis properties
pd <- position_dodge(0.75)
period_levels <- levels(out$period_proper)
x_breaks <- 1:length(period_levels)
x_labels <- period_levels

# --- Generate Plots ---

# Figure 2
plot_data_fig2 <- out %>%
  filter(
    year_start == 1963,
    type == "Split based\non median",
    ss == "Full",
    str_detect(outcome, "public")
  )

p_fig2 <- plot_laglead(plot_data_fig2, x_breaks = x_breaks, x_labels = x_labels) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

print(p_fig2)

# Figure A.10 (Appendix - Different Treatment Specs)
plot_data_a10 <- out %>%
  filter(
    ss == "Full",
    str_detect(outcome, "public"),
    year_start %in% c(1963, 1971)
  ) %>%
  mutate(
    type = recode(type, `Continuous` = "Continuous treatment"),
    year_start_str = case_when(
      year_start == 1963 ~ "Petitions measured\nbetween 1963-71",
      year_start == 1971 ~ "Petitions measured\nin 1971",
      TRUE ~ NA_character_
    ),
    # Ensure consistent factor levels for faceting
    type = factor(type, levels = c("Split based\non median", "Split based\non mean", "Continuous treatment"))
  )

p_a10 <- plot_laglead(plot_data_a10, x_breaks = x_breaks, x_labels = x_labels) +
  facet_grid(type ~ year_start_str) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  ))

print(p_a10)

# Figure A12 (Appendix - Demand Controls)
out_d_raw <- read_rds("results/est_lag_lead_demand.rds")

out_d <- process_results(out_d_raw, dict_df)

plot_data_a12 <- out_d %>%
  filter(
    which_var == "Main treatment", # Specific filter for this dataset
    ss == "Full",
    str_detect(outcome, "public"),
    year_start %in% c(1963, 1971)
  ) %>%
  mutate(
    type = recode(type, `Continuous` = "Continuous treatment"),
    year_start_str = case_when(
      year_start == 1963 ~ "Petitions measured\nbetween 1963-71",
      year_start == 1971 ~ "Petitions measured\nin 1971",
      TRUE ~ NA_character_
    ),
    type = factor(type, levels = c("Split based\non median", "Split based\non mean", "Continuous treatment"))
  )

# Need to get the x-axis labels specific to this dataset if they differ
period_levels_d <- levels(plot_data_a12$period_proper)
x_breaks_d <- 1:length(period_levels_d)
x_labels_d <- period_levels_d

p_a12 <- plot_laglead(plot_data_a12, x_breaks = x_breaks_d, x_labels = x_labels_d) +
  facet_grid(type ~ year_start_str) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  ))

print(p_a12)

# Figure A13 (Appendix - Party Strength Controls)
out_p_raw <- read_rds("results/est_lag_lead_party.rds") # No filter needed here based on original

out_p <- process_results(out_p_raw, dict_df)

plot_data_a13 <- out_p %>%
  filter(
    which_var == "Main treatment", # Specific filter for this dataset
    ss == "Full",
    str_detect(outcome, "public"),
    year_start %in% c(1963, 1971)
  ) %>%
  mutate(
    type = recode(type, `Continuous` = "Continuous treatment"),
    year_start_str = case_when(
      year_start == 1963 ~ "Petitions measured\nbetween 1963-71",
      year_start == 1971 ~ "Petitions measured\nin 1971",
      TRUE ~ NA_character_
    ),
    type = factor(type, levels = c("Split based\non median", "Split based\non mean", "Continuous treatment"))
  )

# Need to get the x-axis labels specific to this dataset if they differ
period_levels_p <- levels(plot_data_a13$period_proper)
x_breaks_p <- 1:length(period_levels_p)
x_labels_p <- period_levels_p

p_a13 <- plot_laglead(plot_data_a13, x_breaks = x_breaks_p, x_labels = x_labels_p) +
  facet_grid(type ~ year_start_str) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  ))

print(p_a13)
