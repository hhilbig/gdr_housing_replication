rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(dplyr, readr, stringr, tidyr, kableExtra, knitr)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

## Get data

df_full <- read_rds("data/data_main.rds")

# Calculate necessary variables on the full dataset before filtering by year
df <- df_full %>%
  # Calculate flats in 1971 per 1000 capita
  group_by(county.id) %>%
  mutate(flats71 = first(qual_flats_capita[year == 1971]) * 1000) %>% # Ensure only one value per county
  ungroup() %>%
  # Scale population
  mutate(pop_1969 = pop_1969 / 1000)

## Calculate Average Petitions (1963-1971)

agg_pet <- df_full %>%
  filter(between(year, 1963, 1971)) %>% # Corrected period
  group_by(county.id) %>%
  # Calculate mean, ensuring multiplication by 1000
  summarise(avg_pet_63_71 = mean(petitions_housing_pc, na.rm = TRUE) * 1000, .groups = "drop")

## Select Data Snapshot (e.g., 1971) and Join Aggregated Petitions

df_snapshot <- df %>%
  filter(year == 1971) %>% # Use 1971 snapshot for cross-sectional vars
  left_join(agg_pet, by = "county.id")

## Define Variables and Labels for Table A.1

vlist_table <- c(
  "pop_1969",
  "flats71",
  "incr_flats_after71_abs",
  "incr_flats_after71_pct",
  "avg_pet_63_71", # Use the newly calculated average
  "treated_cont_resid_1963" # Residualized petitions (as proxy)
)

vlabels_table <- c(
  "Population in 1,000s",
  "Stock of flats per 1,000 capita",
  "Increase in flats per 1,000 capita, 1972-1989 (absolute)",
  "Increase in flats per 1,000 capita, 1972-1989 (rel. to baseline)",
  "Avg. petitions per 1,000 capita, 1963-1971",
  "Avg. petitions per 1,000 capita, 1963-1971 (residualized)"
)

## Summary Statistics Function

svec2 <- function(vector) {
  # Handle cases where all values are NA
  if (all(is.na(vector))) {
    return(data.frame(
      Mean = NA_real_, Median = NA_real_, SD = NA_real_,
      Min = NA_real_, Max = NA_real_, N = 0L
    ))
  }
  # Calculate stats, ensuring finite=TRUE for min/max if Inf/-Inf possible
  mean_v <- mean(vector, na.rm = TRUE)
  median_v <- median(vector, na.rm = TRUE)
  sd_v <- sd(vector, na.rm = TRUE)
  min_v <- min(vector, na.rm = TRUE)
  max_v <- max(vector, na.rm = TRUE)
  n_obs <- sum(!is.na(vector)) # Count non-NA observations
  # Return as data frame with specific column names
  data.frame(Mean = mean_v, Median = median_v, SD = sd_v, Min = min_v, Max = max_v, N = n_obs)
}

## Calculate and Format Summary Statistics

sstats_list <- vlist_table %>%
  lapply(function(var) {
    # Ensure the variable exists before trying to pull it
    if (var %in% names(df_snapshot)) {
      svec2(df_snapshot[[var]]) # Use [[var]] instead of pull for robustness
    } else {
      warning(paste("Variable", var, "not found in df_snapshot. Skipping."))
      # Return a data frame with NAs if variable not found
      data.frame(Mean = NA, Median = NA, SD = NA, Min = NA, Max = NA, N = NA)
    }
  }) %>%
  bind_rows() %>% # Use bind_rows instead of reduce(rbind)
  # Add the variable labels as the first column
  mutate(Variable = vlabels_table, .before = 1)


# Create the kable table
kable(sstats_list,
  format = "latex", # Explicitly set format
  longtable = FALSE, # Match table image (not longtable)
  booktabs = TRUE, # Use booktabs rules (\toprule, \midrule, \bottomrule)
  col.names = c( # Set column names exactly as in image
    "Variable", "Mean", "Median", "SD", "Min", "Max", "N"
  ),
  linesep = "", # Remove default lines between rows
  caption = "Table A.1: Summary statistics", # Set caption
  label = "tab:sumstats_A1", # Set label (updated for specificity)
  escape = FALSE, # Don't escape LaTeX characters in labels/notes
  digits = 2, # Set number of decimal places for numeric columns
  align = c("l", "r", "r", "r", "r", "r", "r") # Specify column alignments (left, 6x right, left)
)
