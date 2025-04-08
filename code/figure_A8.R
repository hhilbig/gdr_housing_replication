# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available
pacman::p_load(dplyr, readr, ggplot2, conflicted)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

## Get data and prepare variables
df <- read_rds("data/data_main.rds") %>%
  mutate(
    petitions_housing_pc = petitions_housing_pc * 1000
  )

## Identify years with sufficient petition data
pet_years <- df %>%
  group_by(year) %>%
  summarise(nobs = sum(!is.na(petitions_housing_pc)), .groups = "drop") %>%
  filter(nobs > 100) %>%
  pull(year)

# Filter main dataframe for relevant years
df <- df %>%
  filter(year %in% pet_years)

## Simulation: Calculate correlation between full-period average and subset-period average
# Pre-calculate the full-period average outside the loop for efficiency
sum_all <- df %>%
  group_by(AGS) %>%
  summarise(sum_housing_pet = mean(petitions_housing_pc, na.rm = T), .groups = "drop")

set.seed(123)

sim_res <- sapply(1:1000, function(i) {
  # Randomly drop 4 petition years for this iteration
  pet_years_i <- sample(pet_years, size = length(pet_years) - 4, replace = FALSE)

  # Calculate average petitions in the subset of years for this iteration
  sum_ss <- df %>%
    filter(year %in% pet_years_i) %>%
    group_by(AGS) %>%
    summarise(sum_housing_pet_ss = mean(petitions_housing_pc, na.rm = T), .groups = "drop") %>%
    left_join(sum_all, by = "AGS")

  # Calculate the correlation for this iteration
  res <- cor(sum_ss$sum_housing_pet, sum_ss$sum_housing_pet_ss, use = "complete.obs")

  return(res)
})


## Plot the distribution of correlation coefficients
ggplot(data.frame(sim_res), aes(x = sim_res)) +
  geom_histogram(binwidth = .0003, col = "black", fill = "grey80") +
  theme_bw() +
  labs(
    x = "Bivariate correlation coefficient",
    y = "Frequency"
  )
