rm(list = ls())

# Load necessary packages
pacman::p_load(tidyverse, conflicted, haven, ggpubr)

# Resolve conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

## Load and Prepare Housing Data
df_housing <- read_rds("data/data_main.rds") %>%
  # Scale petition variable
  mutate(petitions_housing_pc_per1000 = petitions_housing_pc * 1000) %>%
  # Filter out urban districts ("Stadtkreise"), but keep "Karl-Marx-Stadt-Land"
  # This combines the original filter-out and filter-in steps
  filter(
    !str_detect(AGS_name, "-Stadt$|Berlin Ost|Schwedt/Oder|^Frankfurt$") | # Exclude cities (using more specific regex for Frankfurt)
      str_detect(AGS_name, "Karl-Marx-Stadt-Land") # Explicitly include KMS-Land
  )

## Identify years with sufficient petition data
pet_years <- df_housing %>%
  group_by(year) %>%
  summarise(nobs = sum(!is.na(petitions_housing_pc_per1000)), .groups = "drop") %>%
  filter(nobs > 100) %>%
  pull(year)

## Load 1953 Protest Data
df_protest <- haven::read_dta("data/GDRProtest1953.dta") %>%
  dplyr::select(GDRCountyID1989, protestMun1953_share) # Select only needed columns

## Join Data and Filter Years
plot_data <- df_housing %>%
  # Keep only years with enough petition data
  filter(year %in% pet_years) %>%
  # Join with protest data
  left_join(df_protest, by = c("county.id" = "GDRCountyID1989")) %>%
  # Select only columns needed for the plot
  dplyr::select(protestMun1953_share, petitions_housing_pc_per1000) %>%
  # Remove rows where either variable is missing for the correlation/plot
  drop_na()

## Create Plot
p1 <- ggplot(plot_data, aes(x = protestMun1953_share, y = petitions_housing_pc_per1000)) +
  geom_point(shape = 21, size = 0.8, alpha = 0.6) + # Added alpha for potential overplotting
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  # theme_bw(fontsize = 16) + # Replace with standard theme if theme_bw is not available
  theme_bw(base_size = 16) + # Using theme_bw as standard alternative
  labs(
    y = "Housing petitions\n(per 1,000 capita)",
    x = "1953 protests\n(share of municipalities in county)",
    title = "Correlation between Housing Petitions and 1953 Protests" # Added title
  ) +
  # Add correlation coefficient (using ggpubr)
  ggpubr::stat_cor(aes(label = after_stat(r.label)), # Updated label aesthetic for newer ggpubr
    cor.coef.name = "r",
    label.x.npc = 0.05, # Position label
    label.y.npc = 0.95,
    hjust = 0
  )

p1
