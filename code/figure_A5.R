# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(dplyr, readr, ggplot2, ggpubr, tidyr, janitor, conflicted)


## Get data and prepare variables
df <- read_rds("data/data_main.rds") %>%
  mutate(
    petitions_housing_pc = petitions_housing_pc * 1000
    # Removed redundant line: flats_constr_public_pc = flats_constr_public_pc
  )

## Identify years with sufficient petition data
pet_years <- df %>%
  group_by(year) %>%
  summarise(nobs = sum(!is.na(petitions_housing_pc)), .groups = "drop") %>%
  filter(nobs > 100) %>%
  pull(year)

## Create scatter plot faceted by year
# petitions_housing_pc vs flats_constr_public_pc

# Define plot object p1
p1 <- ggplot(
  df %>% filter(year %in% pet_years),
  aes(x = petitions_housing_pc, y = flats_constr_public_pc)
) +
  geom_point(
    shape = 21,
    size = .8,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    col = "black",
    size = .5,
    formula = y ~ x # Explicitly set formula for clarity
  ) +
  theme_bw(base_size = 12) +
  facet_wrap(~year,
    ncol = 4,
    scales = "free"
  ) +
  labs(
    title = "Housing Petitions vs. Construction by Year",
    x = "Housing petitions (per 1,000 capita)",
    y = "Housing construction (per 1,000 capita)"
  ) +
  ggpubr::stat_cor(
    aes(label = after_stat(r.label)),
    cor.coef.name = "r",
    label.x.npc = 0.05, # Position correlation label
    label.y.npc = 0.95, # Position correlation label
    hjust = 0,
    size = 3 # Adjust label size
  )

# Display the plot (optional)
print(p1)
