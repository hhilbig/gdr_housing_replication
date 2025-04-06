# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available
pacman::p_load(dplyr, readr, sf, ggrepel, stringr, units)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# --- Define Custom Theme ---
# Define the map theme function once
theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(), # Simplified: remove both x and y text
      axis.ticks = element_blank(),
      axis.title = element_blank(), # Simplified: remove both x and y titles
      panel.grid.major = element_line(color = "grey90", size = 0.2), # Adjusted color slightly
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      legend.position = "bottom", # Default legend position
      legend.text = element_text(size = 10), # Adjusted default legend text size
      legend.title = element_text(size = 11), # Added legend title size
      ...
    )
}


# --- Load and Prepare Data ---

# Load main housing data
df_full <- read_rds("data/data_main.rds")

# Calculate average petitions (1963-1971, per 1000 capita)
# Note: Original code used 1960-1972, adjusted based on Table A.1 context if needed,
# keeping 1960-1972 as per original script here. Adjust range if necessary.
agg_pet <- df_full %>%
  filter(between(year, 1960, 1972)) %>%
  group_by(county.id) %>%
  summarise(agg_pet_mean_60_72 = mean(petitions_housing_pc, na.rm = TRUE) * 1000, .groups = "drop")

# Get distinct county data (using 1971 snapshot as common point, similar to Table A.1 script)
df_distinct_county <- df_full %>%
  filter(year == 1971) %>% # Or use distinct(county.id, .keep_all=T) if year doesn't matter
  left_join(agg_pet, by = "county.id")

# Load shapefile and join with distinct county data
shp <- sf::st_read("data/shp/Germany_1989_v.1.0.shp", quiet = TRUE) %>%
  # Ensure KREIS_KENN is character for joining
  mutate(KREIS_KENN = as.character(KREIS_KENN)) %>%
  left_join(df_distinct_county, by = c("KREIS_KENN" = "AGS")) %>%
  # Filter for GDR (land codes > 10 based on KREIS_KENN prefix)
  mutate(land = as.numeric(substr(KREIS_KENN, 1, 2))) %>%
  filter(land > 10) %>%
  # Filter out counties with no protest median data (as per original filter)
  filter(!is.na(prot_share_median)) %>%
  # Transform CRS to standard lat/lon
  sf::st_transform(4326) # Use explicit package prefix

# Load city data
cit <- read_csv("data/cities_de.csv") %>%
  filter(str_detect(city, "Berlin|Dresden|Erfurt|Rostock"))


# --- Plotting Function (for binary fill) ---
# Simplified plot function specifically for the binary treated_median plot
plot_binary_map <- function(data, fill_var, vname = "", hi_lab = "High", lo_lab = "Low") {
  # Ensure fill variable is factor
  data <- data %>%
    filter(!is.na(.data[[fill_var]])) %>% # Use .data pronoun
    mutate({{ fill_var }} := as.factor(.data[[fill_var]])) # Use := for dynamic assignment

  ggplot(data) +
    geom_sf(aes(fill = .data[[fill_var]]), size = 0.2, color = "black") + # Reduced line size
    scale_fill_manual(
      values = c("0" = "#fff7bc", "1" = "#d95f0e"), # Assign values based on factor levels (assuming 0/1)
      name = vname,
      labels = c("0" = lo_lab, "1" = hi_lab), # Assign labels based on factor levels
      na.value = "grey80" # Explicit NA color
    ) +
    geom_point(data = cit, aes(x = lng, y = lat), shape = 21, fill = "white", size = 3) + # Adjusted size
    geom_text_repel( # Switched to text_repel for potentially better placement
      data = cit, aes(x = lng, y = lat, label = city),
      size = 3, # Adjusted size
      nudge_y = -0.1, # Adjusted nudge
      min.segment.length = 0.5, # Adjusted segment length
      point.padding = 0.5, # Added padding
      box.padding = 0.5 # Added padding
    ) +
    theme_map() +
    theme(legend.key.width = unit(0.5, "cm")) # Adjust legend key width
}

# --- Generate Plots ---

# Figure A4 Middle Panel (Binary Residualized Petitions)
p_middle <- plot_binary_map(
  data = shp,
  fill_var = "treated_median_resid_1963",
  hi_lab = "Residualized per-capita\npetitions above median",
  lo_lab = "Residualized per-capita\npetitions below median"
)

p_middle


# Figure A4 Left-hand Panel (Continuous Average Petitions)
# Using direct ggplot call as it uses scale_fill_gradient

# Ensure the fill variable exists and is numeric
shp_left <- shp %>% filter(!is.na(agg_pet_mean_60_72))

p_left <- ggplot(shp_left) +
  geom_sf(aes(fill = agg_pet_mean_60_72), size = 0.2, color = "black") +
  scale_fill_gradient(
    low = "#fff7bc", high = "#d95f0e",
    name = "Avg. Petitions per 1,000 capita\n(1960-1972)", # Updated label for clarity
    na.value = "grey80" # Explicit NA color
  ) +
  geom_point(data = cit, aes(x = lng, y = lat), shape = 21, fill = "white", size = 3) + # Match styling
  geom_text_repel( # Match styling
    data = cit, aes(x = lng, y = lat, label = city),
    size = 3, nudge_y = -0.1, min.segment.length = 0.5,
    point.padding = 0.5, box.padding = 0.5
  ) +
  theme_map() +
  theme(legend.key.width = unit(1, "cm")) # Wider key for gradient

p_left

# Figure A4 Right-hand Panel (Continuous % Increase Flats Post-1971)
# Using direct ggplot call as it uses scale_fill_gradient

# Ensure the fill variable exists and is numeric
shp_right <- shp %>% filter(!is.na(incr_flats_after71_pct))

p_right <- ggplot(shp_right) +
  geom_sf(aes(fill = incr_flats_after71_pct), size = 0.2, color = "black") +
  scale_fill_gradient(
    low = "#fff7bc", high = "#d95f0e", # Consistent color scale
    name = "% Increase in flat construction\n(Post-1971 vs Pre-1971)", # Updated label
    na.value = "grey80", # Explicit NA color
    labels = scales::percent_format(accuracy = 1) # Format labels as percentages
  ) +
  geom_point(data = cit, aes(x = lng, y = lat), shape = 21, fill = "white", size = 3) + # Match styling
  geom_text_repel( # Match styling
    data = cit, aes(x = lng, y = lat, label = city),
    size = 3, nudge_y = -0.1, min.segment.length = 0.5,
    point.padding = 0.5, box.padding = 0.5
  ) +
  theme_map() +
  theme(legend.key.width = unit(1, "cm")) # Wider key for gradient

p_right
