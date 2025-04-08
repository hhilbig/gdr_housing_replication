# Clear environment
rm(list = ls())

# Load packages
pacman::p_load(
  tidyverse, # For data manipulation (dplyr, readr, etc.) and plotting (ggplot2)
  lubridate, # For date/time manipulation (if needed, check usage)
  conflicted
)

# --- Load and Prepare Main Dataset ---

df_final <- read_rds("data/data_main.rds") %>%
  filter(year > 1960) %>%
  mutate(period_rel = year - 1974) %>%
  # Remove specific columns not needed for these plots
  dplyr::select(-matches("nonhousing"), -matches("cont"), -matches("census"))

# Create a dictionary mapping aggregated periods to year ranges
dict_df <- df_final %>%
  distinct(period_agg_2years, year) %>%
  group_by(period_agg_2years) %>%
  mutate(index = paste0("y", 1:n())) %>%
  pivot_wider(names_from = index, values_from = year) %>%
  ungroup() %>% # Ungroup after pivot
  mutate(period_proper = paste0("[", y1, ", ", y2, "]"))

# --- Prepare Data for Figure 1 and Figure A3 ---

plot_df_base <- df_final %>%
  filter(period_agg_2years != -6) %>% # Exclude period -6
  dplyr::select(
    period_agg_2years, county.id, flats_constr_public_pc,
    matches("treated_.*_resid") # Select relevant treatment variables
  ) %>%
  pivot_longer(
    cols = matches("treated_.*_resid"),
    names_to = "treatment",
    values_to = "treated"
  ) %>%
  filter(!is.na(treated)) %>% # Remove rows where treatment is NA
  # Calculate mean construction per capita for each group
  group_by(
    period_agg_2years, treatment, treated
  ) %>%
  summarise(
    m = mean(flats_constr_public_pc, na.rm = TRUE),
    n = n(),
    .groups = "drop" # Drop grouping after summarise
  ) %>%
  # Extract treatment type and start year
  separate(
    col = treatment, sep = "_", # Use underscore as separator
    into = c("treated_label", "split_type", "resid_label", "year_start"),
    remove = FALSE # Keep original treatment column if needed later, or set to TRUE
  ) %>%
  # Clean up columns
  mutate(
    year_start = as.numeric(year_start),
    treated = ifelse(treated == 1,
      "Number of petitions above median\nprior to the housing program",
      "Number of petitions below median\nprior to the housing program"
    ),
    # Recode split type for plot labels
    type = dplyr::recode(split_type,
      `mean` = "Split based\non mean",
      `median` = "Split based\non median"
    )
  ) %>%
  # Join with period dictionary and set factor levels
  left_join(dict_df %>% dplyr::select(period_agg_2years, period_proper), by = "period_agg_2years") %>%
  mutate(
    period_proper = factor(period_proper, levels = unique(period_proper[order(period_agg_2years)])), # Ensure correct order
    type = factor(type, levels = unique(type)) # Adjust levels if specific order needed
  ) %>%
  select(-treated_label, -resid_label, -split_type) # Remove intermediate columns


# --- Figure 1: Public Housing Construction Trends (Median Split, 1964) ---

# Filter data for Figure 1
plot_df_fig1 <- plot_df_base %>%
  filter(year_start == 1964 & type == "Split based\non median") # Adjusted filter logic

# Create Plot
p_fig1 <- ggplot(
  plot_df_fig1,
  aes(x = as.numeric(period_proper), y = m, linetype = factor(treated), group = factor(treated))
) +
  geom_vline(
    xintercept = (which(levels(plot_df_fig1$period_proper) == "[1970, 1971]") +
      which(levels(plot_df_fig1$period_proper) == "[1972, 1973]")) / 2,
    linetype = "dotted", color = "grey40"
  ) +
  geom_line() + # Removed aes mapping here as it's inherited
  ylab("Flats constructed / 1000 capita") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  scale_x_continuous(
    breaks = 1:length(levels(plot_df_fig1$period_proper)),
    labels = levels(plot_df_fig1$period_proper)
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_linetype_manual(
    name = "",
    values = c(
      "Number of petitions below median\nprior to the housing program" = "solid",
      "Number of petitions above median\nprior to the housing program" = "dashed"
    )
    # Removed labels as they are inferred from values
  ) +
  annotate("text",
    x = (which(levels(plot_df_fig1$period_proper) == "[1970, 1971]") +
      which(levels(plot_df_fig1$period_proper) == "[1972, 1973]")) / 2 + 0.1,
    y = Inf, vjust = 1.1, # Position relative to vline
    label = "Start of the housing program", angle = 90, size = 3.3, hjust = 1
  )

print(p_fig1) # Display the plot

# --- Figure A3: Public Housing Construction Trends (Multiple Splits/Years) ---

# Prepare data for Figure A3 by filtering the base prepared data
plot_df_figA3 <- plot_df_base %>%
  filter(year_start %in% c(1963, 1971)) %>% # Filter for specific start years
  mutate(
    year_start_str = case_when(
      year_start == 1963 ~ "Petitions measured\nbetween 1963-71",
      year_start == 1971 ~ "Petitions measured\nin 1971",
      TRUE ~ NA_character_
    ),
    # Ensure correct factor ordering if needed
    type = factor(type, levels = c("Split based\non mean", "Split based\non median")) # Adjust as per desired facet order
  ) %>%
  filter(!is.na(type) & !is.na(year_start_str)) # Ensure factors are valid

# Create Plot
p_figA3 <- ggplot(plot_df_figA3, aes(x = as.numeric(period_proper), y = m, linetype = factor(treated), group = factor(treated))) +
  geom_vline(
    xintercept = (which(levels(plot_df_figA3$period_proper) == "[1970, 1971]") +
      which(levels(plot_df_figA3$period_proper) == "[1972, 1973]")) / 2,
    linetype = "dotted", color = "grey40"
  ) +
  geom_line() +
  facet_grid(year_start_str ~ type) +
  scale_x_continuous(
    breaks = 1:length(levels(plot_df_figA3$period_proper)),
    labels = levels(plot_df_figA3$period_proper)
  ) +
  ylab("Flats constructed / 1000 capita") +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  scale_linetype_manual(
    name = "",
    values = c(
      "Number of petitions below median\nprior to the housing program" = "solid",
      "Number of petitions above median\nprior to the housing program" = "dashed"
    )
    # Removed labels
  ) +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5,
    hjust = 1
  )) +
  annotate("text",
    x = (which(levels(plot_df_figA3$period_proper) == "[1970, 1971]") +
      which(levels(plot_df_figA3$period_proper) == "[1972, 1973]")) / 2 - 0.1,
    y = Inf, vjust = -0.1, # Position relative to vline
    label = "Start of the housing program", angle = 90, size = 2.4, hjust = 0
  ) # Use hjust for alignment

print(p_figA3) # Display the plot

# --- Figure A1: Total Annual Public Housing Construction ---

# Load separate dataset for total construction
df_total <- read_rds("data/construction_data.rds") %>%
  mutate(n_flats_public = n_flats_total - n_flats_priv) %>%
  filter(year > 1960) %>%
  group_by(year) %>%
  summarise(h = sum(n_flats_public, na.rm = TRUE), .groups = "drop")

# Create Plot
p_figA1 <- ggplot(df_total, aes(x = year, y = h / 1000)) + # Use year directly
  geom_vline(xintercept = 1971.5, linetype = "dotted", color = "grey40") +
  geom_line() + # No grouping needed for a single time series
  ylab("Total number of flats\nconstructed annually (1000s)") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank()) + # No legend needed
  scale_x_continuous(breaks = seq(1960, max(df_total$year, na.rm = TRUE), 5)) # Adjust breaks based on data range

print(p_figA1) # Display the plot


# --- Figure A2: Cumulative Public Construction Post-1971 ---

# Prepare data for cumulative plot using the initial df_final
df_cum <- df_final %>%
  dplyr::select(
    year, pop_1969, treated_median_resid_1963, # Make sure this treatment variable exists and is correct
    post, county.id,
    flats_constr_public_pc
  ) %>%
  filter(post == 1) %>% # Filter for post-treatment period
  arrange(county.id, year) %>%
  group_by(county.id) %>%
  mutate(csum = cumsum(replace_na(flats_constr_public_pc, 0))) %>% # Calculate cumulative sum, handle NAs
  ungroup() %>%
  filter(!is.na(treated_median_resid_1963)) %>% # Ensure treatment group is not NA
  # Calculate weighted mean of cumulative sum by treatment group and year
  group_by(treated_median_resid_1963, year) %>%
  summarise(csum = weighted.mean(csum, pop_1969, na.rm = TRUE), .groups = "drop") %>%
  # Recode treatment for labels
  mutate(
    treatment_label = factor(ifelse(treated_median_resid_1963 == 1,
      "Number of petitions above median\nprior to the housing program",
      "Number of petitions below median\nprior to the housing program"
    ))
  )


# Create Plot
p_cum <- ggplot(df_cum, aes(x = year, y = csum, linetype = treatment_label, group = treatment_label)) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey60") +
  geom_hline(yintercept = 75, linetype = "dotted", color = "grey60") +
  geom_line() +
  ylab("Flat construction / 1000 capita\n(cumulative)") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  scale_x_continuous(breaks = seq(min(df_cum$year), max(df_cum$year), 2)) + # Dynamic breaks
  scale_linetype_manual(
    name = "", # Legend title
    values = c( # Keep original mapping for cumulative: Below=dashed, Above=solid
      "Number of petitions below median\nprior to the housing program" = "dashed",
      "Number of petitions above median\nprior to the housing program" = "solid"
    )
  )

print(p_cum) # Display the plot
