# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(tidyverse, conflicted, relaimpo)

# Resolve conflicts if needed

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::between)
conflicts_prefer(dplyr::ungroup)

## Get data
df_raw <- read_rds("data/data_main.rds") %>%
  dplyr::select(
    county.id, year, post, pop_1969,
    flats_constr_public_pc, n_flats_public,
    qual_flats_avg_age_imputed1910, qual_flats_share_kitchen,
    qual_flats_share_toilet, qual_flats_share_heating,
    critInd_any, pop_den_1969, births_pc63_71, deaths_pc63_71,
    sed_members_popshare_1971, sed_share46, treated_cont_1971
  )

## Calculate necessary county-level aggregates
df_agg <- df_raw %>%
  arrange(county.id, year) %>%
  group_by(county.id) %>%
  summarise(
    flats_avg_constr_change = mean(flats_constr_public_pc[post == 1 & year > 1945], na.rm = T) -
      mean(flats_constr_public_pc[post == 0 & year > 1945], na.rm = T),
    n_flats_public_1945 = sum(n_flats_public[year < 1946], na.rm = T),
    flats_public_avg_constr_45_71 = mean(flats_constr_public_pc[between(year, 1946, 1970)], na.rm = T),
    pop_1969 = first(pop_1969)
  ) %>%
  mutate(
    flats_public_pc_1945 = n_flats_public_1945 / pop_1969,
  ) %>%
  ungroup() %>%
  distinct(county.id, .keep_all = T)

## Prepare data for PCA and model (using 1971 values for time-varying predictors)
pca_vars <- c(
  "qual_flats_avg_age_imputed1910", "qual_flats_share_kitchen",
  "qual_flats_share_toilet", "qual_flats_share_heating"
)

df_model <- df_raw %>%
  filter(year == 1971) %>%
  dplyr::select(
    county.id, all_of(pca_vars), critInd_any, pop_den_1969,
    births_pc63_71, deaths_pc63_71, sed_members_popshare_1971,
    sed_share46, treated_cont_1971
  ) %>%
  left_join(df_agg, by = "county.id") %>%
  filter(!is.na(qual_flats_avg_age_imputed1910))

## Perform PCA on quality variables
pca_mat <- df_model[, pca_vars] %>%
  mutate(across(everything(), scale)) %>%
  as.matrix()

pca_results <- princomp(pca_mat)
df_model <- df_model %>%
  mutate(qual_pc1 = pca_results$scores[, 1])

## Define variable sets for the model
qvars <- c(
  "qual_pc1",
  "flats_public_pc_1945",
  "flats_public_avg_constr_45_71"
)
othervars <- c(
  "pop_1969",
  "critInd_any",
  "pop_den_1969",
  "births_pc63_71",
  "deaths_pc63_71",
  "sed_members_popshare_1971",
  "sed_share46"
)
petvars <- c("treated_cont_1971")
yvars <- c("flats_avg_constr_change")

## Combine control variables
cvars <- c(qvars, othervars)

## Define dictionary for plot labels
dict <- data.frame(
  orig = c(
    qvars, othervars, petvars,
    "treated_median_1971", "prot_share", "marriages_pc63_71"
  ),
  new_name = c(
    "Housing quality\n(1971, from PCA)", "Stock of public flats p.c., 1945",
    "Avg. p.c. public\nconstruction, 1945-1971", "Population (1969)",
    "Any critical industry", "Population density (1969)",
    "Births p.c. (1963-1971)", "Deaths p.c. (1963-1971)",
    "SED member p.c. (1971)", "SED vote share (1946)", "Petitions",
    "Petitions", "Protest share", "Marriages p.c. (1963-1971)"
  ),
  new_name2 = c(
    "Housing quality\n(indicators)", "Housing stock (1945)",
    "New housing\nconstruction (1945-1971)", "Population (1969)",
    "Critical export industry", "Urbanization (1969)",
    "Births p.c. (1963-1971)", "Deaths p.c. (1963-1971)",
    "SED member p.c. (1971)", "SED vote share (1946)", "Petitions (1963-1971)",
    "Petitions (1963-1971)", "Protest share", "Marriages p.c. (1963-1971)"
  )
) %>% distinct(orig, .keep_all = TRUE)

## Build model formula
f1 <- paste(
  yvars[1], "~", petvars[1], "+",
  paste0(cvars, collapse = "+")
)

## Run the linear model
m1 <- lm(f1, data = df_model)

## Calculate relative importance
imp1 <- relaimpo::calc.relimp(m1)

## Process results for plotting
ilist <- data.frame(
  lmg = imp1@lmg,
  v = names(imp1@lmg),
  y = imp1@namen[1],
  petvar = names(imp1@lmg)[1]
) %>%
  left_join(dict, by = c("v" = "orig")) %>%
  mutate(new_name2 = ifelse(is.na(new_name2), v, new_name2)) %>%
  mutate(
    y_label = "P.c. change in construction\n(pre/post 1971)",
    petvar_label = "Cont. petition measure"
  ) %>%
  mutate(new_name2 = fct_reorder(new_name2, lmg))

## Generate Plot
p <- ilist %>% ggplot(aes(x = new_name2, y = lmg)) +
  geom_bar(
    stat = "identity", fill = "grey40",
    color = "black", width = 0.85
  ) +
  coord_flip() +
  theme_bw() +
  ylab(expression(paste(
    "Average contribution to ", R^2, " (LMG method)"
  ))) +
  xlab("") +
  ggtitle(paste("Relative Importance for Outcome:", unique(ilist$y_label)))

p
