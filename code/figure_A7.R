# Clear environment
rm(list = ls())

# Load packages
pacman::p_load(dplyr, readr, haven, lfe, broom, ggplot2, forcats)

# Resolve conflicts if needed
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Tidy_felm helper function
tidy_felm <- function(model, add_glance = T, add_dv_stats = T, add_conf_90 = T) {
    n <- sum(!is.na(model$residuals))
    m_tidy <- broom::tidy(model, conf.int = T)
    out <- m_tidy %>% mutate(n = n)
    if (add_conf_90) {
        out <- out %>% mutate(conf.low90 = estimate - qnorm(0.95) *
            std.error, conf.high90 = estimate + qnorm(0.95) *
            std.error)
    }
    dv <- model$formula %>%
        as.character() %>%
        str_split(" ~ ",
            simplify = T
        ) %>%
        .[2]
    dv_mean <- model %>%
        augment() %>%
        pull(!!dv) %>%
        mean(na.rm = T)
    dv_sd <- model %>%
        augment() %>%
        pull(!!dv) %>%
        sd(na.rm = T)
    dv_min <- model %>%
        augment() %>%
        pull(!!dv) %>%
        min(na.rm = T)
    dv_max <- model %>%
        augment() %>%
        pull(!!dv) %>%
        max(na.rm = T)
    if (add_dv_stats) {
        out <- out %>% mutate(
            dv_mean = dv_mean, dv_sd = dv_sd,
            dv_min = dv_min, dv_max = dv_max
        )
    }
    if (add_glance) {
        g <- model %>%
            glance() %>%
            dplyr::rename(
                f_stat = statistic,
                f_pval = p.value
            ) %>%
            dplyr::select(matches("squared|f_"))
        out <- out %>% mutate(
            rsq = g$r.squared, a_rsq = g$adj.r.squared,
            f_stat = g$f_stat, f_pval = g$f_pval
        )
        out
    } else {
        out
    }
}

## Load Crabtree covariates
# Combine loading, joining, and selecting into one pipe
covs <- readRDS("data/crabtree_covs.rds") %>%
    mutate(wgtv = ifelse(ITM_Dresden == TRUE, 1, 0)) %>%
    left_join(readRDS("data/ids_county_ags.rds"), by = "county.id") %>% # Specify join key explicitly if different names
    # dplyr::select(-one_of("county.id")) %>% # Removed as AGS is the key now
    dplyr::select(AGS, where(is.numeric)) # Keep only AGS and numeric columns

crabtree_covars <- c("wgtv", "industry")

## Get 1953 protest - Hans data
prot <- haven::read_dta("data/GDRProtest1953.dta")

## 1963 cross-section of main panel data set
# Combine loading, filtering, selecting, and joining into one pipe
main_df <- read_rds("data/data_main.rds") %>%
    filter(year == 1963) %>%
    # Use any_of for potentially missing columns, though these are expected here
    dplyr::select(-any_of(c("territory_1988", "dist_Berlin", "dist_border"))) %>%
    left_join(prot, by = c("county.id" = "GDRCountyID1989")) %>%
    left_join(covs %>% dplyr::select(AGS, all_of(crabtree_covars)), by = "AGS")

## Define covariates to keep from main DF
# No changes needed here, variable lists are clear
covar_list <- c(
    "pop_1969",
    "critInd_any",
    "pop_den_1969",
    "births_pc63_71",
    "deaths_pc63_71",
    "protestMun1953_share"
)

## Residualization variables
res_covars <- c(
    "qual_flats_capita",
    "qual_flats_share_kitchen",
    "qual_flats_share_toilet",
    "qual_flats_share_heating",
    "qual_flats_avg_age_imputed1910"
)

covars_all <- c(covar_list, res_covars, crabtree_covars)

## Define treat variable
treat_var <- "treated_median_resid_1963" # Keep as string for formula construction

## Subset and standardize
# Combine select, filter, and mutate using across
# Use the actual variable name in filter for clarity
main_df <- main_df %>%
    dplyr::select(any_of(c(
        "county.id", "treated_median_resid_1963",
        covars_all, "Bezirk", "treated_cont_resid_1963"
    ))) %>%
    filter(!is.na(treated_median_resid_1963)) %>% # Filter using the actual column
    mutate(across(all_of(covars_all), scale)) # Use across() with all_of()

## Define model
important_covars <- c(
    covar_list, res_covars,
    "industry", "wgtv"
)

# Formula construction remains the same
m <- as.formula(paste(treat_var, "~",
    paste(important_covars, collapse = "+"),
    "| 0 | 0 | 0",
    sep = ""
))

# FELM estimation and tidying remain the same
res_2 <- felm(m, data = main_df) %>%
    tidy_felm() %>%
    filter(term != "(Intercept)")

# Combine mutate calls for recoding and reordering factors
res_2 <- res_2 %>%
    mutate(
        term_label = dplyr::recode(term,
            "pop_1969" = "Population (1969)",
            "critInd_any" = "Critical industry",
            "pop_den_1969" = "Population density (1969)",
            "births_pc63_71" = "Births p.c. (1963 - 1971)",
            "deaths_pc63_71" = "Deaths p.c. (1963 - 1971)",
            "protestMun1953_share" = "Protest 1953\n(share of municipalities)",
            "qual_flats_capita" = "Housing quality: number of flats p.c.",
            "qual_flats_share_kitchen" = "Housing quality: kitchens",
            "qual_flats_share_toilet" = "Housing quality: in-unit toilets",
            "qual_flats_share_heating" = "Housing quality: modern heating",
            "qual_flats_avg_age_imputed1910" = "Housing quality: average age",
            "industry" = "Industry share (Crabtree et. al.)",
            "wgtv" = "West-German TV (Crabtree et. al.)"
        ),
        term_label = fct_reorder(term_label, -estimate) # Reorder within the same mutate
    )

# Plotting remains the same
p2 <- ggplot(res_2, aes(x = term_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar(width = 0) +
    geom_point() +
    theme_bw() +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(
        x = "",
        y = "Coefficient estimate\n(x-standardized)"
    )

p2
