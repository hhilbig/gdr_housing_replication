# Clear environment
rm(list = ls())

# Load packages
pacman::p_load(tidyverse, pbapply, lfe, broom)

## Get data

df <- read_rds("data/data_main.rds") %>%
  filter(year > 1960)

# Function to tidy felm objects

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

## Define treatment variables

outcomevars <- c("flats_constr_public_pc")
treatvars <- c(
  "treated_mean_resid_1963",
  "treated_median_resid_1963",
  "treated_cont_resid_1963",
  "treated_mean_resid_1971",
  "treated_median_resid_1971",
  "treated_cont_resid_1971",
  "treated_mean_resid_census71_1963",
  "treated_median_resid_census71_1963",
  "treated_cont_resid_census71_1963",
  "treated_mean_resid_census71_1971",
  "treated_median_resid_census71_1971",
  "treated_cont_resid_census71_1971"
)
othervars <- c(
  "incr_flats_after71_pct",
  "incr_flats_after71_abs", "prot_share"
)

## Aggregate

df <- df %>%
  group_by(county.id, period_agg_2years, Bezirk) %>%
  summarise_at(vars(one_of(c(treatvars, outcomevars, othervars, "post"))),
    mean,
    na.rm = T
  )

## Data

df <- df %>%
  dplyr::select(
    one_of(treatvars),
    one_of(outcomevars),
    one_of(othervars),
    period_agg_2years,
    county.id,
    post,
    Bezirk
  ) %>%
  mutate(period = as.numeric(factor(period_agg_2years))) %>%
  mutate(period_bezirk = paste0(period_agg_2years, "_", Bezirk))

## Define subsets ##
ss_list <- list(
  df,
  df %>% filter(!incr_flats_after71_pct > quantile(df$incr_flats_after71_pct, 0.95, na.rm = T))
)
ss_labels <- c("Full", "Rem outlier")

## Loop

out <- pblapply(treatvars, function(tv) {
  pblapply(1:length(ss_list), function(ss_id) {
    df_ss <- ss_list[[ss_id]]

    ## Treated and treated * post

    df_ss$treat_temp <- df_ss %>% pull(!!tv)
    df_ss$treat_temp_post <- df_ss$treat_temp * df_ss$post

    ## Get results

    results <- lapply(outcomevars, function(dv) {
      ## Standard w/ multiple periods

      model_formula1 <- as.formula(paste0(
        dv,
        " ~ post + treat_temp + treat_temp_post",
        "| 0 | 0 | county.id"
      ))

      ## Estimate

      model_est1 <- felm(model_formula1, data = df_ss) %>%
        tidy_felm() %>%
        filter(term == "treat_temp_post") %>%
        mutate(outcome = paste(dv)) %>%
        mutate(covars = "No") %>%
        mutate(treatvar = tv) %>%
        mutate(ss = ss_labels[ss_id]) %>%
        mutate(model = "1 No FE")

      model_stats1 <- felm(model_formula1, data = df_ss) %>%
        glance()

      ## 2WFE

      model_formula3 <- as.formula(paste0(
        dv, " ~ treat_temp_post",
        "| period_agg_2years + county.id | 0 | county.id"
      ))


      model_est3 <- felm(model_formula3, data = df_ss) %>%
        tidy_felm() %>%
        filter(term == "treat_temp_post") %>%
        mutate(outcome = paste(dv)) %>%
        mutate(covars = "No") %>%
        mutate(treatvar = tv) %>%
        mutate(ss = ss_labels[ss_id]) %>%
        mutate(model = "2 2WFE")

      ## 2WFE w/o protest, Bezirk*period

      model_formula5 <- as.formula(paste0(
        dv, " ~ treat_temp_post ",
        "| period_bezirk + county.id | 0 | county.id"
      ))


      model_est5 <- felm(model_formula5, data = df_ss) %>%
        tidy_felm() %>%
        filter(term %in% c("treat_temp_post", "prot_share_post")) %>%
        mutate(outcome = paste(dv)) %>%
        mutate(covars = "No") %>%
        mutate(treatvar = tv) %>%
        mutate(ss = ss_labels[ss_id]) %>%
        mutate(model = "3 2WFE, Period*Bezirk FE")

      ## Combine

      rbind(
        model_est1, model_est3, model_est5
      )
    }) %>% reduce(rbind)

    results
  }) %>% reduce(rbind)
}) %>%
  reduce(rbind) %>%
  mutate(resid_type = case_when(
    str_detect(treatvar, "census71") ~ "census71",
    TRUE ~ "Main"
  )) %>%
  mutate(treatvar = str_remove_all(treatvar, "_census71")) %>%
  separate(
    col = treatvar, sep = -5,
    into = c("type", "year_start")
  ) %>%
  mutate(year_start = as.numeric(substr(year_start, 2, 5))) %>%
  mutate(type = dplyr::recode(type,
    `treated_mean_resid` = "Split based\non mean",
    `treated_median_resid` = "Split based\non median",
    `treated_cont_resid` = "Continuous"
  ))

## Save estimates

write_rds(out, "results/est_2wfe.rds")
