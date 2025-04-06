# Clear environment
rm(list = ls())

# Load packages
pacman::p_load(broom, fixest)

tidy_feols <- function(model, ...) {
        if (!is.null(names(model))) {
            tidy_feols_single(model, ...)
        } else {
            lapply(model, function(x) {
                tidy_feols_single(x, ...)
            }) %>% reduce(bind_rows)
        }

tidy_feols_single <- function(
    model, add_glance = T, add_dv_stats = T, add_conf_90 = T,
    add_first_rhs_stats = T) {
    if (!names(model)[1] == "nobs") {
        cat("Looks like this model uses more than one outcome;\nPlease use tidy_feols instead")
    } else {
        n <- model$nobs
        m_tidy <- broom::tidy(model, conf.int = F) %>% mutate(conf.low = estimate -
            qnorm(0.975) * std.error, conf.high = estimate +
            qnorm(0.975) * std.error)
        if (n > 0) {
            dv_val <- model.matrix(model, type = "lhs")
        } else {
            dv_val <- NA
        }
        out <- m_tidy %>% mutate(n = n)
        if (add_conf_90) {
            out <- out %>% mutate(conf.low90 = estimate - qnorm(0.95) *
                std.error, conf.high90 = estimate + qnorm(0.95) *
                std.error)
        }
        dv_mean <- dv_val %>% mean(na.rm = T)
        dv_sd <- dv_val %>% sd(na.rm = T)
        dv_min <- dv_val %>% min(na.rm = T)
        dv_max <- dv_val %>% max(na.rm = T)
        if (is.nan(dv_mean)) {
            dv_mean <- NA
        }
        if (is.nan(dv_sd)) {
            dv_sd <- NA
        }
        if (is.nan(dv_min)) {
            dv_min <- NA
        }
        if (is.nan(dv_max)) {
            dv_max <- NA
        }
        if (add_dv_stats) {
            out <- out %>% mutate(
                dv_mean = dv_mean, dv_sd = dv_sd,
                dv_min = dv_min, dv_max = dv_max
            )
        }
        if (add_first_rhs_stats) {
            if (n > 0) {
                rhs <- model.matrix(model, type = "rhs")
                if (!all(rhs[, 1] == 1)) {
                    first_rhs <- rhs[, 1]
                } else {
                    first_rhs <- rhs[, 2]
                }
                first_rhs_mean <- first_rhs %>% mean(na.rm = T)
                first_rhs_sd <- first_rhs %>% sd(na.rm = T)
                first_rhs_min <- first_rhs %>% min(na.rm = T)
                first_rhs_max <- first_rhs %>% max(na.rm = T)
            } else {
                first_rhs_mean <- NA
                first_rhs_sd <- NA
                first_rhs_min <- NA
                first_rhs_max <- NA
            }
            out <- out %>% mutate(
                first_rhs_mean = first_rhs_mean,
                first_rhs_sd = first_rhs_sd, first_rhs_min = first_rhs_min,
                first_rhs_max = first_rhs_max
            )
        }
        fml <- model$fml
        dv_lab <- fml %>%
            as.character() %>%
            {
                .[2]
            }
        fml <- Reduce(paste, deparse(fml)) %>%
            str_squish() %>%
            str_replace_all("\"", "")
        out <- out %>% mutate(dv = dv_lab, fml = deparse(fml))
        fe_vars <- paste0(model$fixef_vars, collapse = ", ")
        out <- out %>% mutate(fixef_vars = fe_vars)
        if (add_glance) {
            g <- model %>%
                broom::glance() %>%
                dplyr::select(matches("squared|f_"))
            out <- out %>% mutate(rsq = g$r.squared, a_rsq = g$adj.r.squared)
            out %>% dplyr::select(term, dv, fml, everything())
        } else {
            out %>% dplyr::select(term, dv, fml, everything())
        }
    }
}