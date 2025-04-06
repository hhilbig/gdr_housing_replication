# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman") # Ensure pacman is available

pacman::p_load(
  tidyverse # For data manipulation (dplyr, readr) and plotting (ggplot2)
)

##

out <- read_rds("results/est_2wfe.rds") %>%
  filter(resid_type != "census71") %>%
  filter(str_detect(type, "mean|median|Continuous")) %>%
  filter(year_start %in% c(1963, 1971)) %>%
  filter(ss == "Full") %>%
  mutate(year_start_str = case_when(
    year_start == 1963 ~ "Petitions measured\nbetween 1963-71",
    year_start == 1971 ~ "Petitions measured\nin 1971",
    T ~ NA_character_
  )) %>%
  mutate(type = ifelse(str_detect(type, "Cont"), "Continuous\ntreatment", type)) %>%
  mutate(type = factor(type, levels = unique(type)[c(2, 3, 1)])) %>%
  mutate(model2 = case_when(
    str_detect(model, "No FE") ~ "No fixed effects",
    str_detect(model, "2 2") ~ "County & period\nfixed effects",
    str_detect(model, "Bezirk") ~ "County & period*district\nfixed effects"
  )) %>%
  mutate(model2 = factor(model2, levels = unique(model2)[c(3:1)]))

## Plot this

pd <- position_dodge(0.4)

ggplot(
  out,
  aes(
    x = model2, y = estimate,
    ymin = conf.low, ymax = conf.high
  )
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(
    width = 0,
    position = pd
  ) +
  geom_point(
    shape = 21,
    position = pd,
    fill = "white",
    size = 2
  ) +
  theme_bw() +
  ylab("Estimated effect on\nflats constructed / 1,000 capita") +
  xlab("Model") +
  facet_grid(type ~ year_start_str) +
  theme(legend.position = "bottom") +
  coord_flip()
