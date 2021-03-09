
# Prep data for charts in child poverty update

library(tidyverse)

source("R/00_strings.R")
source("R/00_functions.R")

hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland",
         gs_newch > 0)

persistent <- readRDS("data/persistentpoverty.rds")

cp_data <- list()

# Rel pov ----------------------------------------------------------------------
cp_data$rel <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch") %>%
  mutate(x = factor(yearn, levels = labels$years$numbered,
                        labels = labels$years$formatted,
                        ordered = TRUE),
         x = fct_expand(x, levels(labels$years_exp$formatted)),
         y = rate,
         label = fmtpct(y),
         tooltip = str_c(x, ": ", fmtpct(rate)),
         y3 = get3yrcentavg(y)) %>%
  mutate_at(vars(y3, y), ~round2(., 4)) %>%
  select(x, y, y3, tooltip, label)

# Abs pov ----------------------------------------------------------------------
cp_data$abs <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newch") %>%
  mutate(x = factor(yearn, levels = labels$years$numbered,
                        labels = labels$years$formatted,
                        ordered = TRUE),
         x = fct_expand(x, levels(labels$years_exp$formatted)),
         y = rate,
         label = fmtpct(y),
         tooltip = str_c(x, ": ", fmtpct(rate)),
         y3 = get3yrcentavg(y)) %>%
  mutate_at(vars(y3, y), ~round2(., 4)) %>%
  select(x, y, y3, tooltip, label)

# Mat dep ----------------------------------------------------------------------
cmdahc_new <- getpovby(hbai, pov = "cmdahc_new", weight = "gs_newch")

cp_data$md <- getpovby(hbai, pov = "cmdahc", weight = "gs_newch") %>%
  rbind(cmdahc_new) %>%
  arrange(yearn) %>%
  mutate(y3 = case_when(yearn %in% seq(11, 16, 1) ~ get3yrcentavg(rate),
                        TRUE ~ get3yrcentavg(rate)),
         y3 = ifelse(yearn == 17, NA, y3),
         x = factor(yearn, levels = labels$years$numbered,
                        labels = labels$years$formatted,
                        ordered = TRUE),
         x = fct_expand(x, levels(labels$years_exp$formatted)),
         y = rate,
         label = fmtpct(y),
         tooltip = str_c(x, ": ", fmtpct(y)),
         tooltip = case_when(yearn == 17 & type == "cmdahc" ~ str_c("Old methodology ", tooltip),
                             yearn == 17 & type == "cmdahc_new" ~ str_c("New methodology ", x, ": ", fmtpct(y)),
                           TRUE ~ tooltip)) %>%
  mutate_at(vars(y, y3), ~round2(., 4)) %>%
  select(x, y, y3, tooltip, label)

# Pers pov ---------------------------------------------------------------------

cp_data$pers <- persistent %>%
  filter(housingcosts == "AHC",
         group == "ch",
         nation == "Scotland") %>%
  select(period, value) %>%
  rename(y = value) %>%
  mutate(x = str_sub(period, -2L, -1L),
         x = str_c(x, as.numeric(x) + 1),
         x = factor(x, levels = labels$years_exp$years,
                        labels = labels$years_exp$formatted,
                        ordered = TRUE),
         tooltip = str_c(period, ": ", fmtpct(y)),
         label = fmtpct(y),
         y3 = get3yrcentavg(y),
         y = round2(y, 4),
         y3 = round2(y3, 4))

saveRDS(cp_data, "data/cp_data.rds")
rm(list = ls())

