
# Prep data for charts in child poverty update

library(tidyverse)

source("R/00_strings.R")
source("R/00_functions.R")

hbai <- readRDS("data/tidyhbai.rds")
persistent <- readRDS("data/persistentpoverty.rds")

yearlevels <- factor(labels[["years_exp"]]$years_exp,
                     levels = as.character(labels[["years_exp"]]$years_exp)) %>%
  levels()

cp_data <- list()

# Rel pov ----
cp_data[["rel"]] <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "low60ahc")) %>%
  addyearvar() %>%
  select(years, chrate) %>%
  mutate(chrate3 = get3yrcentavg(chrate),
         years = factor(years, levels = yearlevels),
         years_formatted = factor(years, levels = yearlevels,
                                  labels = labels[["years_exp"]]$formatted),
         text = str_c(years_formatted, ": ", percent(chrate, 1)),
         value = round2(chrate, 4),
         chrate3 = round2(chrate3, 4))

# Abs pov ----
cp_data[["abs"]] <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "abspovahc")) %>%
  addyearvar() %>%
  select(years, chrate) %>%
  mutate(chrate3 = get3yrcentavg(chrate),
         years = factor(years, levels = yearlevels),
         years_formatted = factor(years, levels = yearlevels,
                                  labels = labels[["years_exp"]]$formatted),
         text = str_c(years_formatted, ": ", percent(chrate, 1)),
         value = round2(chrate, 4),
         chrate3 = round2(chrate3, 4))

# Mat dep ----
cmdahc_new <- do.call(rbind.data.frame,
                      lapply(hbai, getpov, povvar = "cmdahc_new")) %>%
  addyearvar() %>%
  select(years, chrate) %>%
  rename(chrate_new = chrate)

cp_data[["md"]] <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc")) %>% addyearvar() %>%
  left_join(cmdahc_new, by = "years") %>%
  select(years, chrate, chrate_new) %>%
  mutate(chrate3 = ifelse(years %in% years[11:16], get3yrcentavg(chrate),
                         ifelse(years == 1011, NA,
                                ifelse(years == 1112, (chrate + lag(chrate, 1L) + lead(chrate))/3,
                                       get3yrcentavg(chrate)))),
         years = factor(years, levels = yearlevels),
         years_formatted = factor(years, levels = yearlevels,
                                  labels = labels[["years_exp"]]$formatted),
         text = str_c(years_formatted, ": ", percent(chrate, 1)),
         text = ifelse(years == 1011, str_c("Old methodology ", text), text),
         text_new = ifelse(years == 1011, str_c("New methodology ",
                                                years_formatted,
                                                ": ",
                                                percent(chrate_new,1)), NA),
         value = round2(chrate, 4),
         chrate3 = round2(chrate3, 4))

# Pers pov ----

cp_data[["pers"]] <- persistent %>%
  filter(housingcosts == "AHC",
         group == "ch") %>%
  select(period, Scotland) %>%
  rename(chrate = Scotland) %>%
  mutate(years = str_sub(period, -2L, -1L),
         years = str_c(lag(years), years),
         years = ifelse(is.na(years), "1314", years),
         years = factor(years, levels = yearlevels, ordered = TRUE),
         text = str_c(period, ": ", percent(chrate, 1)),
         chrate3 = get3yrcentavg(chrate),
         value = round2(chrate, 4),
         chrate3 = round2(chrate3, 4))

saveRDS(cp_data, "data/cp_data.rds")
rm(list = ls())

