library(tidyverse)
library(scales)
library(ggiraph)

source("R/00_functions.R")
source("R/00_colours.R")

persistentcharts <- list()

data <- readRDS("data/persistentpoverty.rds")

source <- "Source: Understanding Society, 2010-2011 to 2017-2018"

data <- data %>%
  mutate(value = ifelse(housingcosts == "sample", value, round(value, 2))) %>%
  mutate(group = factor(group,
                         levels = c("pp", "ch", "wa", "pn"),
                         labels = c("All individuals", "Children", "Working-age adults", "Pensioners")))

# UK nations AHC and BHC -------------------------------------------------------

persistentcharts[["chart01"]] <- data %>%
  filter(group == "All individuals",
         housingcosts == "AHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart02"]] <- data %>%
  filter(group == "All individuals",
         housingcosts == "BHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart03"]] <- data %>%
  filter(group == "Children",
         housingcosts == "AHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart04"]] <- data %>%
  filter(group == "Children",
         housingcosts == "BHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart05"]] <- data %>%
  filter(group == "Working-age adults",
         housingcosts == "AHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart06"]] <- data %>%
  filter(group == "Working-age adults",
         housingcosts == "BHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart07"]] <- data %>%
  filter(group == "Pensioners",
         housingcosts == "AHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart08"]] <- data %>%
  filter(group == "Pensioners",
         housingcosts == "BHC",
         nation != "Total") %>%
  select(-group, -housingcosts) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

# Scotland chart AHC (for briefing only) ---------------------------------------

persistentcharts[["Scotland"]] <- data %>%
  filter(nation == "Scotland",
         housingcosts == "AHC",
         nation != "Total") %>%
  select(-nation, -housingcosts) %>%
  mutate(key = group) %>%
  persistentchart()

saveRDS(persistentcharts, "data/persistentcharts.rds")
rm(list = ls())
