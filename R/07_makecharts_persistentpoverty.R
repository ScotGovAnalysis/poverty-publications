library(tidyverse)
library(scales)
library(ggiraph)

source("R/00_functions.R")
source("R/00_colours.R")

persistentcharts <- list()

data <- readRDS("data/persistentpoverty.rds")

source <- "Source: Understanding Society, 2010-2011 to 2017-2018"

data <- data %>%
  mutate(group = factor(group,
                         levels = c("pp", "ch", "wa", "pn"),
                         labels = c("All individuals", "Children", "Working-age adults", "Pensioners")),
         period = factor(period,
                         levels = c("2010 to 2014", "2011 to 2015",
                                    "2012 to 2016", "2013 to 2017",
                                    "2014 to 2018", "2015 to 2019"),
                         labels = c("2010-2014", "2011-2015", "2012-2016",
                                    "2013-2017", "2014-2018", "2015-2019"),
                         ordered = TRUE))

# UK nations AHC and BHC

persistentcharts[["chart01"]] <- data %>%
  filter(group == "All individuals",
         housingcosts == "AHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart02"]] <- data %>%
  filter(group == "All individuals",
         housingcosts == "BHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart03"]] <- data %>%
  filter(group == "Children",
         housingcosts == "AHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart04"]] <- data %>%
  filter(group == "Children",
         housingcosts == "BHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart05"]] <- data %>%
  filter(group == "Working-age adults",
         housingcosts == "AHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart06"]] <- data %>%
  filter(group == "Working-age adults",
         housingcosts == "BHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart07"]] <- data %>%
  filter(group == "Pensioners",
         housingcosts == "AHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

persistentcharts[["chart08"]] <- data %>%
  filter(group == "Pensioners",
         housingcosts == "BHC") %>%
  select(-group, -UK, -housingcosts) %>%
  gather(nation, value, -period) %>%
  mutate(nation = factor(nation, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
         key = nation) %>%
  persistentchart()

saveRDS(persistentcharts, "data/persistentcharts.rds")
rm(list = ls())
