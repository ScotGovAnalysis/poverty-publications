
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

data <- readRDS("data/persistentpoverty.rds")

data <- data %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  mutate(group = factor(group, levels = c("pp", "ch", "wa", "pn")),
         value = ifelse(housingcosts == "sample", comma(value, 1),
                        percent(value, 1))) %>%
  rename(Period = period)


persistenttables <- list()

persistenttables[["source"]] <- "Source: Understanding Society Survey"

# Scotland table ----
persistenttables[["tableScotland"]] <- data %>%
  filter(nation == "Scotland") %>%
  mutate(type = str_c(housingcosts, group)) %>%
  select(Period, value, type) %>%
  spread(type, value)

# Table 1 ----
data_pp <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

persistenttables[["table1"]] <- data_pp %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table2"]] <- data_pp %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

# Table 2 ----
data_ch <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

persistenttables[["table3"]] <- data_ch %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table4"]] <- data_ch %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

# Table 3 ----
data_wa <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

persistenttables[["table5"]] <- data_wa %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table6"]] <- data_wa %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

# Table 4 ----
data_pn <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

persistenttables[["table7"]] <- data_pn %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table8"]] <- data_pn %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(Period, Scotland, England, Wales, "Northern Ireland", UK)

# Table 10 sample size
persistenttables[["table10"]] <- data %>%
  filter(housingcosts == "sample",
         nation == "Scotland") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())
