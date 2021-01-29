
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

data <- readRDS("data/persistentpoverty.rds")

data <- data %>%
  mutate_if(is.numeric, ~round(., 2))

persistenttables <- list()

persistenttables[["source"]] <- "Source: Understanding Society Survey"

persistenttables[["tableScotland"]] <- data %>%
  filter(nation == "Scotland") %>%
  mutate(type = str_c(housingcosts, group),
         value = ifelse(housingcosts == "sample",
                        comma(value, 1), percent(value, 1))) %>%
  select(period, value, type) %>%
  spread(type, value)

# Table 1 ----
data_pp <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation),
         value = ifelse(housingcosts == "sample",
                        comma(value, 1), percent(value, 1)))

dataAHC <- data_pp %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

dataBHC <- data_pp %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

data_sample <- data_pp %>%
  filter(housingcosts == "sample") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table1"]] <- rbind(dataAHC, dataBHC, data_sample)

# Table 2 ----
data_ch <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation),
         value = ifelse(housingcosts == "sample",
                        comma(value, 1), percent(value, 1)))

dataAHC <- data_ch %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

dataBHC <- data_ch %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

data_sample <- data_ch %>%
  filter(housingcosts == "sample") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table2"]] <- rbind(dataAHC, dataBHC, data_sample)

# Table 3 ----
data_wa <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation),
         value = ifelse(housingcosts == "sample",
                        comma(value, 1), percent(value, 1)))

dataAHC <- data_wa %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

dataBHC <- data_wa %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

data_sample <- data_wa %>%
  filter(housingcosts == "sample") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table3"]] <- rbind(dataAHC, dataBHC, data_sample)

# Table 4 ----
data_pn <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation),
         value = ifelse(housingcosts == "sample",
                        comma(value, 1), percent(value, 1)))

dataAHC <- data_pn %>%
  filter(housingcosts == "AHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

dataBHC <- data_pn %>%
  filter(housingcosts == "BHC") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

data_sample <- data_pn %>%
  filter(housingcosts == "sample") %>%
  select(-housingcosts) %>%
  spread(nation, value) %>%
  select(period, Scotland, England, Wales, "Northern Ireland", UK)

persistenttables[["table4"]] <- rbind(dataAHC, dataBHC, data_sample)

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())
