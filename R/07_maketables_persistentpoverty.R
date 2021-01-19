
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

data <- readRDS("data/persistentpoverty.rds")

persistenttables <- list()

persistenttables[["source"]] <- "Source: Understanding Society Survey"

persistenttables[["table1"]] <- data %>%
  mutate(type = str_c(housingcosts, group)) %>%
  select(period, Scotland, type) %>%
  mutate(Scotland = percent(Scotland, 1)) %>%
  spread(type, Scotland) %>%
  select(period, BHCpp, BHCch, BHCwa, BHCpn, AHCpp, AHCch, AHCwa, AHCpn)

persistenttables[["table2"]] <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

persistenttables[["table3"]] <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

persistenttables[["table4"]] <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

persistenttables[["table5"]] <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())
