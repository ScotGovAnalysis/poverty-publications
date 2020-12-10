
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

data <- readRDS("data/persistentpoverty.rds")

pers_source <- "Source: Understanding Society Survey"

pers_table1 <- data %>%
  mutate(type = str_c(housingcosts, group)) %>%
  select(period, Scotland, type) %>%
  mutate(Scotland = percent(Scotland, 1)) %>%
  spread(type, Scotland) %>%
  select(period, BHCpp, BHCch, BHCwa, BHCpn, AHCpp, AHCch, AHCwa, AHCpn)

pers_table2 <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

pers_table3 <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

pers_table4 <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

pers_table5 <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  gather(nation, value, -period, -housingcosts) %>%
  mutate(type = str_c(housingcosts, nation)) %>%
  select(period, value, type) %>%
  mutate(value = percent(value, 1)) %>%
  spread(type, value) %>%
  select(period, BHCScotland, BHCEngland, BHCWales, "BHCNorthern Ireland", BHCUK,
         AHCScotland, AHCEngland, AHCWales, "AHCNorthern Ireland", AHCUK)

remove(data)
