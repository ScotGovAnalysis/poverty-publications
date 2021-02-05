
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

data <- readRDS("data/persistentpoverty.rds")

data <- data %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  mutate(group = factor(group, levels = c("pp", "ch", "wa", "pn")),
         value = ifelse(housingcosts == "sample", comma(value, 1),
                        percent(value, 1)))


persistenttables <- list()

persistenttables[["source"]] <- "Source: Understanding Society Survey"

# Scotland table ----
persistenttables[["tableScotland"]] <- data %>%
  filter(nation == "Scotland") %>%
  mutate(type = str_c(housingcosts, group)) %>%
  select(period, value, type) %>%
  spread(type, value)

# Table 1 ----
data_pp <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

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

persistenttables[["table1"]] <- rbind(dataAHC, dataBHC)

# Table 2 ----
data_ch <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

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

persistenttables[["table2"]] <- rbind(dataAHC, dataBHC)

# Table 3 ----
data_wa <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

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

persistenttables[["table3"]] <- rbind(dataAHC, dataBHC)

# Table 4 ----
data_pn <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  mutate(nation = ifelse(nation == "Total", "UK", nation))

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

persistenttables[["table4"]] <- rbind(dataAHC, dataBHC)

# Table 6 sample size
sample_S <- data %>%
  filter(housingcosts == "sample",
         nation == "Scotland") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

sample_E <- data %>%
  filter(housingcosts == "sample",
         nation == "England") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

sample_W <- data %>%
  filter(housingcosts == "sample",
         nation == "Wales") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

sample_N <- data %>%
  filter(housingcosts == "sample",
         nation == "Northern Ireland") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

sample_U <- data %>%
  filter(housingcosts == "sample",
         nation == "Total") %>%
  select(-housingcosts, -nation) %>%
  spread(group, value)

persistenttables[["table6"]] <- rbind(sample_S, sample_E, sample_W, sample_N,
                                      sample_U)

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())
