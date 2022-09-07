
# Create tables to include on website and as Excel files for download
library(tidyverse)
library(scales)

source("R/00_functions.R")

persistent <- readRDS("data/persistentpoverty.rds")

data <- persistent %>%
  filter(group %in% c("ch", "wa", "pn", "pp"),
         nation == "Scotland") %>%
  mutate_if(is.numeric, ~round2(., 2)) %>%
  mutate(group = factor(group, levels = c("pp", "ch", "wa", "pn")),
         value = ifelse(housingcosts == "sample", comma(value, 1),
                        percent(value, 1)),
         housingcosts = case_when(housingcosts == "sample" ~ "Sample",
                                  housingcosts == "AHC" ~ "After housing costs",
                                  housingcosts == "BHC" ~ "Before housing costs")) %>%
  rename(Period = period) %>%
  select(-nation)

persistenttables <- list()

persistenttables$source <- "Source: Understanding Society Survey"

# Scotland table ---------------------------------------------------------------

persistenttables$tableScotland <- data %>%
  filter(housingcosts == "After housing costs") %>%
  select(Period, value, group) %>%
  spread(group, value)

# table 1 ----------------------------------------------------------------------
persistenttables$table1 <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  spread(housingcosts, value)

# table 2 ----------------------------------------------------------------------
persistenttables$table2 <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  spread(housingcosts, value)

# table 3 ----------------------------------------------------------------------
persistenttables$table3 <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  spread(housingcosts, value)

# table 4 ----------------------------------------------------------------------
persistenttables$table4 <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  spread(housingcosts, value)

# Entry / exit -----------------------------------------------------------------
data2 <- persistent %>%
  filter(group %in% c("entry", "exit", "entry_sample", "exit_sample"),
         nation == "Scotland") %>%
  mutate_if(is.numeric, ~round2(., 2)) %>%
  mutate(measure = ifelse(group %in% c("entry_sample", "exit_sample"), "Sample", housingcosts),
         measure = case_when(measure == "Sample" ~ paste("Sample", housingcosts),
                             housingcosts == "AHC" ~ "After housing costs (AHC)",
                             housingcosts == "BHC" ~ "Before housing costs (BHC)"),
         value = ifelse(group %in% c("entry_sample", "exit_sample"),
                        comma(value, 1), percent(value, 1)),
         group = case_when(group == "entry_sample" ~ "entry",
                           group == "exit_sample" ~ "exit",
                           TRUE ~ group)) %>%
  rename(Period = period) %>%
  select(-nation, -housingcosts)

# table 5 exit -----------------------------------------------------------------
persistenttables$table5 <- data2 %>%
  filter(group == "exit") %>%
  select(-group) %>%
  spread(measure, value)

# table 6 entry ----------------------------------------------------------------
persistenttables$table6 <- data2 %>%
  filter(group == "entry") %>%
  select(-group) %>%
  spread(measure, value)

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())


