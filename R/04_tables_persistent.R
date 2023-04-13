
# Create tables to include on website and as Excel files for download

# load -------------------------------------------------------------------------
library(tidyverse)
library(scales)

source("R/00_functions.R")

persistent <- readRDS("data/persistentpoverty.rds")
priority <- readRDS("data/persistentpriority.rds") %>%
  mutate(Period = factor(Period, ordered = TRUE))

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
  pivot_wider(names_from = group, values_from = value)

# table 1 ----------------------------------------------------------------------
persistenttables$table1 <- data %>%
  filter(group == "pp") %>%
  select(-group) %>%
  pivot_wider(names_from = housingcosts, values_from = value)

# table 2 ----------------------------------------------------------------------
persistenttables$table2 <- data %>%
  filter(group == "ch") %>%
  select(-group) %>%
  pivot_wider(names_from = housingcosts, values_from = value)

# table 3 ----------------------------------------------------------------------
persistenttables$table3 <- data %>%
  filter(group == "wa") %>%
  select(-group) %>%
  pivot_wider(names_from = housingcosts, values_from = value)

# table 4 ----------------------------------------------------------------------
persistenttables$table4 <- data %>%
  filter(group == "pn") %>%
  select(-group) %>%
  pivot_wider(names_from = housingcosts, values_from = value)

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
  pivot_wider(names_from = measure, values_from = value)

# table 6 entry ----------------------------------------------------------------
persistenttables$table6 <- data2 %>%
  filter(group == "entry") %>%
  select(-group) %>%
  pivot_wider(names_from = measure, values_from = value)

# table 7 priority groups ------------------------------------------------------

data1 <- priority %>%
  filter(Group %in% c("Mixed/ multiple ethnic groups", "Asian/ Asian British",
                      "Black/ African/ Caribbean/ Black British",
                      "Other ethnic group")) %>%
  group_by(Period) %>%
  summarise(Group = "Minority ethnic family",
            Rate = NA,
            Sample = sum(Sample))

data2 <- priority %>%
  filter(!Group %in% c("Mixed/ multiple ethnic groups", "Asian/ Asian British",
                       "Black/ African/ Caribbean/ Black British",
                       "Other ethnic group"))

persistenttables$table7a <- rbind(data1, data2) %>%
  mutate(Group = factor(Group,
                        levels = c("Total",
                                   "Three or more children",
                                   "At least one disabled adult",
                                   "0 - 4",
                                   "Minority ethnic family",
                                   "Lone parent family",
                                   "Yes, mother under 25"),
                        labels = c("All children",
                                   "3 or more children in the family",
                                   "Disabled adult(s) in family",
                                   "Youngest child in the family is under 5",
                                   "Minority ethnic family",
                                   "Single parent family",
                                   "Mother under 25"),
                        ordered = TRUE),
         Rate = round2(Rate, 2)) %>%
  arrange(Group) %>%
  select(-Sample) %>%
  pivot_wider(names_from = Period, values_from = Rate)

persistenttables$table7b <- rbind(data1, data2) %>%
  mutate(Group = factor(Group,
                        levels = c("Total",
                                   "Three or more children",
                                   "At least one disabled adult",
                                   "0 - 4",
                                   "Minority ethnic family",
                                   "Lone parent family",
                                   "Yes, mother under 25"),
                        labels = c("All children",
                                   "3 or more children in the family",
                                   "Disabled adult(s) in family",
                                   "Youngest child in the family is under 5",
                                   "Minority ethnic family",
                                   "Single parent family",
                                   "Mother under 25"),
                        ordered = TRUE)) %>%
  arrange(Group) %>%
  select(-Rate) %>%
  pivot_wider(names_from = Period, values_from = Sample)


# save all ---------------------------------------------------------------------

saveRDS(persistenttables, "data/persistenttables.rds")
rm(list = ls())

cat("Persistent poverty tables created", fill = TRUE)

