# load packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)

source("R/00_functions.R")

# note: close spreadsheets before importing

# TODO ----
# - update filepaths and data ranges

# paths for headline stats and priority breakdowns
path <- "-- path redacted --"

# filenames
file_pp <- "File 2 - Publication four wave All Individuals.xlsm"
file_ch <- "File 3 - Publication four wave Children.xlsm"
file_wa <- "File 4 - Publication four wave Working Age Adults.xlsm"
file_pn <- "File 5 - Publication four wave Pensioners.xlsm"
file_pr <- "Scotland four wave Children.xlsm"

entry_exit <- "File 8 - Publication Entries and Exits.xlsm"

# relevant cell ranges
range = "B8:J25"
range_entryexit = "B8:J41"
range_priority = "B8:J31"

nations <- c("Scotland", "England", "Wales", "Northern Ireland", "UK")

periods <- c("2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018",
             "2015-2019", "2016-2020", "2017-2021")

# Import headline stats --------------------------------------------------------

pp_BHC <- read_xlsx(str_c(path, file_pp), sheet = "Table_2_2p", range = range, col_names = TRUE)
pp_AHC <- read_xlsx(str_c(path, file_pp), sheet = "Table_2_8p", range = range, col_names = TRUE)
pp_sample <- read_xlsx(str_c(path, file_pp), sheet = "Table_2_14p", range = range, col_names = TRUE)

ch_BHC <- read_xlsx(str_c(path, file_ch), sheet = "Table_3_2p", range = range, col_names = TRUE)
ch_AHC <- read_xlsx(str_c(path, file_ch), sheet = "Table_3_8p", range = range, col_names = TRUE)
ch_sample <- read_xlsx(str_c(path, file_ch), sheet = "Table_3_14p", range = range, col_names = TRUE)

wa_BHC <- read_xlsx(str_c(path, file_wa), sheet = "Table_4_2p", range = range, col_names = TRUE)
wa_AHC <- read_xlsx(str_c(path, file_wa), sheet = "Table_4_8p", range = range, col_names = TRUE)
wa_sample <- read_xlsx(str_c(path, file_wa), sheet = "Table_4_14p", range = range, col_names = TRUE)

pn_BHC <- read_xlsx(str_c(path, file_pn), sheet = "Table_5_2p", range = range, col_names = TRUE)
pn_AHC <- read_xlsx(str_c(path, file_pn), sheet = "Table_5_8p", range = range, col_names = TRUE)
pn_sample <- read_xlsx(str_c(path, file_pn), sheet = "Table_5_14p", range = range, col_names = TRUE)

# Import entry / exit stats ----------------------------------------------------
entry_BHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_4a", range = range_entryexit, col_names = TRUE)
exit_BHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_4b", range = range_entryexit, col_names = TRUE)
entry_AHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_12a", range = range_entryexit, col_names = TRUE)
exit_AHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_12b", range = range_entryexit, col_names = TRUE)

# Combine ----------------------------------------------------------------------
all <- do.call("rbind",
               list(getpersistentpoverty(pp_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "pp",
                             value = value/100),
                    getpersistentpoverty(pp_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "pp",
                             value = value/100),
                    getpersistentpoverty(pp_sample) %>%
                      mutate(housingcosts = "sample",
                             group = "pp"),
                    getpersistentpoverty(ch_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "ch",
                             value = value/100),
                    getpersistentpoverty(ch_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "ch",
                             value = value/100),
                    getpersistentpoverty(ch_sample) %>%
                      mutate(housingcosts = "sample",
                             group = "ch"),
                    getpersistentpoverty(wa_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "wa",
                             value = value/100),
                    getpersistentpoverty(wa_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "wa",
                             value = value/100),
                    getpersistentpoverty(wa_sample) %>%
                      mutate(housingcosts = "sample",
                             group = "wa"),
                    getpersistentpoverty(pn_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "pn",
                             value = value/100),
                    getpersistentpoverty(pn_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "pn",
                             value = value/100),
                    getpersistentpoverty(pn_sample) %>%
                      mutate(housingcosts = "sample",
                             group = "pn"),
                    get_entry_exits(entry_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = ifelse(value > 100, "entry_sample", "entry"),
                             value = ifelse(value > 100, value, value/100)),
                    get_entry_exits(exit_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = ifelse(value > 100, "exit_sample", "exit"),
                             value = ifelse(value > 100, value, value/100)),
                    get_entry_exits(entry_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = ifelse(value > 100, "entry_sample", "entry"),
                             value = ifelse(value > 100, value, value/100)),
                    get_entry_exits(exit_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = ifelse(value > 100, "exit_sample", "exit"),
                             value = ifelse(value > 100, value, value/100))   )) %>%

  mutate(period = factor(period,
                         ordered = TRUE))

# Import priority group breakdowns ---------------------------------------------
priority1 <- read_xlsx(str_c(path, file_pr), sheet = "Table_3_7p",
                      range = range_priority, col_names = TRUE)
priority2 <- read_xlsx(str_c(path, file_pr), sheet = "Table_3_9p",
                       range = range_priority, col_names = TRUE)

priority1_sample <- read_xlsx(str_c(path, file_pr), sheet = "Table_3_13p",
                      range = range_priority, col_names = TRUE)
priority2_sample <- read_xlsx(str_c(path, file_pr), sheet = "Table_3_15p",
                              range = range_priority, col_names = TRUE)

names(priority1) <- c("Group", periods)
names(priority2) <- c("Group", periods)
names(priority1_sample) <- c("Group", periods)
names(priority2_sample) <- c("Group", periods)

priority <- rbind(priority1, priority2) %>%
  filter(Group %in% c("Total", "0 - 4", "At least one disabled adult",
                      "Yes, mother under 25", "Lone parent family",
                      "Mixed/ multiple ethnic groups", "Asian/ Asian British",
                      "Black/ African/ Caribbean/ Black British",
                      "Other ethnic group",
                      "Three or more children")) %>%

  # bad way of getting rid of the child age 0-4 category (and only keep the
  # youngest child aged 0-4 category)
  filter(!row_number() == 9) %>%

  distinct() %>%
  pivot_longer(cols = periods, names_to = "Period", values_to = "Rate") %>%
  mutate(Rate = ifelse(Rate == "..", NA, Rate),
         Rate = as.numeric(Rate)/100)

priority_sample <- rbind(priority1_sample, priority2_sample) %>%
  filter(Group %in% c("Total", "0 - 4", "At least one disabled adult",
                      "Yes, mother under 25", "Lone parent family",
                      "Mixed/ multiple ethnic groups", "Asian/ Asian British",
                      "Black/ African/ Caribbean/ Black British",
                      "Other ethnic group",
                      "Three or more children")) %>%

  # bad way of getting rid of the child age 0-4 category (and only keep the
  # youngest child aged 0-4 category)
  filter(!row_number() == 9) %>%

  distinct() %>%
  pivot_longer(cols = periods, names_to = "Period", values_to = "Sample") %>%
  mutate(Sample = as.numeric(Sample))

priority <- priority %>%
  left_join(priority_sample, by = c("Group", "Period"))

# Save all ---------------------------------------------------------------------
saveRDS(all, "data/persistentpoverty.rds")
saveRDS(priority, "data/persistentpriority.rds")

rm(list = ls())





