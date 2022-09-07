# load packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)

source("R/00_functions.R")

# import Excel files -----------------------------------------------------------
# update filepath and data ranges

path <- " -- path redacted -- "

file_pp <- "Publication four wave All Individuals.xlsm"
file_ch <- "Publication four wave Children.xlsm"
file_wa <- "Publication four wave Working Age Adults.xlsm"
file_pn <- "Publication four wave Pensioners.xlsm"

entry_exit <- "Publication Entries and Exits.xlsm"

range = "B8:I25"

nations <- c("Scotland", "England", "Wales", "Northern Ireland", "UK")

periods <- c("2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018",
             "2015-2019", "2016-2020")

# Import data

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

range = "B8:I41"

entry_BHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_4a", range = range, col_names = TRUE)
exit_BHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_4b", range = range, col_names = TRUE)
entry_AHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_12a", range = range, col_names = TRUE)
exit_AHC <- read_xlsx(str_c(path, entry_exit), sheet = "Table_8_12b", range = range, col_names = TRUE)

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

saveRDS(all, "data/persistentpoverty.rds")

rm(list = ls())





