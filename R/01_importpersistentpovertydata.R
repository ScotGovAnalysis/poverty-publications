
library(tidyverse)
library(readxl)
library(scales)

source("R/00_functions.R")

# Import Excel files - update filepath and data ranges

path <- "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Persistent Poverty in Scotland 2021 publication/Data/QA/"

file_pp <- "File 2 - Publication All Individuals.xlsm"
file_ch <- "File 3 - Publication Children.xlsm"
file_wa <- "File 4 - Publication Working Age Adults.xlsm"
file_pn <- "File 5 - Publication Pensioners.xlsm"

range = "B8:H25"

nations <- c("Scotland", "England", "Wales", "Northern Ireland", "UK")

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
                             group = "pn")      )) %>%
  mutate(period = factor(period,
                         levels = c("2010-2011\r\nto\r\n2013-2014",
                                    "2011-2012\r\nto\r\n2014-2015",
                                    "2012-2013\r\nto\r\n2015-2016",
                                    "2013-2014\r\nto\r\n2016-2017",
                                    "2014-2015\r\nto\r\n2017-2018",
                                    "2015-2016\r\nto\r\n2018-2019"),
                         labels = c("2010-2014", "2011-2015", "2012-2016",
                                    "2013-2017", "2014-2018", "2015-2019"),
                         ordered = TRUE))

saveRDS(all, "data/persistentpoverty.rds")

rm(list = ls())





