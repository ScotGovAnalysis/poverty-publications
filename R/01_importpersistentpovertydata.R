
library(tidyverse)
library(readxl)
library(scales)

source("R/00_functions.R")

# Import Excel files - update filepath and data ranges

path <- "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Persistent Poverty in Scotland 2020 publication/Data/"

file_pp <- "File 2 - Publication All Individuals.xlsm"
file_ch <- "File 3 - Publication Children.xlsm"
file_wa <- "File 4 - Publication Working Age Adults.xlsm"
file_pn <- "File 5 - Publication Pensioners.xlsm"

range = "B8:G25"

nations <- c("Scotland", "England", "Wales", "Northern Ireland", "UK")

# Import data
  
pp_BHC <- read_xlsx(str_c(path, file_pp), sheet = "Table_2_2p", range = range, col_names = TRUE)
pp_AHC <- read_xlsx(str_c(path, file_pp), sheet = "Table_2_8p", range = range, col_names = TRUE)

ch_BHC <- read_xlsx(str_c(path, file_ch), sheet = "Table_3_2p", range = range, col_names = TRUE)
ch_AHC <- read_xlsx(str_c(path, file_ch), sheet = "Table_3_8p", range = range, col_names = TRUE)

wa_BHC <- read_xlsx(str_c(path, file_wa), sheet = "Table_4_2p", range = range, col_names = TRUE)
wa_AHC <- read_xlsx(str_c(path, file_wa), sheet = "Table_4_8p", range = range, col_names = TRUE)

pn_BHC <- read_xlsx(str_c(path, file_pn), sheet = "Table_5_2p", range = range, col_names = TRUE)
pn_AHC <- read_xlsx(str_c(path, file_pn), sheet = "Table_5_8p", range = range, col_names = TRUE)

all <- do.call("rbind", 
               list(formatpersistentpoverty(pp_BHC) %>% 
                      mutate(housingcosts = "BHC",
                             group = "pp"),
                    formatpersistentpoverty(pp_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "pp"),
                    formatpersistentpoverty(ch_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "ch"),
                    formatpersistentpoverty(ch_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "ch"),
                    formatpersistentpoverty(wa_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "wa"),
                    formatpersistentpoverty(wa_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "wa"),
                    formatpersistentpoverty(pn_BHC) %>%
                      mutate(housingcosts = "BHC",
                             group = "pn"),
                    formatpersistentpoverty(pn_AHC) %>%
                      mutate(housingcosts = "AHC",
                             group = "pn")))
               
saveRDS(all, "data/persistentpoverty.rds")

rm(list = ls())               

