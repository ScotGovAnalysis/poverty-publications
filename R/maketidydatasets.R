
# Prepare minimal tidy datasets

# Load packages, helpers and clean datasets

library(tidyverse)
library(Hmisc)
source("R/00_strings.R")
source("R/00_functions.R")

hbai <- readRDS("data/hbai_clean.rds")
househol <- readRDS("data/househol_clean.rds")

  # _______________ #
  #                 #
  # Poverty dataset #
  # _______________ # 

# Flags for poverty outcomes and BU and hhld characteristics _____________

# Store year in comment attribute of each data frame
for (year in years){
  df <- hbai[[year]] 
  attr(df, "comment") <- year
  hbai[[year]] <- df 
}

hbai <- lapply(hbai, geturbanrural)
hbai <- lapply(hbai, gethhworkstatus)
hbai <- lapply(hbai, gethhdisabledstatus)
hbai <- lapply(hbai, getpovertyflags)


tidyhbai <- hbai

# Store year in comment attribute of each data frame
for (year in years){
  df <- tidyhbai[[year]] 
  attr(df, "comment") <- year
  tidyhbai[[year]] <- df 
}

saveRDS(hbai, "data/tidyhbai.rds")
rm(list = ls())











