
# Prepare minimal tidy datasets

# Load helpers and clean datasets
source("R/00_strings.R")
source("R/00_functions.R")

years <- labels[["years"]]$years

hbai <- readRDS("data/hbai_clean.rds")
househol <- readRDS("data/househol_clean.rds")
adult <- readRDS("data/adult_clean.rds")

# Get tidy HBAI dataset for most analysis ---------------------------------------

# store year in comment attribute of each data frame
for (year in years){
  df <- hbai[[year]] 
  attr(df, "comment") <- year
  hbai[[year]] <- df 
}

# get flags for poverty outcomes and BU and hhld characteristics
hbai <- lapply(hbai, geturbanrural)
hbai <- lapply(hbai, gethhworkstatus)
hbai <- lapply(hbai, gethhdisabledstatus)
hbai <- lapply(hbai, getpovertyflags)

tidyhbai <- hbai

# store year in comment attribute of each data frame (again)
for (year in years){
  df <- tidyhbai[[year]] 
  attr(df, "comment") <- year
  tidyhbai[[year]] <- df 
}

saveRDS(tidyhbai, "data/tidyhbai.rds")

# Get tidy ADULT dataset for some adult-level analysis (marital/religion) -------

# store year in comment attribute of each data frame
for (year in years){
  df <- adult[[year]] 
  attr(df, "comment") <- year
  adult[[year]] <- df 
}

# add adult weights and poverty flags
adult <- lapply(adult, addpovflagsnadultwgt)

tidyadult <- adult

# store year in comment attribute of each data frame (again)
for (year in years){
  df <- tidyadult[[year]] 
  attr(df, "comment") <- year
  tidyadult[[year]] <- df 
}

saveRDS(tidyadult, "data/tidyadult.rds")

rm(list = ls())