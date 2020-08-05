
# Clean datasets to reduce size and address variable name changes

library(tidyverse)

source("R/00_strings.R")
source("R/00_functions.R")


  #####################
  #   FRS HOUSEHOL    #    
  #####################


househol_clean <- vector("list", length(years))
names(househol_clean) <- years

# Variable changes
# urinds from 0405

# From 9495 to 0304
for (year in years[1:10]){
  
  nextdataset <- readRDS("data/files_househol.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(urinds = NA) %>%
    select(sernum, urinds)
  
  househol_clean[[year]] <- nextdataset 
  
}

# From 0405 to latest year
for (year in years[11:length(years)]){
  
  nextdataset <- readRDS("data/files_househol.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    select(sernum, urinds)
  
  househol_clean[[year]] <- nextdataset 
  
}

saveRDS(househol_clean, "data/househol_clean.rds")
rm(list = ls())
