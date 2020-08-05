
# Clean datasets to reduce size and address variable name changes

library(tidyverse)

source("R/00_strings.R")
source("R/00_functions.R")


#####################
#   FRS BENEFITS    #    
#####################

benefits_clean <- vector("list", length(years))
names(benefits_clean) <- years

# Variable changes
# label changes in 9697

# From 9495 to 9697
for (year in years[1:2]){
  
  nextdataset <- readRDS("data/files_benefits.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(benefit = ifelse(benefit == 12, 13,
                            ifelse(benefit == 11, 12, benefit))) %>%
    select(sernum, benefit)
  
  benefits_clean[[year]] <- nextdataset 
}

# From 9697 to latest year
for (year in years[3:length(years)]){
  
  nextdataset <- readRDS("data/files_benefits.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    select(sernum, benefit)
  
  benefits_clean[[year]] <- nextdataset 
  
}

saveRDS(benefits_clean, "data/benefits_clean.rds")
rm(list = ls())
