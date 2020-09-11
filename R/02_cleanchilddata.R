
# Clean datasets to reduce size and address variable name changes

source("R/00_strings.R")
source("R/00_functions.R")


##################
#   FRS CHILD    #    
##################


child_clean <- vector("list", length(years))
names(child_clean) <- years

# Variable changes


# From 9495 to latest year
for (year in years[1:length(years)]){
  
  nextdataset <- readRDS("data/files_child.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    select(sernum, benunit, age)
  
  child_clean[[year]] <- nextdataset 
  
}

saveRDS(child_clean, "data/child_clean.rds")
rm(list = ls())
