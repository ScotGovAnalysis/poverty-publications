
# Clean datasets to reduce size and address variable name changes

source("R/00_strings.R")
source("R/00_functions.R")

years <- labels[["years"]]$years


child_clean <- vector("list", length(years))
names(child_clean) <- years

# Variable changes


# 9495 to latest year ----------------------------------------------------------
for (year in years[1:length(years)]) {

  nextdataset <- readRDS("data/files_child.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    select(sernum, benunit, age)

  child_clean[[year]] <- nextdataset

}

child_clean <- do.call(rbind, child_clean)

# remove some attributes to avoid warnings
attr(child_clean$sernum, "format.sas") <- NULL
attr(child_clean$sernum, "label") <- NULL

saveRDS(child_clean, "data/child_clean.rds")
rm(list = ls())
