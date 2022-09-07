
# Clean dataset: variable name changes, combine into single data frame ---------

library(tidyverse)

years = c(        "0506", "0607", "0708", "0809",
          "0910", "1011", "1112", "1213", "1314",
          "1415", "1516", "1617", "1718", "1819",
          "1920", "2021")

files_chldcare <- readRDS("data/files_chldcare.rds")
chldcare_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------

# From 0506 to  ------------------------------------------------------------
for (year in years[1:length(years)]) {

  nextdataset <- files_chldcare[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    select(year, sernum, benunit, chamt, cost)

  chldcare_clean[[year]] <- nextdataset
}


chldcare_clean <- do.call(rbind, chldcare_clean)

# remove some attributes to avoid warnings
attr(chldcare_clean$sernum, "format.sas") <- NULL
attr(chldcare_clean$sernum, "label") <- NULL
attr(chldcare_clean$benunit, "format.sas") <- NULL
attr(chldcare_clean$benunit, "label") <- NULL

saveRDS(chldcare_clean, "data/chldcare_clean.rds")
#rm(list = ls())
