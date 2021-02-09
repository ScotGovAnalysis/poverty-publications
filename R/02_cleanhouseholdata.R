
# Clean datasets to reduce size and address variable name changes

source("R/00_strings.R")
source("R/00_functions.R")

years <- labels[["years"]]$years

househol_clean <- vector("list", length(years))
names(househol_clean) <- years

# Variable changes
# urinds from 0405
# foodqs from 1920
# imds from 1920

# 9495 to 0304 ----------------------------------------------------------------------------------------
for (year in years[1:10]) {

  nextdataset <- readRDS("data/files_househol.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(urinds = NA,
           imds = NA,
           foodq1 = NA,
           foodq2 = NA,
           foodq3 = NA,
           foodq4a = NA,
           foodq4b = NA,
           foodq4c = NA,
           foodq5 = NA,
           foodq6 = NA,
           foodq7 = NA,
           foodq8a = NA,
           foodq8b = NA,
           foodq8c = NA) %>%
    select(sernum, urinds, imds, foodq1, foodq2, foodq3, foodq4a, foodq4b,
           foodq4c, foodq5, foodq6, foodq7, foodq8a, foodq8b, foodq8c)

  househol_clean[[year]] <- nextdataset

}

# 0405 to 1819 -----------------------------------------------------------------
for (year in years[11:25]) {

  nextdataset <- readRDS("data/files_househol.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(imds = NA,
           foodq1 = NA,
           foodq2 = NA,
           foodq3 = NA,
           foodq4a = NA,
           foodq4b = NA,
           foodq4c = NA,
           foodq5 = NA,
           foodq6 = NA,
           foodq7 = NA,
           foodq8a = NA,
           foodq8b = NA,
           foodq8c = NA) %>%
    select(sernum, urinds, imds, foodq1, foodq2, foodq3, foodq4a, foodq4b,
           foodq4c, foodq5, foodq6, foodq7, foodq8a, foodq8b, foodq8c)

  househol_clean[[year]] <- nextdataset

}

# 1920 to latest year ----------------------------------------------------------
for (year in years[26:length(years)]) {

  nextdataset <- readRDS("data/files_househol.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    select(sernum, urinds, imds, foodq1, foodq2, foodq3, foodq4a, foodq4b,
           foodq4c, foodq5, foodq6, foodq7, foodq8a, foodq8b, foodq8c)

  househol_clean[[year]] <- nextdataset

}

househol_clean <- do.call(rbind, househol_clean)

# remove some attributes to avoid warnings
attr(househol_clean$sernum, "format.sas") <- NULL
attr(househol_clean$sernum, "label") <- NULL

saveRDS(househol_clean, "data/househol_clean.rds")
rm(list = ls())
