
# Clean dataset: variable name changes, combine into single data frame ---------

library(tidyverse)
source("R/00_strings.R")

years <- unique(labels$years$years)

files_benefits <- readRDS("data/files_benefits.rds")
benefits_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------
# label changes in 9697 (12 = AA, previously: 11 = AA)

# From 9495 to 9596 ------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- files_benefits[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    filter(benefit %in% c(1, 2, 11)) %>%
    select(sernum, benamt) %>%
    group_by(sernum) %>%
    summarise(benamt = sum(benamt),
              year = year)

  benefits_clean[[year]] <- nextdataset
}

# From 9697 to 1213 ------------------------------------------------------------
for (year in years[3:19]) {

  nextdataset <- files_benefits[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    filter(benefit %in% c(1, 2, 12)) %>%
    select(sernum, benamt) %>%
    group_by(sernum) %>%
    summarise(benamt = sum(benamt),
              year = year)

  benefits_clean[[year]] <- nextdataset

  remove(nextdataset)

}

# From 1314 to latest year -----------------------------------------------------
for (year in years[20:length(years)]) {

  nextdataset <- files_benefits[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    filter(benefit %in% c(1, 2, 12, 96, 97)) %>%
    select(sernum, benamt) %>%
    group_by(sernum) %>%
    summarise(benamt = sum(benamt),
              year = year)

  benefits_clean[[year]] <- nextdataset

  remove(nextdataset)

}

benefits_clean <- do.call(rbind, benefits_clean)

# remove some attributes to avoid warnings
attr(benefits_clean$sernum, "format.sas") <- NULL
attr(benefits_clean$sernum, "label") <- NULL

saveRDS(benefits_clean, "data/benefits_clean.rds")
rm(list = ls())

cat("Benefits dataset cleaned and saved", fill = TRUE)
