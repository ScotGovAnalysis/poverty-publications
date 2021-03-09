
# Clean dataset: variable name changes, combine into single data frame ---------

library(tidyverse)

years = c("9495", "9596", "9697", "9798", "9899",
          "9900", "0001", "0102", "0203", "0304",
          "0405", "0506", "0607", "0708", "0809",
          "0910", "1011", "1112", "1213", "1314",
          "1415", "1516", "1617", "1718", "1819",
          "1920")

files_benefits <- readRDS("data/files_benefits.rds")
benefits_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------
# label changes in 9697

# From 9495 to 9697 ------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- files_benefits[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(benefit = ifelse(benefit == 12, 13,
                            ifelse(benefit == 11, 12, benefit))) %>%
    filter(benefit %in% c(1, 2, 12, 96, 97)) %>%
    select(sernum, benamt) %>%
    group_by(sernum) %>%
    summarise(benamt = sum(benamt),
              year = year)

  benefits_clean[[year]] <- nextdataset
}

# From 9697 to latest year -----------------------------------------------------
for (year in years[3:length(years)]) {

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
