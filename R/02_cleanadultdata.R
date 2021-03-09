
# Clean datasets: address variable changes, combine into single data frame -----

library(tidyverse)
source("R/00_functions.R")

years <- c("9495", "9596", "9697", "9798", "9899",
           "9900", "0001", "0102", "0203", "0304",
           "0405", "0506", "0607", "0708", "0809",
           "0910", "1011", "1112", "1213", "1314",
           "1415", "1516", "1617", "1718", "1819",
           "1920")

files_adult <- readRDS("data/files_adult.rds")
adult_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------

# relationship matrix changed in 9697, 9798
# marital new in 9697 (before: ms)
# penflag from 1011 (before: fixed ages 60 f and 65 m)
# religsc from 1112 - not needed before

# 9495 to 9596 -----------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           r11 = NA,
           r12 = NA,
           r13 = NA,
           r14 = NA,
           marital = ms,
           hdage = NA,
           year = year) %>%
    select(year, sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  # recode relationship matrix
  for (i in c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09", "r10")) {
  nextdataset[[i]] <- decode(nextdataset[[i]],
                            search =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            replace = c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  }

  # recode marital status
  nextdataset$marital <- decode(nextdataset$marital,
                             search =  c(1, 2, 3, 4, 5, 6, 7),
                             replace = c(1, 1, 2, 3, 4, 5, 6))

  adult_clean[[year]] <- nextdataset

}

# 9697 -------------------------------------------------------------------------
for (year in years[3]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           r11 = NA,
           r12 = NA,
           r13 = NA,
           r14 = NA,
           year = year) %>%
    select(year, sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 9798 to 0910 -----------------------------------------------------------------
for (year in years[4:16]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           year = year) %>%
    select(year, sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 1011 -------------------------------------------------------------------------
for (year in years[17]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           year = year) %>%
    select(year, sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 1112 to latest year ----------------------------------------------------------
for (year in years[18:length(years)]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    select(year, sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

  remove(nextdataset)
}

adult_clean <- do.call(rbind, adult_clean)

# remove some attributes to avoid warnings -------------------------------------
attr(adult_clean$sernum, "format.sas") <- NULL
attr(adult_clean$sernum, "label") <- NULL
attr(adult_clean$benunit, "format.sas") <- NULL
attr(adult_clean$benunit, "label") <- NULL

# Save -------------------------------------------------------------------------
saveRDS(adult_clean, "data/adult_clean.rds")
rm(list = ls())
