
# Clean datasets: variable changes, cobine into single data frame --------------

library(tidyverse)
source("R/00_strings.R")

years <- unique(labels$years$years)

files_househol <- readRDS("data/files_househol.rds")
househol_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------
# urinds from 0405
# foodqs from 1920
# imds from 1920
# council areas changes (merges etc.) in 1996/97
# no laua from 2021
# sstrtreg from 9798

# 9495 to 9697 -----------------------------------------------------------------
for (year in years[1:3]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(urinds = NA,
           imds = NA,
           imde = NA,
           imdw = NA,
           imdn = NA,
           sstrtreg = NA,
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
           foodq8c = NA,
           laua = NA,
           year = year,
           gvtregn = NA) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
}

# 9798 to 9899 -----------------------------------------------------------------
for (year in years[4:5]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(urinds = NA,
           imds = NA,
           imde = NA,
           imdw = NA,
           imdn = NA,
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
           foodq8c = NA,
           laua = NA,
           year = year,
           gvtregn = NA) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
}

# 9900 to 0304 -----------------------------------------------------------------
for (year in years[6:10]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(urinds = NA,
           imds = NA,
           imde = NA,
           imdw = NA,
           imdn = NA,
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
           foodq8c = NA,
           laua = NA,
           year = year) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
}

# 0405 to 1213 -----------------------------------------------------------------
for (year in years[11:18]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(imds = NA,
           imde = NA,
           imdw = NA,
           imdn = NA,
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
           foodq8c = NA,
           year = year,
           laua = NA) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
}

# 1314 to 1819 -----------------------------------------------------------------
for (year in years[19:25]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(imds = NA,
           imde = NA,
           imdw = NA,
           imdn = NA,
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
           foodq8c = NA,
           year = year,
           gvtregn = gvtregno,
           laua = NA) %>%
    select(year, sernum, gvtregn, sstrtreg, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
}

# 1920 ----------------------------------------------------------
for (year in years[26:26]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year,
           gvtregn = gvtregno) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
  remove(nextdataset)
}

# 2021 to latest year ----------------------------------------------------------
for (year in years[27:length(years)]) {

  nextdataset <- files_househol[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year,
           lac = NA,
           gvtregn = gvtregno) %>%
    select(year, sernum, sstrtreg, gvtregn, urinds,
           imds, imdn, imde, imdw,
           hhstat, foodq1, foodq2, foodq3, foodq4a, foodq4b, foodq4c, foodq5,
           foodq6, foodq7,foodq8a, foodq8b, foodq8c,
           laua, lac, ctband)

  househol_clean[[year]] <- nextdataset
  remove(nextdataset)
}

househol_clean <- do.call(rbind, househol_clean)

# remove some attributes to avoid warnings -------------------------------------
attr(househol_clean$sernum, "format.sas") <- NULL
attr(househol_clean$sernum, "label") <- NULL

saveRDS(househol_clean, "data/househol_clean.rds")
rm(list = ls())

cat("Househol dataset cleaned and saved", fill = TRUE)
