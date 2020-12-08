
# Clean datasets to reduce size and address variable name changes

source("R/00_strings.R")
source("R/00_functions.R")

years <- labels[["years"]]$years

adult_clean <- vector("list", length(years))
names(adult_clean) <- years

# Variable changes

# relationship matrix changed in 9697, 9798
# marital new in 9697 (before: ms)
# penflag from 1011 (before: fixed ages 60 f and 65 m)
# religsc from 1112 - not needed before

# 9495 to 9596 ---------------------------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- readRDS("data/files_adult.rds")[[year]]

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
           hdage = NA) %>%
    select(sernum, benunit,
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

# 9697 ------------------------------------------------------------------------------------------
for (year in years[3]){

  nextdataset <- readRDS("data/files_adult.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           r11 = NA,
           r12 = NA,
           r13 = NA,
           r14 = NA) %>%
    select(sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 9798 to 0910 ---------------------------------------------------------------------------------
for (year in years[4:16]) {

  nextdataset <- readRDS("data/files_adult.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2))) %>%
    select(sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 1011 ------------------------------------------------------------------------------------------
for (year in years[17]) {

  nextdataset <- readRDS("data/files_adult.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(religsc = NA) %>%
    select(sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# 1112 to latest year --------------------------------------------------------------------------
for (year in years[18:length(years)]) {

  nextdataset <- readRDS("data/files_adult.rds")[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    select(sernum, benunit,
           age, hdage, sex, marital, empstatc, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14)

  adult_clean[[year]] <- nextdataset

}

# Last: Add year variable and also year in comment attribute to each dataset --------------------------
for (year in years) {
  adult_clean[[year]]$year <- year
  attr(adult_clean[[year]], "comment") <- year
}

saveRDS(adult_clean, "data/adult_clean.rds")
rm(list = ls())
