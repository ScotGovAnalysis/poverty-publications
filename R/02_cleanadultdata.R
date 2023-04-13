
# Clean datasets: address variable changes, combine into single data frame -----

library(tidyverse)
source("R/00_functions.R")
source("R/00_strings.R")

years <- unique(labels$years$years)

files_adult <- readRDS("data/files_adult.rds")
adult_clean <- vector("list", length(years))

# Variable changes -------------------------------------------------------------

# relationship matrix changed in 9697, 9798
# marital new in 9697 (before: ms)
# penflag from 1011 (before: fixed ages 60 f and 65 m)
# religsc from 1112 - not needed before
# sidqn from 1112
# corign from 0809 (cat change in 1314)
# corigoth from 1213
# discora1 from 1213
# intxcred from 0001

# 9495 to 9596 -----------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corign = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           r11 = NA,
           r12 = NA,
           r13 = NA,
           r14 = NA,
           marital = ms,
           hdage = NA,
           year = year,
           hrpid = NA,
           seincam2 = incse1,
           intxcred = NA) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1,
           empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

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
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corign = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           r11 = NA,
           r12 = NA,
           r13 = NA,
           r14 = NA,
           year = year,
           hrpid = NA,
           intxcred = NA) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 9798 to 9899 -----------------------------------------------------------------
for (year in years[4:5]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corign = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           year = year,
           hrpid = NA,
           intxcred = NA) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 9900 -------------------------------------------------------------------------
for (year in years[6]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corign = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           year = year,
           intxcred = NA) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 0001 to 0709 -----------------------------------------------------------------

for (year in years[7:14]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corign = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           year = year) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 0809 to 0910 -----------------------------------------------------------------
for (year in years[15:16]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           sidqn = NA,
           corigoth = NA,
           penflag = ifelse(sex == 2 & age >= 60, 1,
                            ifelse(sex == 1 & age >= 65, 1, 2)),
           year = year) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 1011 -------------------------------------------------------------------------
for (year in years[17]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           religsc = NA,
           corigoth = NA,
           sidqn = NA,
           year = year) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

}

# 1112 -------------------------------------------------------------------------
for (year in years[18:18]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(discora1 = NA,
           year = year,
           corigoth = NA) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

  adult_clean[[year]] <- nextdataset

  remove(nextdataset)
}

# 1213 to latest year ----------------------------------------------------------
for (year in years[19:length(years)]) {

  nextdataset <- files_adult[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    select(year, sernum, benunit, person, hrpid,
           age, hdage, sex, sidqn, marital, corign, corigoth, discora1, empstatc, empstati, penflag, religsc,
           r01, r02, r03, r04, r05, r06, r07,
           r08, r09, r10, r11, r12, r13, r14,
           tothours, indinc, inearns, seincam2, ininv, inrpinc, inpeninc, indisben, inothben, intxcred)

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

cat("Adult dataset cleaned and saved", fill = TRUE)
