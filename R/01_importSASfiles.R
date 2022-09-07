# Careful - this code takes very long to run, only run once to import data to local machine

library(haven)

years <- c("9495", "9596", "9697", "9798", "9899",
           "9900", "0001", "0102", "0203", "0304",
           "0405", "0506", "0607", "0708", "0809",
           "0910", "1011", "1112", "1213", "1314",
           "1415", "1516", "1617", "1718", "1819",
           "1920", "2021")

# Filenames and paths to SAS libraries

path_socjust <- " -- path redacted -- "
path_frs <- " -- path redacted -- "

# Import all -------------------------------------------------------------------

# Filenames
filenames_hbai <- paste0(path_socjust, "hbai", years, ".sas7bdat")
filenames_adult <- paste0(path_socjust, "frs_adult_", years, ".sas7bdat")
filenames_child <- paste0(path_socjust, "frs_child_", years, ".sas7bdat")
filenames_chldcare <- paste0(path_frs, "chldcare_", years[12:length(years)], ".sas7bdat")
filenames_househol <- paste0(path_socjust, "frs_househol_", years, ".sas7bdat")
filenames_benefits <- paste0(path_socjust, "frs_benefits_", years, ".sas7bdat")
filenames_benunit <- paste0(path_frs, "benunit_", years, ".sas7bdat")

# Import SAS datasets (takes several hours for adult and hbai datasets)
files_hbai <- lapply(filenames_hbai, read_sas)
files_adult <- lapply(filenames_adult, read_sas)
files_child <- lapply(filenames_child, read_sas)
files_chldcare <- lapply(filenames_chldcare, read_sas)
files_househol <- lapply(filenames_househol, read_sas)
files_benefits <- lapply(filenames_benefits, read_sas)
files_benunit <- lapply(filenames_benunit, read_sas)
inflationindex <- read_sas(paste0(path_socjust, "inflation_index.sas7bdat"))

# Name list items

names(files_hbai) <- years
names(files_adult) <- years
names(files_child) <- years
names(files_househol) <- years
names(files_benefits) <- years
names(files_benunit) <- years

names(files_chldcare) <- years[12:length(years)]

# Save in RDS format

saveRDS(files_hbai, "data/files_hbai.rds")
saveRDS(files_adult, "data/files_adult.rds")
saveRDS(files_child, "data/files_child.rds")
saveRDS(files_househol, "data/files_househol.rds")
saveRDS(files_benefits, "data/files_benefits.rds")
saveRDS(files_benunit, "data/files_benunit.rds")
saveRDS(files_chldcare, "data/files_chldcare.rds")
saveRDS(inflationindex, "data/inflationindex.rds")

# Import latest ----------------------------------------------------------------
year <- years[length(years)]

# File names
filename_hbai <- paste0(path_socjust, "hbai", year, ".sas7bdat")
filename_adult <- paste0(path_socjust, "frs_adult_", year, ".sas7bdat")
filename_child <- paste0(path_socjust, "frs_child_", year, ".sas7bdat")
filename_chldcare <- paste0(path_socjust, "frs_chldcare_", year, ".sas7bdat")
filename_househol <- paste0(path_socjust, "frs_househol_", year, ".sas7bdat")
filename_benefits <- paste0(path_socjust, "frs_benefits_", year, ".sas7bdat")
filename_benunit <- paste0(path_socjust, "frs_benunit_", year, ".sas7bdat")

# Import
file_hbai <- read_sas(filename_hbai)
file_adult <- read_sas(filename_adult)
file_child <- read_sas(filename_child)
file_chldcare <- read_sas(filename_chldcare)
file_househol <- read_sas(filename_househol)
file_benefits <- read_sas(filename_benefits)
file_benunit <- read_sas(filename_benunit)
inflationindex <- read_sas(paste0(path_socjust, "inflation_index.sas7bdat"))

# Add to existing lists and save
files_hbai <- readRDS("data/files_hbai.rds")
files_hbai[[as.character(year)]] <- file_hbai
saveRDS(files_hbai, "data/files_hbai.rds")
remove(files_hbai)

files_adult <- readRDS("data/files_adult.rds")
files_adult[[as.character(year)]] <- file_adult
saveRDS(files_adult, "data/files_adult.rds")
remove(files_adult)

files_child <- readRDS("data/files_child.rds")
files_child[[as.character(year)]] <- file_child
saveRDS(files_child, "data/files_child.rds")
remove(files_child)

files_househol <- readRDS("data/files_househol.rds")
files_househol[[as.character(year)]] <- file_househol
saveRDS(files_househol, "data/files_househol.rds")
remove(files_househol)

files_benefits <- readRDS("data/files_benefits.rds")
files_benefits[[as.character(year)]] <- file_benefits
saveRDS(files_benefits, "data/files_benefits.rds")

files_benunit <- readRDS("data/files_benunit.rds")
files_benunit[[as.character(year)]] <- file_benunit
saveRDS(files_benunit, "data/files_benunit.rds")

files_chldcare <- readRDS("data/files_chldcare.rds")
files_chldcare[[as.character(year)]] <- file_chldcare
saveRDS(files_chldcare, "data/files_chldcare.rds")

saveRDS(inflationindex, "data/inflationindex.rds")

# clear workspace
rm(list = ls())



