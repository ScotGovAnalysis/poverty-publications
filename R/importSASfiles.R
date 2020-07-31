

library(haven)
library(stringr)

source("R/strings.R")

# Import datasets from SAS libraries

path_adult <- "xx/FRS.ADULT_"
path_child <- "xx/FRS.CHILD_"
path_househol <- "xx/FRS.HOUSEHOL_"
path_hbamast <- "xx/HBAMAST.HBAI"

filenames_hbai <- str_c(path_hbamast, years)
filenames_adult <- str_c(path_adult, years)
filenames_child <- str_c(path_child, years)
filenames_househol <- str_c(path_househol, years)

files_hbai <- lapply(filenames_hbai, read_sas)
files_adult <- lapply(filenames_adult, read_sas)
files_child <- lapply(filenames_child, read_sas)
files_househol <- lapply(filenames_househol, read_sas)

# Save in RDS format

saveRDS(files_hbai, "files_hbai")
saveRDS(files_adult, "files_adult")
saveRDS(files_child, "files_child")
saveRDS(files_househol, "files_househol")





