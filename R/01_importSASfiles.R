
# Careful - this code takes very long to run, only run once to import data to local machine

source("R/00_strings.R")

# Import datasets from SAS libraries

path_hbamast <- "//s0177a/sasdata1/hbai/hbai"
path_adult <- "//s0177a/sasdata2/frs/adult_"
path_child <- "//s0177a/sasdata2/frs/child_"
path_househol <- "//s0177a/sasdata2/frs/househol_"
path_benefits <- "//s0177a/sasdata2/frs/benefits_"

filenames_hbai <- str_c(path_hbamast, years, ".sas7bdat")
filenames_adult <- str_c(path_adult, years, ".sas7bdat")
filenames_child <- str_c(path_child, years, ".sas7bdat")
filenames_househol <- str_c(path_househol, years, ".sas7bdat")
filenames_benefits <- str_c(path_benefits, years, ".sas7bdat")

files_hbai <- lapply(filenames_hbai, read_sas)
files_adult <- lapply(filenames_adult, read_sas)
files_child <- lapply(filenames_child, read_sas)
files_househol <- lapply(filenames_househol, read_sas)
files_benefits <- lapply(filenames_benefits, read_sas)
inflationindex <- read_sas("//s0177a/sasdata2/soc_just/private/inflation_index.sas7bdat")

inflationindex$years <- years
inflationindex <- inflationindex %>%
  mutate(now_bhc = ifelse(years == years[length(years)], inflation_bhc, 0),
         now_ahc = ifelse(years == years[length(years)], inflation_ahc, 0),
         now_bhc = max(now_bhc),
         now_ahc = max(now_ahc),
         infl_bhc = now_bhc/inflation_bhc,
         infl_ahc = now_ahc/inflation_ahc) %>%
  select(years, infl_bhc, infl_ahc)

# Add latest dataset (might be restricted though)

# files_hbai <- c(files_hbai, read_sas("//s0177a/sasdata2/soc_just/private/hbai1920.sas7bdat"))
# files_adult <- c(files_adult, read_sas("//s0177a/sasdata2/soc_just/private/frs_adult_1920.sas7bdat"))
# files_child <- c(files_child, read_sas("//s0177a/sasdata2/soc_just/private/frs_child_1920.sas7bdat"))
# files_househol <- c(files_househol, read_sas("//s0177a/sasdata2/soc_just/private/frs_househol_1920.sas7bdat"))
# files_benefits <- c(files_benefits, read_sas("//s0177a/sasdata2/soc_just/private/frs_benefits_1920.sas7bdat"))


# Name list items

names(files_hbai) <- years
names(files_adult) <- years
names(files_child) <- years
names(files_househol) <- years
names(files_benefits) <- years

# Save in RDS format

saveRDS(files_hbai, "data/files_hbai.rds")
saveRDS(files_adult, "data/files_adult.rds")
saveRDS(files_child, "data/files_child.rds")
saveRDS(files_househol, "data/files_househol.rds")
saveRDS(files_benefits, "data/files_benefits.rds")
saveRDS(inflationindex, "data/inflationindex.rds")

# Clear workspace

rm(list = ls())



