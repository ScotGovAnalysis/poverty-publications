
library(tidyverse)
library(haven)
source("R/00_strings.R")

years <- factor(labels[["years"]]$years,
                levels = as.character(labels[["years"]]$years),
                ordered = TRUE)

newyear <- tail(levels(years), 1L)

path_hbai <- paste0("//s0177a/sasdata2/soc_just/private/hbai", newyear,
                    ".sas7bdat")
path_adult <- paste0("//s0177a/sasdata2/soc_just/private/frs_adult_", newyear,
                    ".sas7bdat")
path_child <- paste0("//s0177a/sasdata2/soc_just/private/frs_child_", newyear,
                     ".sas7bdat")
path_benefits <- paste0("//s0177a/sasdata2/soc_just/private/frs_benefits_",
                        newyear, ".sas7bdat")
path_househol <- paste0("//s0177a/sasdata2/soc_just/private/frs_househol_",
                        newyear, ".sas7bdat")

# Get latest datasets
files_hbai_new <- read_sas(path_hbai)
files_adult_new <- read_sas(path_adult)
files_child_new <- read_sas(path_child)
files_househol_new <- read_sas(path_househol)
files_benefits_new <- read_sas(path_benefits)
inflationindex <- read_sas("//s0177a/sasdata2/soc_just/private/inflation_index.sas7bdat")

# Read in existing dataset lists
files_hbai <- readRDS("data/files_hbai.rds")
files_adult <- readRDS("data/files_adult.rds")
files_child <- readRDS("data/files_child.rds")
files_househol <- readRDS("data/files_househol.rds")
files_benefits <- readRDS("data/files_benefits.rds")

# Add to existing dataset lists
files_hbai[[length(years)]] <- files_hbai_new
files_adult[[length(years)]] <- files_adult_new
files_child[[length(years)]] <- files_child_new
files_benefits[[length(years)]] <- files_benefits_new
files_househol[[length(years)]] <- files_househol_new

# Name new list items
names(files_hbai)[[length(years)]] <- newyear
names(files_adult)[[length(years)]] <- newyear
names(files_child)[[length(years)]] <- newyear
names(files_househol)[[length(years)]] <- newyear
names(files_benefits)[[length(years)]] <- newyear

# Update inflators
inflationindex$years <- years

inflationindex <- inflationindex %>%
  mutate(now_bhc = ifelse(years == years[length(years)], inflation_bhc, 0),
         now_ahc = ifelse(years == years[length(years)], inflation_ahc, 0),
         now_bhc = max(now_bhc),
         now_ahc = max(now_ahc),
         infl_bhc = now_bhc/inflation_bhc,
         infl_ahc = now_ahc/inflation_ahc) %>%
  select(years, infl_bhc, infl_ahc)

# Save in RDS format
saveRDS(files_hbai, "data/files_hbai.rds")
saveRDS(files_adult, "data/files_adult.rds")
saveRDS(files_child, "data/files_child.rds")
saveRDS(files_househol, "data/files_househol.rds")
saveRDS(files_benefits, "data/files_benefits.rds")
saveRDS(inflationindex, "data/inflationindex.rds")

# Clear workspace

rm(list = ls())
