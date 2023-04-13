
# Clean datasets: select required variables, combine into single data frame ----

library(tidyverse)
source("R/00_strings.R")

years <- unique(labels$years$years)

files_child <- readRDS("data/files_child.rds")
child_clean <- vector("list", length(years))

# 9495 to latest year ----------------------------------------------------------
for (year in years[1:length(years)]) {

  nextdataset <- files_child[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(year = year) %>%
    select(year, sernum, benunit, age)

  child_clean[[year]] <- nextdataset
  remove(nextdataset)
}

child_clean <- do.call(rbind, child_clean)

# remove some attributes to avoid warnings -------------------------------------
attr(child_clean$sernum, "format.sas") <- NULL
attr(child_clean$sernum, "label") <- NULL
attr(child_clean$benunit, "format.sas") <- NULL
attr(child_clean$benunit, "label") <- NULL

saveRDS(child_clean, "data/child_clean.rds")
rm(list = ls())

cat("Child dataset cleaned and saved", fill = TRUE)
