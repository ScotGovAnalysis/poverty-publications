
# Create spreadsheet for single-year data

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

filename <- "All single year.xlsx"

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

formatted <- labels[["years"]]$formatted
unformatted <- labels[["years"]]$years
latestyear <- max(levels(formatted))

# 2. Income ----
createSepsheet(filename = filename, sheetname = "- 2 -",
               text = "2 - Income and income inequality")

## Median BHC household income by age group ------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Median income BHC",
             title = "Median income before housing costs by age group",
             subtitle = str_c("Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "People", "Children", "Working-age adults", "Pensioners"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and first worksheet
createSpreadsheet(data)

## Median AHC household income by age group ------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansahc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Median income AHC",
             title = "Median income after housing costs by age group",
             subtitle = str_c("Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "People", "Children", "Working-age adults", "Pensioners"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)


## BHC income decile points ----------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecptsbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Decile points BHC",
             title = "Before housing costs income decile points",
             subtitle = str_c("Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"),
             uberheaders = c(" " = 1, "Scottish income deciles" = 9),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## AHC income decile points ----------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecptsahc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Decile points AHC",
             title = "After housing costs income decile points",
             subtitle = str_c("Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"),
             uberheaders = c(" " = 1, "Scottish income deciles" = 9),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)


## BHC income decile shares ----------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecsharesbhc)) %>%
  addyearvar() %>%
  mutate_if(~ any(is.numeric(.)), ~round2(. / 1000000, n = 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))


# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Decile shares BHC",
             title = "Before housing costs income decile shares",
             subtitle = str_c("Annual household income shares in £ million (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"),
             uberheaders = c(" " = 1, "Scottish income deciles" = 10),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## AHC income decile shares ----------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecsharesahc)) %>%
  addyearvar() %>%
  mutate_if(~ any(is.numeric(.)), ~round2(. / 1000000, n = 0)) %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Decile shares AHC",
             title = "After housing costs income decile shares",
             subtitle = str_c("Annual household income shares in £ million (in ", latestyear, " prices), Scotland"),
             headers = c("Year", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"),
             uberheaders = c(" " = 1, "Scottish income deciles" = 10),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## BHC Palma -------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getpalmabhc)) %>%
  addyearvar() %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted),
         Palma_rate = round2(Palma, 2)) %>%
  select(-Palma)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Palma BHC",
             title = "Palma ratio of income inequality before housing costs",
             subtitle = "Income share of the top 10% divided by the the bottom 40% of the household population, Scotland",
             headers = c("Year", "Palma ratio"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## AHC Palma -------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getpalmaahc)) %>%
  addyearvar() %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted),
         Palma_rate = round2(Palma, 2)) %>%
  select(-Palma)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Palma AHC",
             title = "Palma ratio of income inequality after housing costs",
             subtitle = "Income share of the top 10% divided by the the bottom 40% of the household population, Scotland",
             headers = c("Year", "Palma ratio"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## BHC Gini --------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getginibhc)) %>%
  addyearvar() %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted),
         Gini_rate = round2(Gini, 2)) %>%
  select(-Gini)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Gini BHC",
             title = "Gini coefficient of income inequality before housing costs",
             subtitle = "Scotland",
             headers = c("Year", "Gini coefficient"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

## AHC Gini --------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getginiahc)) %>%
  addyearvar() %>%
  mutate(years = factor(years, levels = unformatted, labels = formatted),
         Gini_rate = round2(Gini, 2)) %>%
  select(-Gini)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Gini AHC",
             title = "Gini coefficient of income inequality after housing costs",
             subtitle = "Scotland",
             headers = c("Year", "Gini coefficient"),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)


## BHC poverty thresholds ------------------------------------------------------

latesthbai <- hbai[[length(unformatted)]]

df <- getpovertythresholdsbhc(latesthbai) %>%
  mutate_at(vars(starts_with("weekly")), ~round2(., n = 0)) %>%
  mutate_at(vars(starts_with("annual")), ~round2(., n = -2))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Poverty thresholds BHC",
             title = "Before housing costs income thresholds for different household types",
             subtitle = "Income in £, after tax and transfers, Scotland 2018/19",
             headers = c(" ", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual"),
             uberheaders = c(" " = 1, "Single person with no children" = 2, "Couple with no children" = 2,
                             "Single person with children aged 5 and 14" = 2, "Couple with children aged 5 and 14" = 2),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

# Fix col widths
wb <- loadWorkbook(str_c("output/", filename))
setColWidths(wb, data$sheetname, cols = 2, widths = 70)
setColWidths(wb, data$sheetname, cols = 3:10, widths = 10)
saveWorkbook(wb, str_c("output/", filename), overwrite = TRUE)


## AHC poverty thresholds ------------------------------------------------------

latesthbai <- hbai[[length(unformatted)]]

df <- getpovertythresholdsahc(latesthbai) %>%
  mutate_at(vars(starts_with("weekly")), ~round2(., n = 0)) %>%
  mutate_at(vars(starts_with("annual")), ~round2(., n = -2))

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Poverty thresholds AHC",
             title = "After housing costs income thresholds for different household types",
             subtitle = "Income in £, after tax and transfers, Scotland 2018/19",
             headers = c(" ", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual"),
             uberheaders = c(" " = 1, "Single person with no children" = 2, "Couple with no children" = 2,
                             "Single person with children aged 5 and 14" = 2, "Couple with children aged 5 and 14" = 2),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

# Fix col widths
wb <- loadWorkbook(str_c("output/", filename))
setColWidths(wb, data$sheetname, cols = 2, widths = 70)
setColWidths(wb, data$sheetname, cols = 3:10, widths = 10)
saveWorkbook(wb, str_c("output/", filename), overwrite = TRUE)

# TOC ----

createContentSheet(paste0("output/", filename))

rm(list = ls())

