
# Create spreadsheet for income analysis

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

filename <- "Income single year.xlsx"

latestyear <- as.character(labels[["years"]]$formatted[length(labels[["years"]]$formatted)])

# Median BHC household income by age group ---------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansbhc)) %>% 
  addyearvar() %>% 
  fmtweeklyGBP %>%
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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

# Median AHC household income by age group ---------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansahc)) %>% 
  addyearvar() %>% 
  fmtweeklyGBP %>%
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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


# BHC income decile points --------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecptsbhc)) %>% 
  addyearvar() %>% 
  fmtweeklyGBP %>%
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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

# AHC income decile points --------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecptsahc)) %>% 
  addyearvar() %>% 
  fmtweeklyGBP %>%
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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


# BHC income decile shares -------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecsharesbhc)) %>% 
  addyearvar() %>% 
  mutate_if(is.numeric, comma_format(scale = 1E-6)) %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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

# AHC income decile shares -------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getdecsharesahc)) %>% 
  addyearvar() %>% 
  mutate_if(is.numeric, comma_format(scale = 1E-6)) %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted))

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

# BHC Palma -----------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getpalmabhc)) %>% 
  addyearvar() %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted),
         Palma = percent(Palma, 1))

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

# AHC Palma -----------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getpalmaahc)) %>% 
  addyearvar() %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted),
         Palma = percent(Palma, 1))

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

# BHC Gini -------------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getginibhc)) %>% 
  addyearvar() %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted),
         Gini = percent(Gini, 1))

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

# AHC Gini -------------------------------------------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getginiahc)) %>% 
  addyearvar() %>% 
  mutate(years = factor(years, labels = labels[["years"]]$formatted),
         Gini = percent(Gini, 1))

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


# BHC poverty thresholds -----------------------------------------------------------

latesthbai <- hbai[[length(labels$years[[1]])]]

df <- getpovertythresholdsbhc(latesthbai)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Poverty thresholds BHC",
             title = "Before housing costs income thresholds for different household types",
             subtitle = "Income in £, after tax and transfers, and in 2018/19 prices, Scotland",
             headers = c(" ", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual"),
             uberheaders = c(" " = 1, "Single person with no children" = 2, "Couple with no children" = 2,
                             "Single person with children aged 5 and 14" = 2, "Couple with children aged 5 and 14" = 2),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

# AHC poverty thresholds -----------------------------------------------------------

latesthbai <- hbai[[length(labels$years[[1]])]]

df <- getpovertythresholdsahc(latesthbai)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Poverty thresholds AHC",
             title = "After housing costs income thresholds for different household types",
             subtitle = "Income in £, after tax and transfers, and in 2018/19 prices, Scotland",
             headers = c(" ", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual", "Weekly", "Annual"),
             uberheaders = c(" " = 1, "Single person with no children" = 2, "Couple with no children" = 2,
                             "Single person with children aged 5 and 14" = 2, "Couple with children aged 5 and 14" = 2),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createSpreadsheet(data)

