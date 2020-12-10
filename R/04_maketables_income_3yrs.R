# Create spreadsheet for income analysis

source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

filename <- "Income three-year average.xlsx"

latestyear <- max(levels(labels[["years"]]$formatted))


# Median BHC household income by age group ---------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted)) %>%
  fmtweeklyGBP

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted)) %>%
  fmtweeklyGBP

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted)) %>%
  fmtweeklyGBP

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted)) %>%
  fmtweeklyGBP

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate_if(is.numeric, comma_format(scale = 1E-6)) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted))

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate_if(is.numeric, comma_format(scale = 1E-6)) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted))

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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted),
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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted),
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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted),
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
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$formatted),
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

latesthbai_1 <- hbai[[length(labels$years[[1]])]]
latesthbai_2 <- hbai[[length(labels$years[[1]])-1]]
latesthbai_3 <- hbai[[length(labels$years[[1]])-2]]

df1 <- getpovertythresholdsbhc(latesthbai_1)
df2 <- getpovertythresholdsbhc(latesthbai_2)
df3 <- getpovertythresholdsbhc(latesthbai_3)

df <- data.frame(df1[1])
df$weekly1 <- (df1$weekly1 + df2$weekly1 + df3$weekly1)/3
df$annual1 <- (df1$annual1 + df2$annual1 + df3$annual1)/3
df$weekly2 <- (df1$weekly2 + df2$weekly2 + df3$weekly2)/3
df$annual2 <- (df1$annual2 + df2$annual2 + df3$annual2)/3
df$weekly3 <- (df1$weekly3 + df2$weekly3 + df3$weekly3)/3
df$annual3 <- (df1$annual3 + df2$annual3 + df3$annual3)/3
df$weekly4 <- (df1$weekly4 + df2$weekly4 + df3$weekly4)/3
df$annual4 <- (df1$annual4 + df2$annual4 + df3$annual4)/3

df <- df %>%
  mutate_at(vars(starts_with("weekly")), comma_format(1)) %>%
  mutate_at(vars(starts_with("annual")), comma_format(100))

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

# Fix col widths
wb <- loadWorkbook(str_c("output/", filename))
setColWidths(wb, data$sheetname, cols = 2, widths = 70)
setColWidths(wb, data$sheetname, cols = 3:10, widths = 10)
saveWorkbook(wb, str_c("output/", filename), overwrite = TRUE)


# AHC poverty thresholds -----------------------------------------------------------

latesthbai_1 <- hbai[[length(labels$years[[1]])]]
latesthbai_2 <- hbai[[length(labels$years[[1]])-1]]
latesthbai_3 <- hbai[[length(labels$years[[1]])-2]]

df1 <- getpovertythresholdsahc(latesthbai_1)
df2 <- getpovertythresholdsahc(latesthbai_2)
df3 <- getpovertythresholdsahc(latesthbai_3)

df <- data.frame(df1[1])
df$weekly1 <- (df1$weekly1 + df2$weekly1 + df3$weekly1)/3
df$annual1 <- (df1$annual1 + df2$annual1 + df3$annual1)/3
df$weekly2 <- (df1$weekly2 + df2$weekly2 + df3$weekly2)/3
df$annual2 <- (df1$annual2 + df2$annual2 + df3$annual2)/3
df$weekly3 <- (df1$weekly3 + df2$weekly3 + df3$weekly3)/3
df$annual3 <- (df1$annual3 + df2$annual3 + df3$annual3)/3
df$weekly4 <- (df1$weekly4 + df2$weekly4 + df3$weekly4)/3
df$annual4 <- (df1$annual4 + df2$annual4 + df3$annual4)/3

df <- df %>%
  mutate_at(vars(starts_with("weekly")), comma_format(1)) %>%
  mutate_at(vars(starts_with("annual")), comma_format(100))

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

# Fix col widths
wb <- loadWorkbook(str_c("output/", filename))
setColWidths(wb, data$sheetname, cols = 2, widths = 70)
setColWidths(wb, data$sheetname, cols = 3:10, widths = 10)
saveWorkbook(wb, str_c("output/", filename), overwrite = TRUE)

# Income sources (BHC only) by income decile ---------------------------------------

df1 <- getsources(hbai[[length(labels$years[[1]])]])
df2 <- getsources(hbai[[length(labels$years[[1]])-1]])
df3 <- getsources(hbai[[length(labels$years[[1]])-2]])

df <- data.frame(df1[1])
df[2] <- (df1[2] + df2[2] + df3[2])/3
df[3] <- (df1[3] + df2[3] + df3[3])/3
df[4] <- (df1[4] + df2[4] + df3[4])/3
df[5] <- (df1[5] + df2[5] + df3[5])/3
df[6] <- (df1[6] + df2[6] + df3[6])/3

df <- df %>%
  mutate(decbhc = factor(decbhc)) %>%
  mutate_if(is.numeric, fmtpct)

# Put all input for the spreadsheet into a list
data <- list(df = df,
             filename = filename,
             sheetname = "Income sources",
             title = "Income sources by income decile",
             subtitle = "Gross household income by income type as a share of total income, Scotland",
             headers = c(" ", "Earnings", "Social security payments", "Occupational pensions", "Investments", "Other sources"),
             uberheaders =  NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income here is gross income before housing costs. Income deciles are based on equivalised net income before housing costs.
",
                           "2. Due to volatility of the data, single-year estimates of these statistics are not available.
"))

# Create spreadsheet and new worksheet
createSpreadsheet(data)

# TOC ------------------------------------------------------------------------------

createContentSheet(paste0("output/", filename))

rm(list = ls())
