
# Create spreadsheet for 3-year averaged data

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

filename <- "UK comparisons.xlsx"

periods <- labels[["years"]]$periods
yearsno <- length(periods)
hbai <- readRDS("data/tidyhbai.rds")

# Relative poverty BHC ----------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getpovbynation, povvar = "low60bhc")) %>% 
  addyearvar() %>%
  checkandfmtUK

pp <- df %>%
  select(years, gvtregn, pprate) %>%
  spread(years, pprate)

ch <- df %>%
  select(years, gvtregn, chrate) %>%
  spread(years, chrate)

wa <- df %>%
  select(years, gvtregn, warate) %>%
  spread(years, warate)

pn <- df %>%
  select(years, gvtregn, pnrate) %>%
  spread(years, pnrate)

pp[is.na(pp)] = "--"
ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"
pn[is.na(pn)] = "--"

# Put all input for the spreadsheet into a list
data <- list(df1 = pp,
             df2 = ch,
             df3 = wa,
             df4 = pn,
             filename = filename,
             sheetname = "Relative BHC",
             title_1 = "People in relative poverty before housing costs",
             title_2 = "Children in relative poverty before housing costs",
             title_3 = "Working-age adults in relative poverty before housing costs",
             title_4 = "Pensioners in relative poverty before housing costs",
             subtitle_1 = "Proportion of people with household incomes below 60% of the UK median",
             subtitle_2 = "Proportion of children with household incomes below 60% of the UK median",
             subtitle_3 = "Proportion of working-age adults with household incomes below 60% of the UK median",
             subtitle_4 = "Proportion of pensioners with household incomes below 60% of the UK median",
             headers = c(" ", levels(periods)),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(pp, ch, wa, pn, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)

# Relative poverty AHC ----------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getpovbynation, povvar = "low60ahc")) %>% 
  addyearvar() %>%
  checkandfmtUK

pp <- df %>%
  select(years, gvtregn, pprate) %>%
  spread(years, pprate)

ch <- df %>%
  select(years, gvtregn, chrate) %>%
  spread(years, chrate)

wa <- df %>%
  select(years, gvtregn, warate) %>%
  spread(years, warate)

pn <- df %>%
  select(years, gvtregn, pnrate) %>%
  spread(years, pnrate)

pp[is.na(pp)] = "--"
ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"
pn[is.na(pn)] = "--"
  
# Put all input for the spreadsheet into a list
data <- list(df1 = pp,
             df2 = ch,
             df3 = wa,
             df4 = pn,
             filename = filename,
             sheetname = "Relative AHC",
             title_1 = "People in relative poverty after housing costs",
             title_2 = "Children in relative poverty after housing costs",
             title_3 = "Working-age adults in relative poverty after housing costs",
             title_4 = "Pensioners in relative poverty after housing costs",
             subtitle_1 = "Proportion of people with household incomes below 60% of the UK median",
             subtitle_2 = "Proportion of children with household incomes below 60% of the UK median",
             subtitle_3 = "Proportion of working-age adults with household incomes below 60% of the UK median",
             subtitle_4 = "Proportion of pensioners with household incomes below 60% of the UK median",
             headers = c(" ", levels(periods)),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(pp, ch, wa, pn, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)



# Absolute poverty BHC ----------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getpovbynation, povvar = "abspovbhc")) %>% 
  addyearvar() %>%
  checkandfmtUK

pp <- df %>%
  select(years, gvtregn, pprate) %>%
  spread(years, pprate)

ch <- df %>%
  select(years, gvtregn, chrate) %>%
  spread(years, chrate)

wa <- df %>%
  select(years, gvtregn, warate) %>%
  spread(years, warate)

pn <- df %>%
  select(years, gvtregn, pnrate) %>%
  spread(years, pnrate)

pp[is.na(pp)] = "--"
ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"
pn[is.na(pn)] = "--"

# Put all input for the spreadsheet into a list
data <- list(df1 = pp,
             df2 = ch,
             df3 = wa,
             df4 = pn,
             filename = filename,
             sheetname = "Absolute BHC",
             title_1 = "People in absolute poverty before housing costs",
             title_2 = "Children in absolute poverty before housing costs",
             title_3 = "Working-age adults in absolute poverty before housing costs",
             title_4 = "Pensioners in absolute poverty before housing costs",
             subtitle_1 = "Proportion of people with household incomes below 60% of the 2010/11 UK median",
             subtitle_2 = "Proportion of children with household incomes below 60% of the 2010/11 UK median",
             subtitle_3 = "Proportion of working-age adults with household incomes below 60% of the 2010/11 UK median",
             subtitle_4 = "Proportion of pensioners with household incomes below 60% of the 2010/11 UK median",
             headers = c(" ", levels(periods)),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(pp, ch, wa, pn, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)

# Absolute poverty AHC ----------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getpovbynation, povvar = "abspovahc")) %>% 
  addyearvar() %>%
  checkandfmtUK

pp <- df %>%
  select(years, gvtregn, pprate) %>%
  spread(years, pprate)

ch <- df %>%
  select(years, gvtregn, chrate) %>%
  spread(years, chrate)

wa <- df %>%
  select(years, gvtregn, warate) %>%
  spread(years, warate)

pn <- df %>%
  select(years, gvtregn, pnrate) %>%
  spread(years, pnrate)

pp[is.na(pp)] = "--"
ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"
pn[is.na(pn)] = "--"

# Put all input for the spreadsheet into a list
data <- list(df1 = pp,
             df2 = ch,
             df3 = wa,
             df4 = pn,
             filename = filename,
             sheetname = "Absolute AHC",
             title_1 = "People in absolute poverty after housing costs",
             title_2 = "Children in absolute poverty after housing costs",
             title_3 = "Working-age adults in absolute poverty after housing costs",
             title_4 = "Pensioners in absolute poverty after housing costs",
             subtitle_1 = "Proportion of people with household incomes below 60% of the 2010/11 UK median",
             subtitle_2 = "Proportion of children with household incomes below 60% of the 2010/11 UK median",
             subtitle_3 = "Proportion of working-age adults with household incomes below 60% of the 2010/11 UK median",
             subtitle_4 = "Proportion of pensioners with household incomes below 60% of the 2010/11 UK median",
             headers = c(" ", levels(periods)),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(pp, ch, wa, pn, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)

# Child material deprivation BHC ----------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getpovbynation, povvar = "cmdbhc")) %>% 
  addyearvar() %>%
  checkandfmtUK

ch <- df %>%
  select(years, gvtregn, chrate) %>%
  spread(years, chrate)

# Remove older years so we don't have to deal with break in time series
ch <- ch[ , c(1,9:length(ch))]

# Put all input for the spreadsheet into a list
data <- list(df1 = ch,
             df2 = NULL,
             df3 = NULL,
             df4 = NULL,
             filename = filename,
             sheetname = "Child material deprivation BHC",
             title_1 = "Children in combined low income (before housing costs) and material deprivation",
             title_2 = NULL,
             title_3 = NULL,
             title_4 = NULL,
             subtitle_1 = "Proportion of children in combined low income and material deprivation",
             subtitle_2 = NULL,
             subtitle_3 = NULL,
             subtitle_4 = NULL,
             headers = c(" ", levels(periods)[20:length(periods)-2]),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

remove(ch, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)

# In-work poverty BHC --------------------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getworkpovbynation, povvar = "low60bhc")) %>% 
  addyearvar() %>%
  group_by(gvtregn) %>%
  arrange(gvtregn, years) %>%
  mutate_at(vars(ends_with("comp")), get3yraverage) %>%
  mutate_at(vars(ends_with("comp")), fmtpct) %>%
  mutate(years = factor(years, 
                        levels = labels[["years"]]$years, 
                        labels = labels[["years"]]$periods)) %>%
  filter(!is.na(chcomp))

ch <- df %>%
  select(years, gvtregn, chcomp) %>%
  spread(years, chcomp)

wa <- df %>%
  select(years, gvtregn, wacomp) %>%
  spread(years, wacomp)

ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"

# Put all input for the spreadsheet into a list
data <- list(df1 = ch,
             df2 = wa,
             df3 = NULL,
             df4 = NULL,
             filename = filename,
             sheetname = "Relative BHC by work status",
             title_1 = "Children in relative poverty before housing costs who live in a working household",
             title_2 = "Working-age adults in relative poverty before housing costs who live in a working household",
             title_3 = NULL,
             title_4 = NULL,
             subtitle_1 = "Proportion of children in relative poverty who live in a household where someone is in paid work",
             subtitle_2 = "Proportion of working-age adults in relative poverty who live in a household where someone is in paid work",
             subtitle_3 = NULL,
             subtitle_4 = NULL,
             headers = c(" ", levels(periods)[5:length(periods)-2]),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(ch, wa, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)


# In-work poverty AHC --------------------------------------------------------------------------

# Create time series dataset
df <- do.call(rbind.data.frame, lapply(hbai, getworkpovbynation, povvar = "low60ahc")) %>% 
  addyearvar() %>%
  group_by(gvtregn) %>%
  arrange(gvtregn, years) %>%
  mutate_at(vars(ends_with("comp")), get3yraverage) %>%
  mutate_at(vars(ends_with("comp")), fmtpct) %>%
  mutate(years = factor(years, 
                        levels = labels[["years"]]$years, 
                        labels = labels[["years"]]$periods)) %>%
  filter(!is.na(chcomp))

ch <- df %>%
  select(years, gvtregn, chcomp) %>%
  spread(years, chcomp)

wa <- df %>%
  select(years, gvtregn, wacomp) %>%
  spread(years, wacomp)

ch[is.na(ch)] = "--"
wa[is.na(wa)] = "--"

# Put all input for the spreadsheet into a list
data <- list(df1 = ch,
             df2 = wa,
             df3 = NULL,
             df4 = NULL,
             filename = filename,
             sheetname = "Relative AHC by work status",
             title_1 = "Children in relative poverty after housing costs who live in a working household",
             title_2 = "Working-age adults in relative poverty after housing costs who live in a working household",
             title_3 = NULL,
             title_4 = NULL,
             subtitle_1 = "Proportion of children in relative poverty who live in a household where someone is in paid work",
             subtitle_2 = "Proportion of working-age adults in relative poverty who live in a household where someone is in paid work",
             subtitle_3 = NULL,
             subtitle_4 = NULL,
             headers = c(" ", levels(periods)[5:length(periods)-2]),
             uberheaders = NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Northern Ireland data has been collected since 2002/03.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

remove(ch, wa, df)

# Create spreadsheet and first worksheet
createUKSpreadsheet(data)


# TOC ------------------------------------------------------------------------------
createContentSheet(paste0("output/", filename))

rm(list = ls())
