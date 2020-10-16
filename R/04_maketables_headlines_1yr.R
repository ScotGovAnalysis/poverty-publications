
# Create spreadsheet for single-year data

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

years <- labels[["years"]]$years
hbai <- readRDS("data/tidyhbai.rds")
filename <- "Poverty single year.xlsx"

# Relative poverty BHC ----

# Create time series dataset
relpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60bhc")) %>% addyearvar() %>% formatpov()

# Put all input for the spreadsheet into a list
data <- list(df = relpovbhc,
             filename = filename,
             sheetname = "Relative BHC",
             title = "Relative poverty before housing costs",
             subtitle = "Number and proportion of people with household incomes below 60% of the UK median, Scotland",
             headers = c("Year", "People", "Children", "Working-age adults", "Pensioners", 
                         "People", "Children", "Working-age adults", "Pensioners"),
             uberheaders = c(" " = 1, "Number" = 4, "Proportion" = 4),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and first worksheet
createSpreadsheet(data)

# Relative poverty AHC ----
relpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60ahc")) %>% addyearvar() %>% formatpov()
data[["df"]] <- relpovahc
data[["sheetname"]] <- "Relative AHC"
data[["title"]] <- "Relative poverty after housing costs"
createSpreadsheet(data)

# Absolute poverty BHC ----
abspovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovbhc")) %>% addyearvar() %>% formatpov()
data[["df"]] <- abspovbhc
data[["sheetname"]] <- "Absolute BHC"
data[["title"]] <- "Absolute poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 60% of the 2010/11 UK median, Scotland"
createSpreadsheet(data)

# Absolute poverty AHC ----
abspovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovahc")) %>% addyearvar() %>% formatpov()
data[["df"]] <- abspovahc
data[["sheetname"]] <- "Absolute AHC"
data[["title"]] <- "Absolute poverty after housing costs"
createSpreadsheet(data)

# Severe poverty BHC ----
sevpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low50bhc")) %>% addyearvar() %>% formatpov()
data[["df"]] <- sevpovbhc
data[["sheetname"]] <- "Severe BHC"
data[["title"]] <- "Severe poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 50% of the UK median, Scotland"
createSpreadsheet(data)

# Severe poverty AHC ----
sevpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low50ahc")) %>% addyearvar() %>% formatpov()
data[["df"]] <- sevpovahc
data[["sheetname"]] <- "Severe AHC"
data[["title"]] <- "Severe poverty after housing costs"
createSpreadsheet(data)

# Child material deprivation BHC ----

cmdbhc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdbhc_new")) %>%
  addyearvar() %>%
  rename(chnum_new = chnum,
         chrate_new = chrate)
cmdbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdbhc")) %>% addyearvar() %>%
  left_join(cmdbhc_new, by = "years") %>%
  select(years, chnum, chrate, chnum_new, chrate_new) %>%
  mutate(years = factor(years, 
                        levels = years, 
                        labels = labels[["years"]]$formatted[11:length(labels[["years"]]$years)]),
         chnum_new = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], 
                            chnum, chnum_new),
         chrate_new = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], 
                             chrate, chrate_new),
         chnum = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], 
                        NA, chnum),
         chrate = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], 
                         NA, chrate)) %>%
  mutate_at(vars(contains("num")), fmtpop) %>%
  mutate_at(vars(contains("rate")), fmtpct) %>%
  replace(., is.na(.), "-")
remove(cmdbhc_new)

data[["df"]] <- cmdbhc
data[["sheetname"]] <- "Child mat dep BHC"
data[["title"]] <- "Children in combined low income and material deprivation before housing costs"
data[["subtitle"]] <- "Number and proportion of children with household incomes below 70% of the UK median and unable to afford some basic necessities, Scotland"
data[["headers"]] <- c("Year", "Number", "Proportion", "Number", "Proportion")
data[["uberheaders"]] <- c(" " = 1, "Old measure" = 2, "New measure" = 2)
data[["footnotes"]] <- "The definition of material deprivation changed in 2010/11, creating a break in the time series."
createSpreadsheet(data)

# Child material deprivation AHC ----
cmdahc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc_new")) %>%
  addyearvar() %>%
  rename(chnum_new = chnum,
         chrate_new = chrate)
cmdahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc")) %>% addyearvar() %>%
  left_join(cmdahc_new, by = "years") %>%
  select(years, chnum, chrate, chnum_new, chrate_new) %>%
  mutate(years = factor(years, 
                        levels = years, 
                        labels = labels[["years"]]$formatted[11:length(labels[["years"]]$years)]),
         chnum_new = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], chnum, chnum_new),
         chrate_new = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], chrate, chrate_new),
         chnum = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], NA, chnum),
         chrate = ifelse(years %in% labels[["years"]]$formatted[18:length(labels[["years"]]$years)], NA, chrate)) %>%
  mutate_at(vars(contains("num")), fmtpop) %>%
  mutate_at(vars(contains("rate")), fmtpct) %>%
  replace(., is.na(.), "-")
remove(cmdahc_new)

data[["df"]] <- cmdahc
data[["sheetname"]] <- "Child mat dep AHC"
data[["title"]] <- "Children in combined low income and material deprivation after housing costs"
createSpreadsheet(data)

# Pensioner deprivation ----
pndep <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "mdpn"))%>% addyearvar() %>%
  select(years, pnnum, pnrate) %>%
  mutate(years = factor(years, 
                 levels = years, 
                 labels = labels[["years"]]$formatted[16:length(labels[["years"]]$years)])) %>%
  mutate_at(vars(contains("num")), fmtpop) %>%
  mutate_at(vars(contains("rate")), fmtpct)

data[["df"]] <- pndep
data[["sheetname"]] <- "Pensioner dep"
data[["title"]] <- "Pensioners in material deprivation"
data[["subtitle"]] <- "Number and proportion of pensioners with limited access to goods and services, Scotland"
data[["headers"]] <- c("Year", "Number", "Proportion")
data[["uberheaders"]] <- NULL
data[["footnotes"]] <- c("Pensioner material deprivation is different to other measures of poverty, including the child low income and material deprivation measure ",
                         "in that it is not associated with an income threshold. It captures issues such as whether poor health, disability and social isolation ",
                         "prevent access to goods and services, rather than solely low income.")
createSpreadsheet(data)

# TOC ----

createContentSheet(paste0("output/", filename))

rm(list = ls())

