
# Create spreadsheet for 3-year averaged data

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

filename <- "Poverty three-year average.xlsx"

years <- labels[["years"]]$periods
hbai <- readRDS("data/tidyhbai.rds")


# Relative poverty BHC ----

# Create time series dataset

relpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60bhc")) %>% addyearvar() %>%
  formatpov3yraverage()

# Put all input for the spreadsheet into a list
data <- list(df = relpovbhc,
             filename = filename,
             sheetname = "Relative BHC",
             title = "Relative poverty before housing costs",
             subtitle = "Number and proportion of people with household incomes below 60% of the UK median, Scotland",
             headers = c("Years", "People", "Children", "Working-age adults", "Pensioners", 
                         "People", "Children", "Working-age adults", "Pensioners"),
             uberheaders = c(" " = 1, "Number" = 4, "Proportion" = 4),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and first worksheet
createSpreadsheet(data)

# Relative poverty AHC ----
relpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60ahc")) %>% addyearvar() %>% formatpov3yraverage()
data[["df"]] <- relpovahc
data[["sheetname"]] <- "Relative AHC"
data[["title"]] <- "Relative poverty after housing costs"
createSpreadsheet(data)

# Absolute poverty BHC ----
abspovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovbhc")) %>% addyearvar() %>% formatpov3yraverage()
data[["df"]] <- abspovbhc
data[["sheetname"]] <- "Absolute BHC"
data[["title"]] <- "Absolute poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 60% of the 2010/11 UK median, Scotland"
createSpreadsheet(data)

# Absolute poverty AHC ----
abspovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovahc")) %>% addyearvar() %>% formatpov3yraverage()
data[["df"]] <- abspovahc
data[["sheetname"]] <- "Absolute AHC"
data[["title"]] <- "Absolute poverty after housing costs"
createSpreadsheet(data)

# Severe poverty BHC ----
sevpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low50bhc")) %>% addyearvar() %>% formatpov3yraverage()
data[["df"]] <- sevpovbhc
data[["sheetname"]] <- "Severe BHC"
data[["title"]] <- "Severe poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 50% of the UK median, Scotland"
createSpreadsheet(data)

# Severe poverty AHC ----
sevpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low50ahc")) %>% addyearvar() %>% formatpov3yraverage()
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
  mutate(chnum = ifelse(years %in% years[11:17], get3yraverage(chnum),
                        ifelse(years == 1112, NA,
                               ifelse(years == 1213, (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                                      get3yraverage(chnum)))),
         chrate = ifelse(years %in% years[11:17], get3yraverage(chrate),
                         ifelse(years == 1112, NA,
                                ifelse(years == 1213, (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                                       get3yraverage(chrate)))),
         chnum = fmtpop(chnum),
         chrate = fmtpct(chrate)) %>%
  tail(-2L) %>%
  select(1:3) %>%
  replace(., is.na(.), "-") %>%
  mutate(years = factor(years, labels = labels[["years"]]$periods[13:length(labels[["years"]]$periods)]))
  
remove(cmdbhc_new)

data[["df"]] <- cmdbhc
data[["sheetname"]] <- "Child mat dep BHC"
data[["title"]] <- "Children in combined low income and material deprivation before housing costs"
data[["subtitle"]] <- "Number and proportion of children with household incomes below 70% of the UK median and unable to afford some basic necessities, Scotland"
data[["headers"]] <- c("Years", "Number", "Proportion")
data[["uberheaders"]] <- NULL
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
  mutate(chnum = ifelse(years %in% years[11:17], get3yraverage(chnum),
                        ifelse(years == 1112, NA,
                               ifelse(years == 1213, (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                                      get3yraverage(chnum)))),
         chrate = ifelse(years %in% years[11:17], get3yraverage(chrate),
                         ifelse(years == 1112, NA,
                                ifelse(years == 1213, (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                                       get3yraverage(chrate)))),
         chnum = fmtpop(chnum),
         chrate = fmtpct(chrate)) %>%
  tail(-2L) %>%
  select(1:3) %>%
  replace(., is.na(.), "-") %>%
  mutate(years = factor(years, labels = labels[["years"]]$periods[13:length(labels[["years"]]$periods)]))

remove(cmdahc_new)

data[["df"]] <- cmdahc
data[["sheetname"]] <- "Child mat dep AHC"
data[["title"]] <- "Children in combined low income and material deprivation after housing costs"
createSpreadsheet(data)

# Pensioner deprivation ----
pndep <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "mdpn"))%>% addyearvar() %>%
  select(years, pnnum, pnrate) %>%
  mutate(pnnum = get3yraverage(pnnum),
         pnrate = get3yraverage(pnrate),
         years = factor(years, 
                        labels = labels[["years"]]$periods[16:length(labels[["years"]]$periods)])) %>%
  mutate_at(vars(contains("num")), fmtpop) %>%
  mutate_at(vars(contains("rate")), fmtpct) %>%
  tail(-2L)

data[["df"]] <- pndep
data[["sheetname"]] <- "Pensioner dep"
data[["title"]] <- "Pensioners in material deprivation"
data[["subtitle"]] <- "Number and proportion of pensioners with limited access to goods and services, Scotland"
data[["headers"]] <- c("Years", "Number", "Proportion")
data[["footnotes"]] <- c("Pensioner material deprivation is different to other measures of poverty, including the child low income and material deprivation measure ",
                         "in that it is not associated with an income threshold. It captures issues such as whether poor health, disability and social isolation ",
                         "prevent access to goods and services, rather than solely low income.")
createSpreadsheet(data)

# TOC --------------------------------------------------------------------

createContentSheet(paste0("output/", filename))

rm(list = ls())