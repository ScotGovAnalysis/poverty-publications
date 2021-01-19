# Create spreadsheet for 3-year averaged data

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

filename <- "All three-year average.xlsx"

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

# Get formatted periods in vector; get total number of years/periods
periods <- labels[["years"]]$periods
yearsno <- length(periods)
period5yr <- levels(labels[["years"]]$period5yr) %>% tail(1L)
latestyear <- max(levels(labels[["years"]]$formatted))

# 1. Headlines ----
createSepsheet(filename = filename, sheetname = "- 1 -",
               text = "1 - Headline poverty estimates")

# Relative poverty BHC ----

# Create time series dataset

relpovbhc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "low60bhc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

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

# Create worksheet
createSpreadsheet(data)

# Relative poverty AHC ----
relpovahc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "low60ahc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

data[["df"]] <- relpovahc
data[["sheetname"]] <- "Relative AHC"
data[["title"]] <- "Relative poverty after housing costs"
createSpreadsheet(data)

# Absolute poverty BHC ----
abspovbhc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "abspovbhc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

data[["df"]] <- abspovbhc
data[["sheetname"]] <- "Absolute BHC"
data[["title"]] <- "Absolute poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 60% of the 2010/11 UK median, Scotland"
createSpreadsheet(data)

# Absolute poverty AHC ----
abspovahc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "abspovahc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

data[["df"]] <- abspovahc
data[["sheetname"]] <- "Absolute AHC"
data[["title"]] <- "Absolute poverty after housing costs"
createSpreadsheet(data)

# Severe poverty BHC ----
sevpovbhc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "low50bhc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

data[["df"]] <- sevpovbhc
data[["sheetname"]] <- "Severe BHC"
data[["title"]] <- "Severe poverty before housing costs"
data[["subtitle"]] <- "Number and proportion of people with household incomes below 50% of the UK median, Scotland"
createSpreadsheet(data)

# Severe poverty AHC ----
sevpovahc <- do.call(rbind.data.frame,
                     lapply(hbai, getpov, povvar = "low50ahc")) %>%
  addyearvar() %>%
  formatpov3yraverage() %>%
  select(years, ppnum, chnum, wanum, pnnum, pprate, chrate, warate, pnrate)

data[["df"]] <- sevpovahc
data[["sheetname"]] <- "Severe AHC"
data[["title"]] <- "Severe poverty after housing costs"
createSpreadsheet(data)

# Child material deprivation BHC ----
cmdbhc_new <- do.call(rbind.data.frame,
                      lapply(hbai, getpov, povvar = "cmdbhc_new")) %>%
  addyearvar() %>%
  rename(chnum_new = chnum,
         chrate_new = chrate)

cmdbhc <- do.call(rbind.data.frame,
                  lapply(hbai, getpov, povvar = "cmdbhc")) %>%
  addyearvar() %>%
  left_join(cmdbhc_new, by = "years") %>%
  select(years, chnum, chrate, chnum_new, chrate_new) %>%
  mutate(chnum = ifelse(years == 1213,
                        (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                        get3yraverage(chnum)),
         chnum = ifelse(years == 1112, NA, chnum),
         chnum = fmtpop(chnum),
         chrate = ifelse(years == 1213,
                         (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                         get3yraverage(chrate)),
         chrate = ifelse(years == 1112, NA, chrate),
         chrate = fmtpct(chrate)) %>%
  tail(-2L) %>%
  select(1:3) %>%
  replace(., is.na(.), "-") %>%
  mutate(years = factor(years,
                        labels = periods[13:length(periods)]))

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
  mutate(chnum = ifelse(years == 1213,
                        (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                        get3yraverage(chnum)),
         chnum = ifelse(years == 1112, NA, chnum),
         chnum = fmtpop(chnum),
         chrate = ifelse(years == 1213,
                         (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                         get3yraverage(chrate)),
         chrate = ifelse(years == 1112, NA, chrate),
         chrate = fmtpct(chrate)) %>%
  tail(-2L) %>%
  select(1:3) %>%
  replace(., is.na(.), "-") %>%
  mutate(years = factor(years,
                        labels = periods[13:length(periods)]))

remove(cmdahc_new)

data[["df"]] <- cmdahc
data[["sheetname"]] <- "Child mat dep AHC"
data[["title"]] <- "Children in combined low income and material deprivation after housing costs"
createSpreadsheet(data)

# Pensioner deprivation ----
pndep <- do.call(rbind.data.frame, lapply(hbai, getpmd)) %>%
  addyearvar() %>%
  select(years, pnnum, pnrate) %>%
  mutate(pnnum = get3yraverage(pnnum),
         pnrate = get3yraverage(pnrate),
         years = factor(years,
                        labels = periods[16:length(periods)])) %>%
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

remove(abspovbhc, abspovahc, cmdahc, cmdbhc, data, pndep, relpovahc, relpovbhc,
       sevpovahc, sevpovbhc)

# 2. Characteristics ----
createSepsheet(filename = filename, sheetname = "- 2 -",
               text = "2 - Poverty characteristics")

# Tenure -----------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample")

rel_rates <- splitntranspose(rel, "pprate")
rel_comps <- splitntranspose(rel, "ppcomp")
rel_numbers <- splitntranspose(rel, "ppnum")

sev_rates <- splitntranspose(sev, "pprate")
sev_comps <- splitntranspose(sev, "ppcomp")
sev_numbers <- splitntranspose(sev, "ppnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Tenure",
             title_a = "A. Proportion of people in poverty and severe poverty by housing tenure",
             title_b = "B. Composition of people in poverty and severe poverty by housing tenure",
             title_c = "C. Number of people in poverty and severe poverty by housing tenure",
             title_d = "D. Sample sizes",

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",

             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[12:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on housing tenure is not available prior to 2003.",
                           "3. Due to a single very large household in the sample in the 'Owned outright' category in 2017/18, the latest estimates are significantly higher than those in previous years.",
                           "However, further data points are required to confirm whether this marks an increasing trend in poverty.")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Urban/rural class ------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample")

rel_rates <- splitntranspose(rel, "pprate")
rel_comps <- splitntranspose(rel, "ppcomp")
rel_numbers <- splitntranspose(rel, "ppnum")

sev_rates <- splitntranspose(sev, "pprate")
sev_comps <- splitntranspose(sev, "ppcomp")
sev_numbers <- splitntranspose(sev, "ppnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Urban rural",
             title_a = "A. Proportion of people in poverty and severe poverty by urban/rural class",
             title_b = "B. Composition of people in poverty and severe poverty by urban/rural class",
             title_c = "C. Number of people in poverty and severe poverty by urban/rural class",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[15:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on urban/rural class is not available prior to 2006.")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Number of children in household ----------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "depchldh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "depchldh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample")

rel_rates <- splitntranspose(rel, "pprate")
rel_comps <- splitntranspose(rel, "ppcomp")
rel_numbers <- splitntranspose(rel, "ppnum")

sev_rates <- splitntranspose(sev, "pprate")
sev_comps <- splitntranspose(sev, "ppcomp")
sev_numbers <- splitntranspose(sev, "ppnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Number of children",
             title_a = "A. Proportion of people in poverty and severe poverty by the number of children in the household",
             title_b = "B. Composition of people in poverty and severe poverty by the number of children in the household",
             title_c = "C. Number of people in poverty and severe poverty by the number of children in the household",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Age (adults) -----------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low60ahc",
                      groupingvar = "ageband")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc",
                      groupingvar = "ageband")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ad")

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Age band",
             title_a = "A. Proportion of adults in poverty and severe poverty by age",
             title_b = "B. Composition of adults in poverty and severe poverty by age",
             title_c = "C. Number of adults in poverty and severe poverty by age",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of adults in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

# Gender (adults) --------------------------------------------------------------

# filter out non-single BUs to get the totals right
rel <- do.call(rbind.data.frame,
               lapply(hbai, function(x) {
                 y <- filter(x, singlehh != "(Missing)")
                 getpovby(df = y, povvar = "low60ahc", groupingvar = "singlehh")
               } )) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, function(x) {
                 y <- filter(x, singlehh != "(Missing)")
                 getpovby(df = y, povvar = "low50ahc", groupingvar = "singlehh")
               } )) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ad")

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Gender",
             title_a = "A. Proportion of single adults in poverty and severe poverty by gender",
             title_b = "B. Composition of single adults in poverty and severe poverty by gender",
             title_c = "C. Number of single adults in poverty and severe poverty by gender",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of single adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of single adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of single adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of single adults in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Single adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Single adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. The term 'single' here refers to adults who are sharing a household with no other adults.",
                           "This differs from the analysis in the 'Family type' worksheet, where single adults may share the household with other families.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Family type (adults) ---------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ad")

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Family type",
             title_a = "A. Proportion of adults in poverty and severe poverty by family type",
             title_b = "B. Composition of adults in poverty and severe poverty by family type",
             title_c = "C. Number of adults in poverty and severe poverty by family type",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. 'Pensioner couples' include working-age adults who are in a couple with a pensioner.",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.",
                           "A household may contain more than one family.",
                           "5. 'Single' adults in this analysis refer to single-adult families, not single-adult households. In some cases, single adult families may share a household with other families.",
                           "This differs from the analysis in the 'Gender' worksheet, where single adults are those who share the household with no other adults.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Marital status (adults) ------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low60ahc",
                      groupingvar = "marital")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc",
                      groupingvar = "marital")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ad")

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Marital status",
             title_a = "A. Proportion of adults in poverty and severe poverty by marital status",
             title_b = "B. Composition of adults in poverty and severe poverty by marital status",
             title_c = "C. Number of adults in poverty and severe poverty by marital status",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of adults in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. 'Single' refers to adults who have never been married or in a civil partnership, and are not living with a partner.",
                           "4. 'Separated' refers to adults who are married or in a civil partnership, but are not living together because of estrangement.",
                           "5. 'Married / Civil Partnership' includes couples who are temporarily living apart (e.g. due to serving in the armed forces).",
                           "6. 'Adults' includes working-age adults and pensioners.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Family economic status (working-age adults) ----------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_wa")

rel_rates <- splitntranspose(rel, "warate")
rel_comps <- splitntranspose(rel, "wacomp")
rel_numbers <- splitntranspose(rel, "wanum")

sev_rates <- splitntranspose(sev, "warate")
sev_comps <- splitntranspose(sev, "wacomp")
sev_numbers <- splitntranspose(sev, "wanum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Family economic status",
             title_a = "A. Proportion of working-age adults in poverty and severe poverty by family economic status",
             title_b = "B. Composition of working-age adults in poverty and severe poverty by family economic status",
             title_c = "C. Number of working-age adults in poverty and severe poverty by family economic status",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of working-age adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of working-age adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of working-age adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Working-age adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Working-age adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Household work status (working-age adults) -----------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_wa")

rel_rates <- splitntranspose(rel, "warate")
rel_comps <- splitntranspose(rel, "wacomp")
rel_numbers <- splitntranspose(rel, "wanum")

sev_rates <- splitntranspose(sev, "warate")
sev_comps <- splitntranspose(sev, "wacomp")
sev_numbers <- splitntranspose(sev, "wanum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Household work status",
             title_a = "A. Proportion of working-age adults in poverty and severe poverty by household work status",
             title_b = "B. Composition of working-age adults in poverty and severe poverty by household work status",
             title_c = "C. Number of working-age adults in poverty and severe poverty by household work status",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of working-age adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of working-age adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of working-age adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of working-age families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Working-age adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Working-age adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Disability -------------------------------------------------------------------

rel_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc",
                         groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc",
                         groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc",
                         groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc",
                         groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc",
                         groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc",
                         groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample1 <- splitntranspose(rel_pp, "groupsample")
sample2 <- splitntranspose(rel_ch, "groupsample") %>% filter(groupingvar != "All")
sample3 <- splitntranspose(rel_ad, "groupsample") %>% filter(groupingvar != "All")

rel_rates1 <- splitntranspose(rel_pp, "pprate")
rel_comps1 <- splitntranspose(rel_pp, "ppcomp")
rel_numbers1 <- splitntranspose(rel_pp, "ppnum")

sev_rates1 <- splitntranspose(sev_pp, "pprate")
sev_comps1 <- splitntranspose(sev_pp, "ppcomp")
sev_numbers1 <- splitntranspose(sev_pp, "ppnum")

rel_rates2 <- splitntranspose(rel_ch, "pprate") %>% filter(groupingvar != "All")
rel_comps2 <- splitntranspose(rel_ch, "ppcomp") %>% filter(groupingvar != "All")
rel_numbers2 <- splitntranspose(rel_ch, "ppnum") %>% filter(groupingvar != "All")

sev_rates2 <- splitntranspose(sev_ch, "pprate") %>% filter(groupingvar != "All")
sev_comps2 <- splitntranspose(sev_ch, "ppcomp") %>% filter(groupingvar != "All")
sev_numbers2 <- splitntranspose(sev_ch, "ppnum") %>% filter(groupingvar != "All")

rel_rates3 <- splitntranspose(rel_ad, "pprate") %>% filter(groupingvar != "All")
rel_comps3 <- splitntranspose(rel_ad, "ppcomp") %>% filter(groupingvar != "All")
rel_numbers3 <- splitntranspose(rel_ad, "ppnum")  %>% filter(groupingvar != "All")

sev_rates3 <- splitntranspose(sev_ad, "pprate") %>% filter(groupingvar != "All")
sev_comps3 <- splitntranspose(sev_ad, "ppcomp") %>% filter(groupingvar != "All")
sev_numbers3 <- splitntranspose(sev_ad, "ppnum")  %>% filter(groupingvar != "All")

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)

sample <- rbind(sample1, sample2, sample3)

# mark time series break
rel_numbers[, 12] <- "--"
rel_numbers[, 13] <- "--"
sev_numbers[, 12] <- "--"
sev_numbers[, 13] <- "--"


# put all input for the spreadsheet into a list
data <- list(sheetname = "Disability",
             title_a = "A. Proportion of people in poverty and severe poverty by whether there is a disabled person in the household",
             title_b = "B. Composition of people in poverty and severe poverty by whether there is a disabled person in the household",
             title_c = "C. Number of people in poverty and severe poverty by whether there is a disabled person in the household",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "3. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "4. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "5. Data on disability is available from 1995/96.")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel_pp, rel_ch, rel_ad, rel_rates, rel_rates1, rel_rates2, rel_rates3,
       rel_comps, rel_comps1, rel_comps2, rel_comps3,
       rel_numbers, rel_numbers1, rel_numbers2, rel_numbers3,
       sev_pp, sev_ch, sev_ad, sev_rates, sev_rates1, sev_rates2, sev_rates3,
       sev_comps, sev_comps1, sev_comps2, sev_comps3,
       sev_numbers, sev_numbers1, sev_numbers2, sev_numbers3)


# Disability with benefits removed from income ---------------------------------

rel_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis",
                         groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis",
                         groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis",
                         groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis",
                         groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis",
                         groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis",
                         groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample1 <- splitntranspose(rel_pp, "groupsample")
sample2 <- splitntranspose(rel_ch, "groupsample") %>% filter(groupingvar != "All")
sample3 <- splitntranspose(rel_ad, "groupsample") %>% filter(groupingvar != "All")

rel_rates1 <- splitntranspose(rel_pp, "pprate")
rel_comps1 <- splitntranspose(rel_pp, "ppcomp")
rel_numbers1 <- splitntranspose(rel_pp, "ppnum")

sev_rates1 <- splitntranspose(sev_pp, "pprate")
sev_comps1 <- splitntranspose(sev_pp, "ppcomp")
sev_numbers1 <- splitntranspose(sev_pp, "ppnum")

rel_rates2 <- splitntranspose(rel_ch, "pprate") %>% filter(groupingvar != "All")
rel_comps2 <- splitntranspose(rel_ch, "ppcomp") %>% filter(groupingvar != "All")
rel_numbers2 <- splitntranspose(rel_ch, "ppnum") %>% filter(groupingvar != "All")

sev_rates2 <- splitntranspose(sev_ch, "pprate") %>% filter(groupingvar != "All")
sev_comps2 <- splitntranspose(sev_ch, "ppcomp") %>% filter(groupingvar != "All")
sev_numbers2 <- splitntranspose(sev_ch, "ppnum") %>% filter(groupingvar != "All")

rel_rates3 <- splitntranspose(rel_ad, "pprate") %>% filter(groupingvar != "All")
rel_comps3 <- splitntranspose(rel_ad, "ppcomp") %>% filter(groupingvar != "All")
rel_numbers3 <- splitntranspose(rel_ad, "ppnum")  %>% filter(groupingvar != "All")

sev_rates3 <- splitntranspose(sev_ad, "pprate") %>% filter(groupingvar != "All")
sev_comps3 <- splitntranspose(sev_ad, "ppcomp") %>% filter(groupingvar != "All")
sev_numbers3 <- splitntranspose(sev_ad, "ppnum")  %>% filter(groupingvar != "All")

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)

sample <- rbind(sample1, sample2, sample3)

# mark time series break
rel_numbers[, 12] <- "--"
rel_numbers[, 13] <- "--"
sev_numbers[, 12] <- "--"
sev_numbers[, 13] <- "--"


# put all input for the spreadsheet into a list
data <- list(sheetname = "DisabilityBenefitsRemoved",
             title_a = "A. Proportion of people in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_b = "B. Composition of people in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_c = "C. Number of people in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income from Disability Living Allowance (DLA), Attendance Allowance (AA), and Personal Independence Payments (PIP) have been excluded from income.",
                           "This income is related to additional living costs associated with a disability.",
                           "2. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "4. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "5. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "6. Data on disability is available from 1995/96.")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel_pp, rel_ch, rel_ad, rel_rates, rel_rates1, rel_rates2, rel_rates3,
       rel_comps, rel_comps1, rel_comps2, rel_comps3,
       rel_numbers, rel_numbers1, rel_numbers2, rel_numbers3,
       sev_pp, sev_ch, sev_ad, sev_rates, sev_rates1, sev_rates2, sev_rates3,
       sev_comps, sev_comps1, sev_comps2, sev_comps3,
       sev_numbers, sev_numbers1, sev_numbers2, sev_numbers3)

# Ethnicity --------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc",
                      groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample")

rel_rates <- splitntranspose(rel, "pprate")
rel_comps <- splitntranspose(rel, "ppcomp")
rel_numbers <- splitntranspose(rel, "ppnum")

sev_rates <- splitntranspose(sev, "pprate")
sev_comps <- splitntranspose(sev, "ppcomp")
sev_numbers <- splitntranspose(sev, "ppnum")

# remove composition data - not reliable for ethnicity
rel_comps[, 2] <- "--"
sev_comps[, 2] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Ethnicity",
             title_a = "A. Proportion of people in poverty and severe poverty by ethnic group (five-year averages)",
             title_b = "B. Composition of people in poverty and severe poverty by ethnic group (five-year averages)",
             title_c = "C. Number of people in poverty and severe poverty by ethnic group (five-year averages)",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subtitle_d = "Number of families in each category in the five-year survey sample, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", period5yr),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. Ethnicity data relates to all people in a household and is based on the ethnicity of the adult with the highest income.",
                           "3. Different ethnic groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. Table B (composition) is not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. A time series is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "6. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Religion (adults) ------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low60ahc",
                      groupingvar = "religsc")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck_ad %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc",
                      groupingvar = "religsc")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck_ad

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ad")

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# remove composition data - not reliable for religion
rel_comps[, 2] <- "--"
sev_comps[, 2] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Religion",
             title_a = "A. Proportion of adults in poverty and severe poverty by religion (five-year averages)",
             title_b = "B. Composition of adults in poverty and severe poverty by religion (five-year averages)",
             title_c = "C. Number of adults in poverty and severe poverty by religion (five-year averages)",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subtitle_d = "Number of adults in each category in the five-year survey sample, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", period5yr),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "3. Different religious groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. Table B (composition) is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. A time series is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "6. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

# 3. Characteristics - children ----
createSepsheet(filename = filename, sheetname = "- 3 -",
               text = "3 - Child poverty characteristics")

# Child age ------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovbychildage, povvar = "low60ahc")) %>%
  addyearvar %>%
  formatpov3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  samplesizecheck_childage %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovbychildage, povvar = "low50ahc")) %>%
  addyearvar %>%
  formatpov3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  samplesizecheck_childage

# split dataset into rates, numbers, compositions and transpose
rel_rates1 <- rel %>% select(years, rate_ch) %>% spread(years, rate_ch) %>% mutate(group = "All")
rel_rates2 <- rel %>% select(years, rate0_4) %>% spread(years, rate0_4) %>% mutate(group = "0 - 4")
rel_rates3 <- rel %>% select(years, rate5_12) %>% spread(years, rate5_12) %>% mutate(group = "5 - 12")
rel_rates4 <- rel %>% select(years, rate13_19) %>% spread(years, rate13_19) %>% mutate(group = "13 - 19")
rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3, rel_rates4) %>% select(group, everything())

rel_comps1 <- rel %>% select(years, comp_ch) %>% spread(years, comp_ch) %>% mutate(group = "All")
rel_comps2 <- rel %>% select(years, comp0_4) %>% spread(years, comp0_4) %>% mutate(group = "0 - 4")
rel_comps3 <- rel %>% select(years, comp5_12) %>% spread(years, comp5_12) %>% mutate(group = "5 - 12")
rel_comps4 <- rel %>% select(years, comp13_19) %>% spread(years, comp13_19) %>% mutate(group = "13 - 19")
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3, rel_comps4) %>% select(group, everything())

rel_numbers1 <- rel %>% select(years, num_ch) %>% spread(years, num_ch) %>% mutate(group = "All")
rel_numbers2 <- rel %>% select(years, num0_4) %>% spread(years, num0_4) %>% mutate(group = "0 - 4")
rel_numbers3 <- rel %>% select(years, num5_12) %>% spread(years, num5_12) %>% mutate(group = "5 - 12")
rel_numbers4 <- rel %>% select(years, num13_19) %>% spread(years, num13_19) %>% mutate(group = "13 - 19")
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3, rel_numbers4) %>% select(group, everything())

sev_rates1 <- sev %>% select(years, rate_ch) %>% spread(years, rate_ch) %>% mutate(group = "All")
sev_rates2 <- sev %>% select(years, rate0_4) %>% spread(years, rate0_4) %>% mutate(group = "0 - 4")
sev_rates3 <- sev %>% select(years, rate5_12) %>% spread(years, rate5_12) %>% mutate(group = "5 - 12")
sev_rates4 <- sev %>% select(years, rate13_19) %>% spread(years, rate13_19) %>% mutate(group = "13 - 19")
sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3, sev_rates4) %>% select(group, everything())

sev_comps1 <- sev %>% select(years, comp_ch) %>% spread(years, comp_ch) %>% mutate(group = "All")
sev_comps2 <- sev %>% select(years, comp0_4) %>% spread(years, comp0_4) %>% mutate(group = "0 - 4")
sev_comps3 <- sev %>% select(years, comp5_12) %>% spread(years, comp5_12) %>% mutate(group = "5 - 12")
sev_comps4 <- sev %>% select(years, comp13_19) %>% spread(years, comp13_19) %>% mutate(group = "13 - 19")
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3, sev_comps4) %>% select(group, everything())

sev_numbers1 <- sev %>% select(years, num_ch) %>% spread(years, num_ch) %>% mutate(group = "All")
sev_numbers2 <- sev %>% select(years, num0_4) %>% spread(years, num0_4) %>% mutate(group = "0 - 4")
sev_numbers3 <- sev %>% select(years, num5_12) %>% spread(years, num5_12) %>% mutate(group = "5 - 12")
sev_numbers4 <- sev %>% select(years, num13_19) %>% spread(years, num13_19) %>% mutate(group = "13 - 19")
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3, sev_numbers4) %>% select(group, everything())

sample1 <- rel %>% select(years, groupsample_ch) %>% spread(years, groupsample_ch) %>% mutate(group = "All")
sample2 <- rel %>% select(years, groupsample0_4) %>% spread(years, groupsample0_4) %>% mutate(group = "0 - 4")
sample3 <- rel %>% select(years, groupsample5_12) %>% spread(years, groupsample5_12) %>% mutate(group = "5 - 12")
sample4 <- rel %>% select(years, groupsample13_19) %>% spread(years, groupsample13_19) %>% mutate(group = "13 - 19")
sample <- rbind(sample1, sample2, sample3, sample4) %>% select(group, everything())

# put all input for the spreadsheet into a list
data <- list(sheetname = "Child age",
             title_a = "A. Proportion of children in poverty and severe poverty by age of child",
             title_b = "B. Composition of children in poverty and severe poverty by age of child",
             title_c = "C. Number of children in poverty and severe poverty by age of child",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Tenure ---------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Tenure ch",
             title_a = "A. Proportion of children in poverty and severe poverty by housing tenure",
             title_b = "B. Composition of children in poverty and severe poverty by housing tenure",
             title_c = "C. Number of children in poverty and severe poverty by housing tenure",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[12:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on housing tenure is not available prior to 2003.",
                           "3. Due to a single very large household in the sample in the 'Owned outright' category in 2017/18, the latest estimates are significantly higher than those in previous years.",
                           "However, further data points are required to confirm whether this marks an increasing trend in poverty.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Urban/rural class ----------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Urban rural ch",
             title_a = "A. Proportion of children in poverty and severe poverty by urban/rural class",
             title_b = "B. Composition of children in poverty and severe poverty by urban/rural class",
             title_c = "C. Number of children in poverty and severe poverty by urban/rural class",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[15:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on urban/rural class is not available prior to 2006.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)



# Number of children in household --------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "depchldh_ch")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "depchldh_ch")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Number of children ch",
             title_a = "A. Proportion of children in poverty and severe poverty by the number of children in the household",
             title_b = "B. Composition of children in poverty and severe poverty by the number of children in the household",
             title_c = "C. Number of children in poverty and severe poverty by the number of children in the household",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Age of youngest child in household --------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "babyhh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "babyhh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# remove estimates where age not available
rel_rates[2:3, 8:13] <- "--"
rel_comps[2:3, 8:13] <- "--"
rel_numbers[2:3, 8:13] <- "--"
sev_rates[2:3, 8:13] <- "--"
sev_comps[2:3, 8:13] <- "--"
sev_numbers[2:3, 8:13] <- "--"
sample[2:3, 8:13] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Age of youngest child",
             title_a = "A. Proportion of children in poverty and severe poverty by age of the youngest child in the household",
             title_b = "B. Composition of children in poverty and severe poverty by age of the youngest child in the household",
             title_c = "C. Number of children in poverty and severe poverty by age of the youngest child in the household",
             title_d = "D. Sample sizes",

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Individual child age data is not available between 2003 and 2005.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Age of mother --------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "youngmumhh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9900" ) %>%
  mutate(years = factor(years, labels = periods[6:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "youngmumhh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9900" ) %>%
  mutate(years = factor(years, labels = periods[6:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# remove estimates where age not available
rel_rates[2:3, 5:10] <- "--"
rel_comps[2:3, 5:10] <- "--"
rel_numbers[2:3, 5:10] <- "--"
sev_rates[2:3, 5:10] <- "--"
sev_comps[2:3, 5:10] <- "--"
sev_numbers[2:3, 5:10] <- "--"
sample[2:3, 5:10] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Age of mother",
             title_a = "A. Proportion of children in poverty and severe poverty by age of mother",
             title_b = "B. Composition of children in poverty and severe poverty by age of mother",
             title_c = "C. Number of children in poverty and severe poverty by age of mother",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[6:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Individual age data is available from 1997, but not between 2003 and 2005.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Family type (children) -------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "loneparenthh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "loneparenthh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Family type ch",
             title_a = "A. Proportion of children in poverty and severe poverty by family type",
             title_b = "B. Composition of children in poverty and severe poverty by family type",
             title_c = "C. Number of children in poverty and severe poverty by family type",
             title_d = "D. Sample sizes",

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Family economic status (children) -------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Family economic status ch",
             title_a = "A. Proportion of children in poverty and severe poverty by family economic status",
             title_b = "B. Composition of children in poverty and severe poverty by family economic status",
             title_c = "C. Number of children in poverty and severe poverty by family economic status",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Household work status (children) ---------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Household work status ch",
             title_a = "A. Proportion of children in poverty and severe poverty by household work status",
             title_b = "B. Composition of children in poverty and severe poverty by household work status",
             title_c = "C. Number of children in poverty and severe poverty by household work status",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)


# Disability -----------------------------------------------------------------------------------

rel_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

rel_rates1 <- splitntranspose(rel_pp, "chrate")
rel_comps1 <- splitntranspose(rel_pp, "chcomp")
rel_numbers1 <- splitntranspose(rel_pp, "chnum")

sev_rates1 <- splitntranspose(sev_pp, "chrate")
sev_comps1 <- splitntranspose(sev_pp, "chcomp")
sev_numbers1 <- splitntranspose(sev_pp, "chnum")

rel_rates2 <- splitntranspose(rel_ch, "chrate") %>% filter(groupingvar != "All")
rel_comps2 <- splitntranspose(rel_ch, "chcomp") %>% filter(groupingvar != "All")
rel_numbers2 <- splitntranspose(rel_ch, "chnum") %>% filter(groupingvar != "All")

sev_rates2 <- splitntranspose(sev_ch, "chrate") %>% filter(groupingvar != "All")
sev_comps2 <- splitntranspose(sev_ch, "chcomp") %>% filter(groupingvar != "All")
sev_numbers2 <- splitntranspose(sev_ch, "chnum") %>% filter(groupingvar != "All")

rel_rates3 <- splitntranspose(rel_ad, "chrate") %>% filter(groupingvar != "All")
rel_comps3 <- splitntranspose(rel_ad, "chcomp") %>% filter(groupingvar != "All")
rel_numbers3 <- splitntranspose(rel_ad, "chnum")  %>% filter(groupingvar != "All")

sev_rates3 <- splitntranspose(sev_ad, "chrate") %>% filter(groupingvar != "All")
sev_comps3 <- splitntranspose(sev_ad, "chcomp") %>% filter(groupingvar != "All")
sev_numbers3 <- splitntranspose(sev_ad, "chnum")  %>% filter(groupingvar != "All")

sample1 <- splitntranspose(rel_pp, "groupsample_ch")
sample2 <- splitntranspose(rel_ch, "groupsample_ch") %>% filter(groupingvar != "All")
sample3 <- splitntranspose(rel_ad, "groupsample_ch") %>% filter(groupingvar != "All")

sample <- rbind(sample1, sample2, sample3)

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)


# mark time series break
rel_numbers[, 12] <- "--"
rel_numbers[, 13] <- "--"
sev_numbers[, 12] <- "--"
sev_numbers[, 13] <- "--"


# put all input for the spreadsheet into a list
data <- list(sheetname = "Disability ch",
             title_a = "A. Proportion of children in poverty and severe poverty by whether there is a disabled person in the household",
             title_b = "B. Composition of children in poverty and severe poverty by whether there is a disabled person in the household",
             title_c = "C. Number of children in poverty and severe poverty by whether there is a disabled person in the household",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "3. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "4. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "5. Data on disability is available from 1995/96.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel_pp, rel_ch, rel_ad, rel_rates, rel_rates1, rel_rates2, rel_rates3,
       rel_comps, rel_comps1, rel_comps2, rel_comps3,
       rel_numbers, rel_numbers1, rel_numbers2, rel_numbers3,
       sev_pp, sev_ch, sev_ad, sev_rates, sev_rates1, sev_rates2, sev_rates3,
       sev_comps, sev_comps1, sev_comps2, sev_comps3,
       sev_numbers, sev_numbers1, sev_numbers2, sev_numbers3)


# Disability with benefits removed from income -------------------------------------------------

rel_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low50ahc_dis", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

rel_rates1 <- splitntranspose(rel_pp, "chrate")
rel_comps1 <- splitntranspose(rel_pp, "chcomp")
rel_numbers1 <- splitntranspose(rel_pp, "chnum")

sev_rates1 <- splitntranspose(sev_pp, "chrate")
sev_comps1 <- splitntranspose(sev_pp, "chcomp")
sev_numbers1 <- splitntranspose(sev_pp, "chnum")

rel_rates2 <- splitntranspose(rel_ch, "chrate") %>% filter(groupingvar != "All")
rel_comps2 <- splitntranspose(rel_ch, "chcomp") %>% filter(groupingvar != "All")
rel_numbers2 <- splitntranspose(rel_ch, "chnum") %>% filter(groupingvar != "All")

sev_rates2 <- splitntranspose(sev_ch, "chrate") %>% filter(groupingvar != "All")
sev_comps2 <- splitntranspose(sev_ch, "chcomp") %>% filter(groupingvar != "All")
sev_numbers2 <- splitntranspose(sev_ch, "chnum") %>% filter(groupingvar != "All")

rel_rates3 <- splitntranspose(rel_ad, "chrate") %>% filter(groupingvar != "All")
rel_comps3 <- splitntranspose(rel_ad, "chcomp") %>% filter(groupingvar != "All")
rel_numbers3 <- splitntranspose(rel_ad, "chnum")  %>% filter(groupingvar != "All")

sev_rates3 <- splitntranspose(sev_ad, "chrate") %>% filter(groupingvar != "All")
sev_comps3 <- splitntranspose(sev_ad, "chcomp") %>% filter(groupingvar != "All")
sev_numbers3 <- splitntranspose(sev_ad, "chnum")  %>% filter(groupingvar != "All")

sample1 <- splitntranspose(rel_pp, "groupsample_ch")
sample2 <- splitntranspose(rel_ch, "groupsample_ch") %>% filter(groupingvar != "All")
sample3 <- splitntranspose(rel_ad, "groupsample_ch") %>% filter(groupingvar != "All")

sample <- rbind(sample1, sample2, sample3)

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)

# mark time series break
rel_numbers[, 12] <- "--"
rel_numbers[, 13] <- "--"
sev_numbers[, 12] <- "--"
sev_numbers[, 13] <- "--"


# put all input for the spreadsheet into a list
data <- list(sheetname = "DisabilityBenefitsRemovedch",
             title_a = "A. Proportion of children in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_b = "B. Composition of children in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_c = "C. Number of children in poverty and severe poverty by whether there is a disabled person in the household, with disability benefits removed from household income",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income from Disability Living Allowance (DLA), Attendance Allowance (AA), and Personal Independence Payments (PIP) have been excluded from income.",
                           "This income is related to additional living costs associated with a disability.",
                           "2. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "4. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "5. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "6. Data on disability is available from 1995/96.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel_pp, rel_ch, rel_ad, rel_rates, rel_rates1, rel_rates2, rel_rates3,
       rel_comps, rel_comps1, rel_comps2, rel_comps3,
       rel_numbers, rel_numbers1, rel_numbers2, rel_numbers3,
       sev_pp, sev_ch, sev_ad, sev_rates, sev_rates1, sev_rates2, sev_rates3,
       sev_comps, sev_comps1, sev_comps2, sev_comps3,
       sev_numbers, sev_numbers1, sev_numbers2, sev_numbers3)

# Ethnicity (5-year average) ------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# remove composition data - not reliable for ethnicity
rel_comps[, 2] <- "--"
sev_comps[, 2] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Ethnicity 5-yr ch",
             title_a = "A. Proportion of children in poverty and severe poverty by ethnic group (five-year averages)",
             title_b = "B. Composition of children in poverty and severe poverty by ethnic group (five-year averages)",
             title_c = "C. Number of children in poverty and severe poverty by ethnic group (five-year averages)",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of families with children in each category in the five-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", period5yr),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "However, there are three-year averages available for a less detailed breakdown",
                           "2. Ethnicity data relates to all people in a household and is based on the ethnicity of the adult with the highest income.",
                           "3. Different ethnic groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. Table B (composition) is not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. A time series is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "6. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)



# Ethnicity (3-year average) ------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ethgrphh_2f")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = periods[length(periods)]) %>%
  samplesizecheck %>%
  mutate_at(vars(contains("sample")), comma_format(1))

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ethgrphh_2f")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = periods[length(periods)]) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

sample <- splitntranspose(rel, "groupsample_ch")

rel_rates <- splitntranspose(rel, "chrate")
rel_comps <- splitntranspose(rel, "chcomp")
rel_numbers <- splitntranspose(rel, "chnum")

sev_rates <- splitntranspose(sev, "chrate")
sev_comps <- splitntranspose(sev, "chcomp")
sev_numbers <- splitntranspose(sev, "chnum")

# remove composition data - not reliable for ethnicity
rel_comps[, 2] <- "--"
sev_comps[, 2] <- "--"

# put all input for the spreadsheet into a list
data <- list(sheetname = "Ethnicity 3-yr ch",
             title_a = "A. Proportion of children in poverty and severe poverty by ethnic group",
             title_b = "B. Composition of children in poverty and severe poverty by ethnic group",
             title_c = "C. Number of families with children in poverty and severe poverty by ethnic group",
             title_d = "D. Sample sizes",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             df7 = sample,
             filename = filename,
             subtitle_a = "Proportion of children in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of children in poverty who are in each category, Scotland",
             subtitle_c = "Number of children in each category who are in poverty, Scotland",
             subtitle_d = "Number of children in each category in the three-year survey sample, Scotland",
             subsubtitle_rel = "Children in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Children in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", as.character(periods[length(periods)])),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Ethnicity data relates to all people in a household and is based on the ethnicity of the adult with the highest income.",
                           "2. Different ethnic groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "3. Table B (composition) is not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "4. A time series is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "5. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create new worksheet
createWideSpreadsheet(data)

# D 4. Income ----
createSepsheet(filename = filename, sheetname = "- 4 -",
               text = "4 - Income and income inequality")

# Median BHC household income by age group ---------------------------------------

df <- do.call(rbind.data.frame, lapply(hbai, getmediansbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = periods)) %>%
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
                        labels = periods)) %>%
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
                        labels = periods)) %>%
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
                        labels = periods)) %>%
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
                        labels = periods))

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
                        labels = periods))

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
                        labels = periods),
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
                        labels = periods),
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
                        labels = periods),
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
                        labels = periods),
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
latesthbai_2 <- hbai[[length(labels$years[[1]]) - 1]]
latesthbai_3 <- hbai[[length(labels$years[[1]]) - 2]]

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
             subtitle = "Income in £, after tax and transfers, and in 2018/19 prices, Scotland 2016-19",
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
             subtitle = "Income in £, after tax and transfers, and in 2018/19 prices, Scotland 2016-19",
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
df2 <- getsources(hbai[[length(labels$years[[1]]) - 1]])
df3 <- getsources(hbai[[length(labels$years[[1]]) - 2]])

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
             subtitle = "Gross household income by income type as a share of total income, Scotland 2016-19",
             headers = c(" ", "Earnings", "Social security payments", "Occupational pensions", "Investments", "Other sources"),
             uberheaders =  NULL,
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income here is gross income before housing costs. Income deciles are based on equivalised net income before housing costs.",
                           "2. Due to volatility of the data, single-year estimates of these statistics are not available."))

# Create spreadsheet and new worksheet
createSpreadsheet(data)

# TOC ------------------------------------------------------------------------------

createContentSheet(paste0("output/", filename))

rm(list = ls())
