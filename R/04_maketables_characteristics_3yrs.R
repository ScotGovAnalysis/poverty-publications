
# Create spreadsheet for characteristics of poverty

# Issue: factor levels in wrong order
# Issue: economic status - poverty rates wrong (category mix-up?)
# Issue: Find a clean way of assigning factor levels to tidy datasets

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

# Get formatted periods in vector; get total number of years/periods
periods <- labels[["years"]]$periods
yearsno <- length(periods)

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

# Tenure ---------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame, 
                      lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

sev <- do.call(rbind.data.frame, 
                      lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

# split dataset into rates, numbers, compositions and transpose

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
             
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = "Poverty characteristics.xlsx",
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[12:yearsno-2]),
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

# Urban/rural class ----------------------------------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "urinds"))%>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

# split dataset into rates, numbers, compositions and transpose

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
             
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = "Poverty characteristics.xlsx",
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[15:yearsno-2]),
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


# Number of children in household --------------------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "depchldh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "depchldh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  ppsamplesizecheck

# split dataset into rates, numbers, compositions and transpose

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
             
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = "Poverty characteristics.xlsx",
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers, 
       sev, sev_rates, sev_comps, sev_numbers)


# Family type (adults) -------------------------------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  adsamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  adsamplesizecheck

# split dataset into rates, numbers, compositions and transpose

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
             filename = "Poverty characteristics.xlsx",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "3. 'Pensioner couples' include working-age adults who are in a couple with a pensioner.",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers, 
       sev, sev_rates, sev_comps, sev_numbers)

# Marital status (adults) ----------------------------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(adult, getpovby_adult, povvar = "low60ahc", groupingvar = "marital")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  adsamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(adult, getpovby_adult, povvar = "low50ahc", groupingvar = "marital")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  adsamplesizecheck

# split dataset into rates, numbers, compositions and transpose

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
             filename = "Poverty characteristics.xlsx",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "3. 'Single' refers to adults who have never been married or in a civil partnership, and are not living with a partner",
                           "4. 'Separated' refers to adults who are married or in a civil partnership, but are not living together because of estrangement",
                           "5. 'Married/civil partnership' includes couples who are temporarily living apart (e.g. due to serving in the armed forces")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers, 
       sev, sev_rates, sev_comps, sev_numbers)

# Family economic status (working-age adults) -------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  wasamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  wasamplesizecheck

# rearrange categories
levels(rel$groupingvar) <- c("All", levels(labels[["economic"]]$labels)[1:7])
levels(sev$groupingvar) <- c("All", levels(labels[["economic"]]$labels)[1:7])

# split dataset into rates, numbers, compositions and transpose

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
             filename = "Poverty characteristics.xlsx",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             subtitle_a = "Proportion of working-age adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of working-age adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of working-age adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Working-age adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Working-age adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers, 
       sev, sev_rates, sev_comps, sev_numbers)


# Household work status (working-age adults) ---------------------------------------------------

rel <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  wasamplesizecheck

sev <- do.call(rbind.data.frame, 
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  wasamplesizecheck

# Rename categories
sev$groupingvar <- factor(sev$groupingvar, labels = c("All", "No one in paid work", "Someone in paid work"))
rel$groupingvar <- factor(rel$groupingvar, labels = c("All", "No one in paid work", "Someone in paid work"))

# split dataset into rates, numbers, compositions and transpose

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
             filename = "Poverty characteristics.xlsx",
             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             subtitle_a = "Proportion of working-age adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of working-age adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of working-age adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Working-age adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Working-age adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[5:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size (fewer than 100)")
)

# Create new worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers, 
       sev, sev_rates, sev_comps, sev_numbers)


# Disability -----------------------------------------------------------------------------------
# Disability with benefits removed from income -------------------------------------------------
# Ethnicity ------------------------------------------------------------------------------------
# Religion (adults) ----------------------------------------------------------------------------













