
# Create spreadsheet for characteristics of poverty

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

# Get formatted periods in vector; get total number of years/periods
periods <- labels[["years"]]$periods
yearsno <- length(periods)
period5yr <- as.character(labels[["years"]]$period5yr[length(periods)])

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

filename <- "Poverty characteristics.xlsx"

# Tenure ---------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
                      lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev <- do.call(rbind.data.frame,
                      lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ptentyp2")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0506" ) %>%
  mutate(years = factor(years, labels = periods[12:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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
             filename = filename,
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
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "urinds")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "0809" ) %>%
  mutate(years = factor(years, labels = periods[15:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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
             filename = filename,
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
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "depchldh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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
             filename = filename,
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

# Age (adults) -----------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low60ahc",
                      groupingvar = "ageband")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc",
                      groupingvar = "ageband")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad

# split dataset into rates, numbers, compositions and transpose

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

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[3:yearsno - 2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

# Family type (adults) -------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "newfambu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

rel_rates <- splitntranspose(rel, "adrate")
rel_comps <- splitntranspose(rel, "adcomp")
rel_numbers <- splitntranspose(rel, "adnum")

sev_rates <- splitntranspose(sev, "adrate")
sev_comps <- splitntranspose(sev, "adcomp")
sev_numbers <- splitntranspose(sev, "adnum")

# put all input for the spreadsheet into a list
data <- list(sheetname = "Family type",
             title_a = "A. Proportion of adults in poverty and severe poverty by family type and gender",
             title_b = "B. Composition of adults in poverty and severe poverty by family type and gender",
             title_c = "C. Number of adults in poverty and severe poverty by family type and gender",
             filename = filename,
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
  samplesizecheck_ad

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc", groupingvar = "marital")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  mutate(years = factor(years, labels = periods[3:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck_ad

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
             filename = filename,
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
                           "3. 'Single' refers to adults who have never been married or in a civil partnership, and are not living with a partner.",
                           "4. 'Separated' refers to adults who are married or in a civil partnership, but are not living together because of estrangement.",
                           "5. 'Married / Civil Partnership' includes couples who are temporarily living apart (e.g. due to serving in the armed forces).",
                           "6. 'Adults' includes working-age adults and pensioners.")
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
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ecobu")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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
             filename = filename,
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
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "workinghh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9899" ) %>%
  mutate(years = factor(years, labels = periods[5:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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
             filename = filename,
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

rel_pp <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size (fewer than 100)",
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


# Disability with benefits removed from income -------------------------------------------------

rel_pp <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

rel_ch <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "disch_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

rel_ad <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis", groupingvar = "disad_hh")) %>%
  addyearvar %>%
  formatpovby3yraverage %>%
  filter(years >= "9798" ) %>%
  mutate(years = factor(years, labels = periods[4:yearsno])) %>%
  arrange(years) %>%
  samplesizecheck

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

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", levels(periods)[4:yearsno-2]),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income from Disability Living Allowance (DLA), Attendance Allowance (AA), and Personal Independence Payments (PIP) have been excluded from income.",
                           "This income is related to additional living costs associated with a disability.",
                           "2. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in table C may, in some cases, be largely explained by rounding.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size (fewer than 100)",
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

# Ethnicity ------------------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc", groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck

sev <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low50ahc", groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck

# split dataset into rates, numbers, compositions and transpose

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

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = filename,
             subtitle_a = "Proportion of people in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of people in poverty who are in each category, Scotland",
             subtitle_c = "Number of people in each category who are in poverty, Scotland",
             subsubtitle_rel = "People in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "People in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", period5yr),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. Ethnicity data relates to all people in a household and is based on the ethnicity of the adult with the highest income.",
                           "3. Different ethnic groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. Table B (composition) is not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

remove(rel, rel_rates, rel_comps, rel_numbers,
       sev, sev_rates, sev_comps, sev_numbers)

# Religion (adults) ----------------------------------------------------------------------------

rel <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low60ahc", groupingvar = "religsc")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck_ad

sev <- do.call(rbind.data.frame,
               lapply(adult, getpovby_adult, povvar = "low50ahc", groupingvar = "religsc")) %>%
  addyearvar %>%
  formatpovby5yraverage %>%
  filter(years == max(years)) %>%
  mutate(years = period5yr) %>%
  samplesizecheck_ad

# split dataset into rates, numbers, compositions and transpose

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

             df1 = rel_rates,
             df2 = sev_rates,
             df3 = rel_comps,
             df4 = sev_comps,
             df5 = rel_numbers,
             df6 = sev_numbers,
             filename = filename,
             subtitle_a = "Proportion of adults in each category who are in poverty, Scotland",
             subtitle_b = "Proportion of adults in poverty who are in each category, Scotland",
             subtitle_c = "Number of adults in each category who are in poverty, Scotland",
             subsubtitle_rel = "Adults in relative poverty (below 60% of UK median income after housing costs)",
             subsubtitle_sev = "Adults in severe poverty (below 50% of UK median income after housing costs)",
             headers = c(" ", period5yr),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "3. Different religious groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. Table B (composition) is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size (fewer than 100)",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ")
)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

# TOC --------------------------------------------------------------------

createContentSheet(paste0("output/", filename))

rm(list = ls())












