# Create spreadsheet for 3-year averaged data

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "output/All three-year average.xlsx"

hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland")

adult <- readRDS("data/tidyadult.rds") %>%
  mutate(yearn = factor(year,
                        levels = labels$years$years,
                        labels = labels$years$numbered,
                        ordered = TRUE)) %>%
  filter(gvtregn == "Scotland")

## 9 Family type (adults) ------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "newfambu", weight = "gs_newad") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

sev <- getpovby(hbai, pov = "low50ahc", by = "newfambu", weight = "gs_newad") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "9 Family type",
             sheettitle = "9. Family type",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of adults in relative poverty",
                         "Composition of adults in relative poverty",
                         "Number of adults in relative poverty",
                         "Proportion of adults in severe poverty",
                         "Composition of adults in severe poverty",
                         "Number of adults in severe poverty",
                         "Sample sizes" ),
             subtitles = c("Proportion of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                            "Proportion of adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                            "Number of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                            "Proportion of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                            "Proportion of adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                            "Number of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                            "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
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

## 10 Number of children in household ------------------------------------------

rel <- getpovby(hbai, by = "depchldh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods),
         groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(hbai, pov = "low50ahc", by = "depchldh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods),
         groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "10 Children",
             sheettitle = "10. Number of children in the household",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. Previously, we reported on the number of children in the family, not the household.",
                           "Therefore, these estimates are not directly comparable with the estimates reported in previous publications.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 11 Family economic status (working-age adults) ------------------------------

rel <- getpovby(hbai, by = "ecobu", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "ecobu", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 5)

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "11 Economic",
             sheettitle = "11. Family economic status",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of working-age adults in relative poverty",
                        "Composition of working-age adults in relative poverty",
                        "Number of working-age adults in relative poverty",
                        "Proportion of working-age adults in severe poverty",
                        "Composition of working-age adults in severe poverty",
                        "Number of working-age adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of working-age adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of working-age adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of working-age adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families with working-age adults in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "4. The term 'family' here refers to the core family in a household, consisting of one or two adults and their dependent children if any.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 12 Household work status (working-age adults) -------------------------------

rel <- getpovby(hbai, by = "workinghh", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "workinghh", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 5)

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "12 Work",
             sheettitle = "12. Household work status",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of working-age adults in relative poverty",
                        "Composition of working-age adults in relative poverty",
                        "Number of working-age adults in relative poverty",
                        "Proportion of working-age adults in severe poverty",
                        "Composition of working-age adults in severe poverty",
                        "Number of working-age adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of working-age adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of working-age adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of working-age adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of working-age adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families with working-age adults in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. Information on economic status is not available prior to 1996.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size"))

# Create new worksheet
createWideSpreadsheet(data)

## 13 Tenure -------------------------------------------------------------------

rel <- getpovby(hbai, by = "tenhbai") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 12)

sev <- getpovby(hbai, pov = "low50ahc", by = "tenhbai") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 12)

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "13 Tenure",
             sheettitle = "13. Housing tenure",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. Information on housing tenure is not available prior to 2003.",
                           "3. Due to a single very large household in the sample in the 'Owned outright' category in 2017/18, the latest estimates are significantly higher than those in previous years.",
                           "However, further data points are required to confirm whether this marks an increasing trend in poverty."))

# Create new worksheet
createWideSpreadsheet(data)

## 14 Urban/rural class ------------------------------------------------------------

rel <- getpovby(hbai, by = "urinds") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 15)

sev <- getpovby(hbai, pov = "low50ahc", by = "urinds") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 15)

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "14 Urbrur",
             sheettitle = "14. Urban and rural areas",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. Information on urban/rural class is not available prior to 2006."))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 15 Age (adults) -----------------------------------------------------------------

rel <- getpovby(adult, by = "ageband", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods),
         groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(adult, pov = "low50ahc", by = "ageband", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods),
         groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "15 Age",
             sheettitle = "15. Adult age",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of adults in relative poverty",
                        "Composition of adults in relative poverty",
                        "Number of adults in relative poverty",
                        "Proportion of adults in severe poverty",
                        "Composition of adults in severe poverty",
                        "Number of adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of adults in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 16 Gender (adults) ----------------------------------------------------------

rel <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(by = "singlehh", weight = "gs_newad") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

sev <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "singlehh", weight = "gs_newad") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))


sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "16 Gender",
             sheettitle = "16. Gender of single adults",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of single adults in relative poverty",
                        "Composition of single adults in relative poverty",
                        "Number of single adults in relative poverty",
                        "Proportion of single adults in severe poverty",
                        "Composition of single adults in severe poverty",
                        "Number of single adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of single adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of single adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of single adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of single adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of single adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of single adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of single adults in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. The term 'single' here refers to adults who are sharing a household with no other adults.",
                           "This differs from the analysis in the 'Family type' worksheet, where single adults may share the household with other families.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 17 Marital status (adults) --------------------------------------------------

rel <- getpovby(adult, by = "marital", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

sev <- getpovby(adult, pov = "low50ahc", by = "marital", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_comps <- splitntranspose(rel, "composition")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_comps <- splitntranspose(sev, "composition")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "17 Marital",
             sheettitle = "17. Marital status",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of adults in relative poverty",
                        "Composition of adults in relative poverty",
                        "Number of adults in relative poverty",
                        "Proportion of adults in severe poverty",
                        "Composition of adults in severe poverty",
                        "Number of adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of adults in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of adults in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of adults in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size",
                           "3. 'Single' refers to adults who have never been married or in a civil partnership, and are not living with a partner.",
                           "4. 'Separated' refers to adults who are married or in a civil partnership, but are not living together because of estrangement.",
                           "5. 'Married / Civil Partnership' includes couples who are temporarily living apart (e.g. due to serving in the armed forces).",
                           "6. 'Adults' includes working-age adults and pensioners.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 18 Disability ------------------------------------------------------------------

rel_pp <- getpovby(hbai, by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_pp <- getpovby(hbai, pov = "low50ahc", by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

rel_ch <- getpovby(hbai, by = "disch_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_ch <- getpovby(hbai, pov = "low50ahc", by = "disch_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

rel_ad <- getpovby(hbai, by = "disad_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_ad <- getpovby(hbai, pov = "low50ahc", by = "disad_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sample1 <- splitntranspose(rel_pp, "sample")
sample2 <- splitntranspose(rel_ch, "sample") %>% filter(Group != "All")
sample3 <- splitntranspose(rel_ad, "sample") %>% filter(Group != "All")

rel_rates1 <- splitntranspose(rel_pp, "rate")
rel_comps1 <- splitntranspose(rel_pp, "composition")
rel_numbers1 <- splitntranspose(rel_pp, "number")

sev_rates1 <- splitntranspose(sev_pp, "rate")
sev_comps1 <- splitntranspose(sev_pp, "composition")
sev_numbers1 <- splitntranspose(sev_pp, "number")

rel_rates2 <- splitntranspose(rel_ch, "rate") %>% filter(Group != "All")
rel_comps2 <- splitntranspose(rel_ch, "composition") %>% filter(Group != "All")
rel_numbers2 <- splitntranspose(rel_ch, "number") %>% filter(Group != "All")

sev_rates2 <- splitntranspose(sev_ch, "rate") %>% filter(Group != "All")
sev_comps2 <- splitntranspose(sev_ch, "composition") %>% filter(Group != "All")
sev_numbers2 <- splitntranspose(sev_ch, "number") %>% filter(Group != "All")

rel_rates3 <- splitntranspose(rel_ad, "rate") %>% filter(Group != "All")
rel_comps3 <- splitntranspose(rel_ad, "composition") %>% filter(Group != "All")
rel_numbers3 <- splitntranspose(rel_ad, "number")  %>% filter(Group != "All")

sev_rates3 <- splitntranspose(sev_ad, "rate") %>% filter(Group != "All")
sev_comps3 <- splitntranspose(sev_ad, "composition") %>% filter(Group != "All")
sev_numbers3 <- splitntranspose(sev_ad, "number")  %>% filter(Group != "All")

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)

sample <- rbind(sample1, sample2, sample3)

data <- list(sheet = "18 Disability",
             sheettitle = "18. Disabled household members",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "3. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "4. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "5. Previously, we reported on disabled family members, not household members.",
                           "Therefore, these estimates are not directly comparable with the estimates reported in previous publications.",
                           "6. Data on disabled children is available from 1995/96."))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

# mark missing data ("--")
mark_missing(data, ncols = 2, nrows = 6, xlscol = 19, xlsrow = 35)
mark_missing(data, ncols = 2, nrows = 6, xlscol = 19, xlsrow = 74)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 11)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 24)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 37)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 50)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 63)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 76)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 89)

## 19 Disability with benefits removed from income --------------------------------

rel_pp <- getpovby(hbai, pov = "low60ahc_dis", by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_pp <- getpovby(hbai, pov = "low50ahc_dis", by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

rel_ch <- getpovby(hbai, pov = "low60ahc_dis", by = "disch_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_ch <- getpovby(hbai, pov = "low50ahc_dis", by = "disch_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

rel_ad <- getpovby(hbai, pov = "low60ahc_dis", by = "disad_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sev_ad <- getpovby(hbai, pov = "low50ahc_dis", by = "disad_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  filter(yearn >= 3)

sample1 <- splitntranspose(rel_pp, "sample")
sample2 <- splitntranspose(rel_ch, "sample") %>% filter(Group != "All")
sample3 <- splitntranspose(rel_ad, "sample") %>% filter(Group != "All")

rel_rates1 <- splitntranspose(rel_pp, "rate")
rel_comps1 <- splitntranspose(rel_pp, "composition")
rel_numbers1 <- splitntranspose(rel_pp, "number")

sev_rates1 <- splitntranspose(sev_pp, "rate")
sev_comps1 <- splitntranspose(sev_pp, "composition")
sev_numbers1 <- splitntranspose(sev_pp, "number")

rel_rates2 <- splitntranspose(rel_ch, "rate") %>% filter(Group != "All")
rel_comps2 <- splitntranspose(rel_ch, "composition") %>% filter(Group != "All")
rel_numbers2 <- splitntranspose(rel_ch, "number") %>% filter(Group != "All")

sev_rates2 <- splitntranspose(sev_ch, "rate") %>% filter(Group != "All")
sev_comps2 <- splitntranspose(sev_ch, "composition") %>% filter(Group != "All")
sev_numbers2 <- splitntranspose(sev_ch, "number") %>% filter(Group != "All")

rel_rates3 <- splitntranspose(rel_ad, "rate") %>% filter(Group != "All")
rel_comps3 <- splitntranspose(rel_ad, "composition") %>% filter(Group != "All")
rel_numbers3 <- splitntranspose(rel_ad, "number")  %>% filter(Group != "All")

sev_rates3 <- splitntranspose(sev_ad, "rate") %>% filter(Group != "All")
sev_comps3 <- splitntranspose(sev_ad, "composition") %>% filter(Group != "All")
sev_numbers3 <- splitntranspose(sev_ad, "number")  %>% filter(Group != "All")

rel_rates <- rbind(rel_rates1, rel_rates2, rel_rates3)
rel_comps <- rbind(rel_comps1, rel_comps2, rel_comps3)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3)

sev_rates <- rbind(sev_rates1, sev_rates2, sev_rates3)
sev_comps <- rbind(sev_comps1, sev_comps2, sev_comps3)
sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3)

sample <- rbind(sample1, sample2, sample3)

data <- list(sheet = "19 Disability adj",
             sheettitle = "19. Disabled household members, disability benefits removed from household income",
             file = filename,
             dfs = list(rel_rates, rel_comps, rel_numbers,
                        sev_rates, sev_comps, sev_numbers, sample),
             formats = c("pct", "pct", "num", "pct", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each category, Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income from Disability Living Allowance (DLA), Attendance Allowance (AA), and Personal Independence Payments (PIP) have been excluded from income.",
                           "This income is related to additional living costs associated with a disability.",
                           "2. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) ",
                           "4. The way in which information on disabled people is collected changed several times during this timeseries.",
                           "This causes breaks in the timeseries between 2001/02 and 2002/03, between 2003/04 and 2004/05, and between 2011/12 and 2012/13.",
                           "Since 2012/13, disabled people are identified as those who report any physical or mental health condition(s) or illness(es) that last or are expected to last 12 months or more, and which limit their ability to carry out day-to-day activities.",
                           "Therefore, care needs to be taken when considering long-term trends.",
                           "5. Since the last break in the methodology caused a large change in the size of the disabled population, the estimated numbers in poverty before and after the break cannot be directly compared and no three-year averaged data is available during the break.",
                           "6. Previously, we reported on disabled family members, not household members.",
                           "Therefore, these estimates are not directly comparable with the estimates reported in previous publications.",
                           "7. Data on disabled children is available from 1995/96."))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

# mark missing data ("--")
mark_missing(data, ncols = 2, nrows = 6, xlscol = 19, xlsrow = 35)
mark_missing(data, ncols = 2, nrows = 6, xlscol = 19, xlsrow = 74)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 11)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 24)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 37)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 50)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 63)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 76)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 3, xlsrow = 89)

## 20 Ethnicity ----------------------------------------------------------------

rel <- getpovby(hbai, by = "ethgrphh") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  filter(yearn == max(yearn)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

sev <- getpovby(hbai, pov = "low50ahc", by = "ethgrphh") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  filter(yearn == max(yearn)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_numbers <- splitntranspose(sev, "number")

data <- list(sheet = "20 Ethnic5",
             sheettitle = "20. Ethnicity - detailed breakdown (5-year average)",
             file = filename,
             dfs = list(rel_rates, rel_numbers,
                        sev_rates,  sev_numbers, sample),
             formats = c("pct", "num", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of people in relative poverty",
                        "Number of people in relative poverty",
                        "Proportion of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Number of people in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of people in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each category in the combined five-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. Ethnicity data relates to all people in a household and is based on the ethnicity of the adult with the highest income.",
                           "3. Different ethnic groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. The Composition tables are not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. A time series is not available. This is because ethnic composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "6. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)"))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 21 Religion (adults) --------------------------------------------------------

rel <- getpovby(adult, by = "religsc", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr),
         groupingvar = factor(groupingvar,
                              levels = c("All",
                                         "Church of Scotland",
                                         "Roman Catholic",
                                         "Other Christian",
                                         "Muslim",
                                         "Other religion",
                                         "No religion"))) %>%
  filter(yearn == max(yearn))

sev <- getpovby(adult, pov = "low50ahc", by = "religsc", weight = "adultwgt") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr),
         groupingvar = factor(groupingvar,
                              levels = c("All",
                                         "Church of Scotland",
                                         "Roman Catholic",
                                         "Other Christian",
                                         "Muslim",
                                         "Other religion",
                                         "No religion"))) %>%
  filter(yearn == max(yearn))

sample <- splitntranspose(rel, "sample")

rel_rates <- splitntranspose(rel, "rate")
rel_numbers <- splitntranspose(rel, "number")

sev_rates <- splitntranspose(sev, "rate")
sev_numbers <- splitntranspose(sev, "number")

# put all input for the spreadsheet into a list
data <- list(sheet = "21 Religion5",
             sheettitle = "21. Religion of adults - detailed breakdown (5-year average)",
             file = filename,
             dfs = list(rel_rates, rel_numbers,
                        sev_rates, sev_numbers, sample),
             formats = c("pct", "num", "pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Proportion of adults in relative poverty",
                        "Number of adults in relative poverty",
                        "Proportion of adults in severe poverty",
                        "Number of adults in severe poverty",
                        "Sample sizes" ),
             subtitles = c("Proportion of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Number of adults in each category who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of adults in each category who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of adults in each category in the combined five-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Due to sample sizes, three-year averages of these statistics are not available.",
                           "2. In these tables, 'adults' include working-age adults as well as pensioners.",
                           "3. Different religious groups have been combined into one for this analysis, as sample sizes are too small to reliably report on individual groups.",
                           "4. The Composition tables are not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, estimates of the composition are not reliable.",
                           "5. A time series is not available. This is because religious composition of the population is not accounted for in the survey weighting process, and therefore, poverty estimates are volatile and apparent trends not reliable.",
                           "6. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.) "))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

# 22 Food security -------------------------------------------------------------

total <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "All") %>%
  ungroup() %>%
  mutate(number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition))

total_comp <- select(total, Group, foodsec, composition) %>%
  spread(foodsec, composition)
total_num <- select(total, Group, foodsec, number) %>%
  spread(foodsec, number)
total_sample <- select(total, Group, sample)[1, ]

relpov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahc == 1) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In relative poverty after housing costs") %>%
  ungroup() %>%
  mutate(number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition))

relpov_comp <- select(relpov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
relpov_num <- select(relpov, Group, foodsec, number) %>%
  spread(foodsec, number)
relpov_sample <- select(relpov, Group, sample)[1, ]

abspov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahcabs == 1) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In absolute poverty after housing costs") %>%
  ungroup() %>%
  mutate(number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition))

abspov_comp <- select(abspov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
abspov_num <- select(abspov, Group, foodsec, number) %>%
  spread(foodsec, number)
abspov_sample  <- select(abspov, Group, sample)[1, ]

comps <- rbind(total_comp, relpov_comp, abspov_comp)
numbers <- rbind(total_num, relpov_num, abspov_num)
sample <- rbind(total_sample, relpov_sample, abspov_sample) %>%
  rename(Sample = sample)

data <- list(sheet = "22 Food security",
             sheettitle = paste0("22. Household food security levels (", max(levels(labels$years$formatted)), ")"),
             file = filename,
             dfs = list(comps, numbers, sample),
             formats = c("pct", "num", "num"),
             totalrow = TRUE,
             titles = c("Composition of people with various levels of food security",
                        "Number of people with various levels of food security",
                        "Sample sizes" ),
             subtitles = c(paste0("Proportion of people in each household food security category, Scotland ", max(levels(labels$years$formatted))),
                           paste0("Number of people in each household food security category, Scotland ", max(levels(labels$years$formatted))),
                           paste0("Number of families in each group in the survey sample, Scotland ", max(levels(labels$years$formatted)))),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Food security questions were newly added to the Family Resources Survey in 2019/20.",
                           "Therefore, only a single year estimate is available.",
                           "The questions ask about whether people were worried about running out of food, had to reduce meal sizes or skip meals.",
                           "2. Shared households such as such as a group of students, or other unrelated adults were excluded from the analysis",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..'   not available due to small sample size")
)

# Create new worksheet
createWideSpreadsheet(data)


rm(list = ls())
