# Create spreadsheet for 3-year averaged data

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "output/All three-year average.xlsx"

hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland")

## 1 Relative AHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60ahc", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low60ahc", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low60ahc", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahc", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "1 Rel AHC",
             sheettitle = "1. Relative poverty (after housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = "yes",
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 2 Absolute AHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60ahcabs", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low60ahcabs", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low60ahcabs", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahcabs", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "2 Abs AHC",
             sheettitle = "2. Absolute poverty (after housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = TRUE,
             titles = c("Proportion of people in absolute poverty",
                        "Composition of people in absolute poverty",
                        "Number of people in absolute poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in absolute poverty (below 60% of the 2010/11 UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 60% of the 2010/11 UK median income after housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in severe poverty (below 60% of the 2010/11 UK median income after housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 3 Severe AHC ----------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low50ahc", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low50ahc", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low50ahc", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahc", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "3 Sev AHC",
             sheettitle = "3. Severe poverty (after housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = "yes",
             titles = c("Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 4 Relative BHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60bhc", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low60bhc", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low60bhc", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhc", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "4 Rel BHC",
             sheettitle = "4. Relative poverty (before housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = "yes",
             titles = c("Proportion of people in relative poverty",
                        "Composition of people in relative poverty",
                        "Number of people in relative poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in relative poverty (below 60% of UK median income before housing costs), Scotland",
                           "Proportion of people in relative poverty (below 60% of UK median income before housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in relative poverty (below 60% of UK median income before housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)
# Create worksheet
createWideSpreadsheet(data)

## 5 Absolute BHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60bhcabs", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low60bhcabs", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low60bhcabs", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhcabs", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "5 Abs BHC",
             sheettitle = "5. Absolute poverty (before housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = "yes",
             titles = c("Proportion of people in absolute poverty",
                        "Composition of people in absolute poverty",
                        "Number of people in absolute poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in absolute poverty (below 60% of the 2010/11 UK median income before housing costs), Scotland",
                           "Proportion of people in severe poverty (below 60% of the 2010/11 UK median income before housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in severe poverty (below 60% of the 2010/11 UK median income before housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 6 Severe BHC ----------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low50bhc", threeyr = TRUE)[["rates"]]
comps <- getheadlines(hbai, pov = "low50bhc", threeyr = TRUE)[["comps"]]
numbers <- getheadlines(hbai, pov = "low50bhc", threeyr = TRUE)[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhc", threeyr = TRUE)[["sample"]]

data <- list(file = filename,
             sheet = "6 Sev BHC",
             sheettitle = "6. Severe poverty (before housing costs)",
             dfs = list(rates, comps, numbers, sample),
             formats = c("pct", "pct", "pop", "pop"),
             totalrow = "yes",
             titles = c("Proportion of people in severe poverty",
                        "Composition of people in severe poverty",
                        "Number of people in severe poverty",
                        "Sample sizes"),
             subtitles = c("Proportion of people in each group who are in severe poverty (below 50% of UK median income before housing costs), Scotland",
                           "Proportion of people in severe poverty (below 50% of UK median income before housing costs) who are in each group, Scotland",
                           "Number of people in each group who are in severe poverty (below 50% of UK median income before housing costs), Scotland",
                           "Number of families in each group in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create worksheet
createWideSpreadsheet(data)

## 7 Child material deprivation ------------------------------------------------

cmd_ahc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_ahc_new <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdahc_new", weight = "gs_newch") %>%
  rbind(cmd_ahc) %>%
  mutate(Measure = "New measure, after housing costs")
cmd_ahc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, after housing costs") %>%
  rbind(cmd_ahc_new) %>%
  group_by(Measure) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd_bhc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_bhc_new <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdbhc_new", weight = "gs_newch") %>%
  rbind(cmd_bhc) %>%
  mutate(Measure = "New measure, before housing costs")
cmd_bhc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, before housing costs") %>%
  rbind(cmd_bhc_new) %>%
  group_by(Measure) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd <- rbind(cmd_ahc, cmd_bhc)  %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

rates <- cmd  %>%
  select(year, rate, Measure) %>%
  spread(year, rate)
numbers <- cmd %>%
  select(year, number, Measure) %>%
  spread(year, number)
sample <- cmd %>%
  select(year, sample, Measure) %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  spread(year, sample) %>%
  mutate(Measure = ifelse(Measure == "Old measure, after housing costs",
                          "Old measure", "New measure"))

data <- list(file = filename,
             sheet = "7 Child dep",
             sheettitle = "7. Children's combined low income and material deprivation",
             dfs = list(rates, numbers, sample),
             formats = c("pct", "num", "num"),
             titles = c("Proportion of children in combined low income and material deprivation",
                        "Number of children in combined low income and material deprivation",
                        "Sample sizes"),
             subtitles = c("Proportion of children who are in combined low income (below 70% of UK median income) and material deprivation, Scotland",
                           "Number of children who are in combined low income (below 70% of UK median income) and material deprivation, Scotland",
                           "Number of families with children in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers tables may, in some cases, be largely explained by rounding.",
                           "2. The definition of material deprivation changed in 2010/11, creating a break in the time series.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

mark_missing(data, ncols = 6, nrows = 2, xlscol = 3, xlsrow = 8)
mark_missing(data, ncols = 6, nrows = 2, xlscol = 3, xlsrow = 18)
mark_missing(data, ncols = 6, nrows = 1, xlscol = 3, xlsrow = 28)
mark_missing(data, ncols = 8, nrows = 2, xlscol = 8, xlsrow = 10)
mark_missing(data, ncols = 8, nrows = 2, xlscol = 8, xlsrow = 20)
mark_missing(data, ncols = 8, nrows = 1, xlscol = 8, xlsrow = 29)

## 8 Pensioner deprivation -----------------------------------------------------

pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older",
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods))

rates <- pmd %>%
  select(Group, year, rate) %>%
  spread(year, rate)

numbers <- pmd %>%
  select(Group, year, number) %>%
  spread(year, number)

sample <- pmd  %>%
  select(Group, year, sample) %>%
  spread(year, sample)

data <- list(file = filename,
             sheet = "8 Pen dep",
             sheettitle = "8. Pensioners material deprivation",
             dfs = list(rates, numbers, sample),
             formats = c("pct", "num", "num"),
             titles = c("Proportion of pensioners in material deprivation",
                        "Number of pensioners in material deprivation",
                        "Sample sizes"),
             subtitles = c("Proportion of pensioners aged 65 and over who have limited access to goods and services, Scotland",
                           "Number of pensioners aged 65 and over who have limited access to goods and services, Scotland",
                           "Number of families with pensioners aged 65 and over in the combined three-year survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Pensioner material deprivation is calculated for pensioners aged 65 or over.",
                           "2. Pensioner material deprivation is different to other measures of poverty, including the child low income and material deprivation measure",
                           "in that it is not associated with an income threshold. It captures issues such as whether poor health, disability and social isolation",
                           "prevent access to goods and services, rather than solely low income.",
                           "3. In the tables, the following conventions have been used where figures are unavailable:",
                           "'..' not available due to small sample size",
                           "'--' not available for another reason (data accuracy, data wasn't collected etc.)")
)

# Create worksheet
createWideSpreadsheet(data)

