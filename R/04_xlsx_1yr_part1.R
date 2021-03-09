
# Create spreadsheet for single-year data - part 1

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "output/All single year.xlsx"

hbai <- readRDS("data/tidyhbai.rds") %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  filter(gvtregn == "Scotland")

## 1 Relative AHC --------------------------------------------------------------

rates <-  getheadlines(hbai, pov = "low60ahc")[["rates"]]
comps <- getheadlines(hbai, pov = "low60ahc")[["comps"]]
numbers <- getheadlines(hbai, pov = "low60ahc")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahc")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 2 Absolute AHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60ahcabs")[["rates"]]
comps <- getheadlines(hbai, pov = "low60ahcabs")[["comps"]]
numbers <- getheadlines(hbai, pov = "low60ahcabs")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahcabs")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 3 Severe AHC ----------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low50ahc")[["rates"]]
comps <- getheadlines(hbai, pov = "low50ahc")[["comps"]]
numbers <- getheadlines(hbai, pov = "low50ahc")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60ahc")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create new worksheet
createWideSpreadsheet(data)

## 4 Relative BHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60bhc")[["rates"]]
comps <- getheadlines(hbai, pov = "low60bhc")[["comps"]]
numbers <- getheadlines(hbai, pov = "low60bhc")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhc")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)
# Create worksheet
createWideSpreadsheet(data)

## 5 Absolute BHC --------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low60bhcabs")[["rates"]]
comps <- getheadlines(hbai, pov = "low60bhcabs")[["comps"]]
numbers <- getheadlines(hbai, pov = "low60bhcabs")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhcabs")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 6 Severe BHC ----------------------------------------------------------------
rates <- getheadlines(hbai, pov = "low50bhc")[["rates"]]
comps <- getheadlines(hbai, pov = "low50bhc")[["comps"]]
numbers <- getheadlines(hbai, pov = "low50bhc")[["numbers"]]
sample <- getheadlines(hbai, pov = "low60bhc")[["sample"]]

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
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Care should be taken when interpreting changes between years. Small sample sizes mean that differences will often not be statistically significant.",
                           "Longer term trends may offer a better indication of a real change over time.",
                           "Also note that differences of 10,000 between years in the Numbers table may, in some cases, be largely explained by rounding.")
)

# Create worksheet
createWideSpreadsheet(data)

## 7 Child material deprivation ------------------------------------------------

cmd_ahc <- getpovby(hbai, pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_ahc_new <- getpovby(hbai, pov = "cmdahc_new", weight = "gs_newch") %>%
  rbind(cmd_ahc) %>%
  mutate(Measure = "New measure, after housing costs")
cmd_ahc <- getpovby(hbai, pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, after housing costs") %>%
  rbind(cmd_ahc_new) %>%
  samplesizecheck() %>%
  roundall()

cmd_bhc <- getpovby(hbai, pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_bhc_new <- getpovby(hbai, pov = "cmdbhc_new", weight = "gs_newch") %>%
  rbind(cmd_bhc) %>%
  mutate(Measure = "New measure, before housing costs")
cmd_bhc <- getpovby(hbai, pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, before housing costs") %>%
  rbind(cmd_bhc_new) %>%
  samplesizecheck() %>%
  roundall()

cmd <- rbind(cmd_ahc, cmd_bhc) %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted))

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
                           "Number of families with children in the survey sample, Scotland"),
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
mark_missing(data, ncols = 9, nrows = 2, xlscol = 10, xlsrow = 10)
mark_missing(data, ncols = 9, nrows = 2, xlscol = 10, xlsrow = 20)
mark_missing(data, ncols = 9, nrows = 1, xlscol = 10, xlsrow = 29)


## 8 Pensioner deprivation -----------------------------------------------------

pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older") %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted))

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
                           "Number of families with pensioners aged 65 in the survey sample, Scotland"),
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

