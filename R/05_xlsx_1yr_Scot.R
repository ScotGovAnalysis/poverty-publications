
# TO DO -----
# update publication date in Readme

# prelims ----------------------------------------------------------------------

library(tidyverse)
library(openxlsx)

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "website/xls/data2023_1yr.xlsx"
mytables <- readRDS("data/tables_1yr.rds")

latestyear <- max(levels(labels$years$formatted))

# headers ----------------------------------------------------------------------

myheaders <- list(

  # * -- headline measures -----------------------------------------------------

  list(header = c("Relative poverty after housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                  "Composition: Proportion of people in relative poverty (below 60% of UK median income after housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in relative poverty (below 60% of UK median income after housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Absolute poverty after housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in absolute poverty (below 60% of the 2010/11 UK median income after housing costs), Scotland",
                  "Composition: Proportion of people in severe poverty (below 60% of the 2010/11 UK median income after housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in severe poverty (below 60% of the 2010/11 UK median income after housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Severe poverty after housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                  "Composition: Proportion of people in severe poverty (below 50% of UK median income after housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in severe poverty (below 50% of UK median income after housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Relative poverty before housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in relative poverty (below 60% of UK median income before housing costs), Scotland",
                  "Composition: Proportion of people in relative poverty (below 60% of UK median income before housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in relative poverty (below 60% of UK median income before housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Absolute poverty before housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in absolute poverty (below 60% of the 2010/11 UK median income before housing costs), Scotland",
                  "Composition: Proportion of people in severe poverty (below 60% of the 2010/11 UK median income before housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in severe poverty (below 60% of the 2010/11 UK median income before housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Severe poverty before housing costs",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of people in each group who are in severe poverty (below 50% of UK median income before housing costs), Scotland",
                  "Composition: Proportion of people in severe poverty (below 50% of UK median income before housing costs) who are in each group, Scotland",
                  "Numbers: Number of people in each group who are in severe poverty (below 50% of UK median income before housing costs), Scotland",
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Children's combined low income and material deprivation",
                  "This worksheet contains three tables.",
                  "Note: The latest estimate cannot be directly compared to previous estimates. This is because the latest period covers a time when families were less able to undertake certain activities due to the pandemic, and not necessarily because they couldn't afford to. This changed how people responded to the material deprivation questions.",
                  "Note: The definition of material deprivation changed in 2010/11, creating a break in the time series.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of children who are in combined low income (below 70% of UK median income) and material deprivation, Scotland",
                  "Numbers: Number of children who are in combined low income (below 70% of UK median income) and material deprivation, Scotland",
                  "Sample size: Number of families with children in the survey sample, Scotland")),
  list(header = c("Pensioners material deprivation",
                  "This worksheet contains three tables.",
                  "Note: The latest estimate cannot be directly compared to previous estimates. This is because the latest period covers a time when families were less able to undertake certain activities due to the pandemic, and not necessarily because they couldn't afford to. This changed how people responded to the material deprivation questions.",
                  "Note: Pensioner material deprivation is calculated for pensioners aged 65 or over. Pensioner material deprivation is different to other measures of poverty, including the child low income and material deprivation measure in that it is not associated with an income threshold. It captures issues such as whether poor health, disability and social isolation prevent access to goods and services, rather than solely low income.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Rate: Proportion of pensioners aged 65 and over who have limited access to goods and services, Scotland",
                  "Numbers: Number of pensioners aged 65 and over who have limited access to goods and services, Scotland",
                  "Sample size: Number of families with pensioners aged 65 and over in the survey sample, Scotland")),

  # * -- income measures -------------------------------------------------------

  list(header = c("Median household income",
                  "This worksheet contains three tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c(paste0("Before housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                  paste0("After housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                  "Sample size: Number of families in each group in the survey sample, Scotland")),
  list(header = c("Household income decile points",
                  "This worksheet contains three tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c(paste0("Before housing costs: Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland"),
                  paste0("After housing costs: Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland"),
                  "Sample size: Number of families in the survey sample, Scotland")),
  list(header = c("Income decile shares",
                  "This worksheet contains three tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c(paste0("Before housing costs: Annual household income shares in £ million (in ", latestyear, " prices), Scotland"),
                  paste0("After housing costs: Annual household income shares in £ million (in ", latestyear, " prices), Scotland"),
                  "Sample size: Number of families in the survey sample, Scotland")),
  list(header = c("Income inequality measures",
                  "This worksheet contains three tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c("Palma ratio (income share of the top 10% divided by the the bottom 40% of the household population), Scotland",
                  "Gini coefficient",
                  "Sample size: Number of families in the survey sample, Scotland")),
  list(header = c("Poverty and other income thresholds",
                  "This worksheet contains four tables.",
                  "Source: Scottish Government analysis of the Family Resources Survey"),
       titles = c(paste0("Before housing costs: Weekly income in £, and after tax and transfers, Scotland ", latestyear),
                  paste0("After housing costs: Annual income in £, and after tax and transfers, Scotland ", latestyear),
                  paste0("Before housing costs: Weekly income in £, and after tax and transfers, Scotland ", latestyear),
                  paste0("After housing costs: Annual income in £, after tax and transfers, Scotland ", latestyear)))
)

# number headers and tables ----------------------------------------------------
sheet_n <- length(mytables)
names(myheaders) <- paste0("sheet_", seq(1, sheet_n, 1))
names(mytables) <- paste0("sheet_", seq(1, sheet_n, 1))

# add prefixes to headings to mark them up as headings for VBA post-processing (optional)
for (i in 1:sheet_n) {
  myheaders[[i]]$header[1] <- paste0("H1-", myheaders[[i]]$header[1])
  myheaders[[i]]$titles <- paste0("H2-", myheaders[[i]]$titles)
}

# run all worksheets -----------------------------------------------------------

# Open / create workbook
if (file.exists(filename)) {wb <- loadWorkbook(filename)
} else {wb <- createWorkbook()}

invisible(
  sapply(seq(1, sheet_n, 1),
         function(x) {
           create_worksheet(sheet_no = x,
                            wb = wb,
                            tables = mytables[[x]],
                            headers = myheaders[[paste0("sheet_", x)]]$header,
                            titles = myheaders[[paste0("sheet_", x)]]$titles)
         }
  )
)


# create TOC -------------------------------------------------------------------
createContentSheet(wb = wb,
                   title = "H1-Table of contents",
                   toptext = "This workbook contains single-year estimates used in the Poverty and Income Inequality in Scotland National Statistics report and the child poverty update, published in March 2023, and additional analysis. The report is available here: data.gov.scot/poverty",
                   headings = list(titles = c("H2-Headline poverty measures - after housing costs",
                                              "H2-Headline poverty measures - before housing costs",
                                              "H2-Material deprivation",
                                              "H2-Income and income inequality"),
                                   location = c(0, 3, 6, 8)))

# create Readme sheet ----------------------------------------------------------
createReadmeSheet(
  wb = wb,
  notes = list(
    "H1-Important notes" = c("Published: 23 March 2023",
                             "Next update: March 2024",
                             "The tables in this spreadsheet contain single-year estimates to complement the three-year averages shown in the 'Poverty and Income Inequality in Scotland' National Statistics report. The report can be found here: https://data.gov.scot/poverty",
                             "Estimates are based on Scotland data from the Family Resources Survey. This survey is managed by the Department for Work and Pensions, who also published a report, available on their website.",
                             "Detailed information on definitions and methodology can be found in the report.",
                             "Three-year averages show trends better than single-year estimates. Three-year averages are also available for download."),

    "H2-COVID-19 impact" = c("The 2020/21 estimates are excluded. They are unreliable as they are based on data collected during the first year of the coronavirus (COVID-19) pandemic. Lockdown rules severely disrupted the data collection. As a result, we were unable to obtain a representative sample for Scotland in this year."),

    "H2-Tables" = c("Each worksheet contains one or more tables. The main table is at the top, and extra tables are below. The extra tables give context such as sample sizes, compositions, or population estimates.",
                    "Poverty rate tables show the proportion of individuals who are in poverty. Poverty rates describe the risk of being in poverty. If one group has a higher poverty rate than another, people in this group have a higher risk of being in poverty.",
                    "Composition tables show how people in poverty break down into subgroups. For example, it shows how many of those who are in poverty rent their homes versus how many own their homes. The poverty composition describes the scale of poverty for certain groups of people.",
                    "Numbers tables show the numbers of people (or children or adults) in poverty."),

    "H2-Poverty measures" = c("In this spreadsheet, we use different measures of poverty. Each measures a different aspect of people's living standards.",
                              "Relative poverty is the measure of low income that we use most often. It measures whether low-income households are keeping pace with middle income households. The relative poverty line is 60% of median UK income, and it changes every year along with median income.",
                              "Severe poverty is like relative poverty, but with a stricter threshold. It measures whether the lowest-income households are keeping pace with middle income households. The severe poverty line is 50% of median UK income, and it also changes every year.",
                              "Absolute poverty is another measure of low income. It measures whether the incomes of low-income households are keeping pace with inflation. The absolute poverty line is fixed at 60% of median UK income from 2010/11. It changes only a little due to inflation. It is now lower than the relative poverty line, but higher than the severe poverty line.",
                              "Material deprivation measures look at whether households can access basic goods and activities. The child measure combines material deprivation and low income. It looks at families who would like certain goods or activities but cannot afford them. The pensioner measure looks at pensioners who would like certain goods or activities but cannot access them. The barriers for this could be money, poor health, disability and social isolation."),

    "H2-Housing costs" = c("The most commonly used poverty indicator in Scotland is relative poverty after housing costs. After-housing-costs measures describe disposable household income after housing costs are paid for. They therefore better describe what is available to spend on food, bills and leisure every week or month."),

    "H2-Missing data" = c("In the tables, the following conventions have been used where figures are unavailable:",
                          "[u] - unreliable due to small sample size",
                          "[low] - below 0.5%",
                          "[b] - break in the time series",
                          "[x] - not available for another reason (data accuracy, data wasn't collected etc.)"),

    "H2-Reliability of the estimates" = c("The figures in these tables are estimates only, based on data from a sample survey. Thus, they could be a little bit higher or lower if we interviewed a different sample of the population.",
                                          "The precision of poverty rate estimates for the whole Scottish population is usually the central estimate plus or minus two percentage points, or plus or minus four percentage points for the Scottish child population. (Note that the precision is lower for 2018-21 estimates.) Precision is lower for smaller groups.",
                                          "Any small changes from year to year may not reflect real changes in the population. Longer-term trends are a better sign of a real change. Small differences between groups do not always reflect real differences in the population. Differences are more likely to be real if they are consistent over time.",
                                          "Estimates are considered unreliable when they are based on a very small sample size. For averages such as median incomes, this is less than 50 cases. For proportions and population estimates, it is less than 100 cases."),

    "H2-Rounding" = c("Proportions are rounded to five decimal places. Population numbers are rounded to the nearest 10,000 people. Annual GBP amounts are rounded to the nearest £100. Weekly GBP amounts are rounded to the nearest £1."),

    "H2-Data revision" = c("In 2021, previously published datasets underwent a minor methodological revision to capture all income from child maintenance. This led to small changes in household income and small adjustments to some poverty estimates. Therefore, some poverty and income estimates that have been published since March 2021 may not exactly match previously published estimates for 1994/95 to 2019/20. The revision did not affect any trends in poverty or household income."),

    "H2-Contact" = c("Maike Waldmann",
                     "Scottish Government",
                     "Communities Analysis Division",
                     "Email: social-justice-analysis@gov.scot")))

# save -------------------------------------------------------------------------

saveWorkbook(wb, filename, overwrite = TRUE)
cat(filename, " created", fill = TRUE)
rm(list = ls())
