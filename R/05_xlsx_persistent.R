# load ----------------------------------------------------------------------

library(tidyverse)
library(openxlsx)

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "website/xls/data2023_persistent.xlsx"
mytables <- readRDS("data/persistenttables.rds")

# tables not to be included in spreadsheet
mytables <- within(mytables, rm(tableScotland, source))

latestperiod = max(mytables[[1]]$Period)

# prep datasets ----------------------------------------------------------------

mytables <- lapply(mytables, as.data.frame)

# Unformat formatted estimates
for (i in 1:4) {
  mytables[[i]][, 2] <- as.numeric(str_remove(mytables[[i]][,2], "%"))/100
  mytables[[i]][, 3] <- as.numeric(str_remove(mytables[[i]][,3], "%"))/100
  mytables[[i]][, 4] <- as.numeric(str_remove(mytables[[i]][,4], ","))
}

for (i in 5:6) {
  mytables[[i]][, 2] <- as.numeric(str_remove(mytables[[i]][,2], "%"))/100
  mytables[[i]][, 4] <- as.numeric(str_remove(mytables[[i]][,4], "%"))/100
  mytables[[i]][, 3] <- as.numeric(str_remove(mytables[[i]][,3], ","))
  mytables[[i]][, 5] <- as.numeric(str_remove(mytables[[i]][,5], ","))
}

# split into rates and samples
tables <- list(table1 = list(mytables$table1 %>% select(-Sample),
                             mytables$table1 %>% select(Period, Sample)),
               table2 = list(mytables$table2 %>% select(-Sample),
                             mytables$table2 %>% select(Period, Sample)),
               table3 = list(mytables$table3 %>% select(-Sample),
                             mytables$table3 %>% select(Period, Sample)),
               table4 = list(mytables$table4 %>% select(-Sample),
                             mytables$table4 %>% select(Period, Sample)),
               table5 = list(mytables$table5 %>% select(-(contains("Sample"))),
                             mytables$table5 %>% select(Period, (contains("Sample")))),
               table6 = list(mytables$table6 %>% select(-(contains("Sample"))),
                             mytables$table6 %>% select(Period, (contains("Sample")))),
               table7 = list(mytables$table7a,
                             mytables$table7b)
)


mytables <- tables

# headers ----------------------------------------------------------------------

myheaders <- list(

  list(header = c("People in persistent poverty",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are based on four two-wave periods; e.g. 2010-2014 is based on poverty status in 2010-2011, 2011-2012, 2012-2013, and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of people in persistent poverty, Scotland",
                  "Number of people in the longitudinal survey sample, Scotland")),
  list(header = c("Children in persistent poverty",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are based on four two-wave periods; e.g. 2010-2014 is based on poverty status in 2010-2011, 2011-2012, 2012-2013, and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of children in persistent poverty, Scotland",
                  "Number of children in the longitudinal survey sample, Scotland")),
  list(header = c("Working-age adults in persistent poverty",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are based on four two-wave periods; e.g. 2010-2014 is based on poverty status in 2010-2011, 2011-2012, 2012-2013, and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of working-age adults in persistent poverty, Scotland",
                  "Number of working-age adults in the longitudinal survey sample, Scotland")),
  list(header = c("Pensioners in persistent poverty",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are based on four two-wave periods; e.g. 2010-2014 is based on poverty status in 2010-2011, 2011-2012, 2012-2013, and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of pensioners in persistent poverty, Scotland",
                  "Number of pensioners in the longitudinal survey sample, Scotland")),

  list(header = c("Poverty exit",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: For an exit to occur, the individual must be in a household whose income is at least 10 per cent above the poverty threshold, while in the previous wave they were in a household whose income was below the poverty threshold.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are an average over three two-wave periods; e.g. 2010-2014 is an average of the entry/exit rate between 2010-2011 and 2011-2012, 2011-2012 and 2012-2013, and 2012-2013 and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of people exiting relative poverty, Scotland",
                  "Number of people in the combined 3-wave longitudinal survey sample, Scotland")),
  list(header = c("Poverty entry",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: For an entry to occur, the individual must be in a household whose income is at least 10 per cent below the poverty threshold, while in the previous wave they were in a household whose income was above the poverty threshold.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are an average over three two-wave periods; e.g. 2010-2014 is an average of the entry/exit rate between 2010-2011 and 2011-2012, 2011-2012 and 2012-2013, and 2012-2013 and 2013-2014.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of people entering relative poverty, Scotland",
                  "Number of people in the combined 3-wave longitudinal survey sample, Scotland")),

  list(header = c("Child poverty priority groups",
                  "This worksheet contains two tables; one for the estimates, and one for sample sizes.",
                  "Note: Understanding Society is based on a two calendar-year sample, with everyone in the sample being interviewed on an annual basis. Figures are based on four two-wave periods; e.g. 2016-2020 is based on poverty status in 2016-2017, 2017-2018, 2018-2019, and 2019-2020.",
                  "Source: Department for Work and Pensions analysis of the Understanding Society Survey"),
       titles = c("Proportion of children in each group who were in persistent poverty after housing costs, Scotland",
                  "Number of children in the longitudinal survey sample, Scotland"))
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

invisible(lapply(seq(1, sheet_n, 1),
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
                   toptext = "This workbook contains data used in the charts and tables in the Persistent poverty in Scotland report, published on 23 March 2023. The report is available at data.gov.scot/poverty/persistent.html",
                   headings = list(titles = c("H2-Persistent poverty",
                                              "H2-Poverty entry and exit",
                                              "H2-Child poverty priority groups"),
                                   location = c(0, 4, 6)))

# create Readme sheet ----------------------------------------------------------
createReadmeSheet(
  wb = wb,
  notes = list(
    "H1-Important notes" = c("Published: 23 March 2023",
                             "Next update: March 2024",
                             "The tables in this spreadsheet contain all estimates shown in the 'Persistent Poverty in Scotland' report. The report can be found here: https://data.gov.scot/poverty/persistent.html",
                             "Estimates are based on Scotland data from the Understanding Society Survey, produced by the UK Department for Work and Pensions (DWP). DWP also published a report, Income Dynamics, available on their website.",
                             "Detailed information on definitions and methodology can be found in the report."),

    "H2-Persistent poverty" = c("Persistent poverty identifies individuals who live in relative poverty for three or more of the last four years. It therefore identifies people who have been living in poverty for a significant period of time, which is more damaging than brief periods spent with a low income. The impacts can affect an individual throughout their lifetime."),

    "H2-Reliability of the estimates" = c("The figures in these tables are estimates only, based on data from a sample survey. Thus, they could be a little bit higher or lower if we interviewed a different sample of the population. Any small changes from period to period may not reflect real changes in the population. Longer-term trends are a better sign of a real change. Small differences between groups do not always reflect real differences in the population. Differences are more likely to be real if they are consistent over time."),

    "H2-Revisions" = c("Some estimates from previous years have been improved and will therefore differ between publications. The latest publication provides the most accurate estimates."),

    "H2-Missing data" = c("In the tables, the following conventions have been used where figures are unavailable:",
                          "[u] - unreliable due to small sample size",
                          "[low] - below 0.5%",
                          "[b] - break in the time series",
                          "[x] - not available for another reason (data accuracy, data wasn't collected etc.)"),

    "H2-Contact" = c("Maike Waldmann",
                     "Scottish Government",
                     "Communities Analysis Division",
                     "Email: social-justice-analysis@gov.scot")))

# save -------------------------------------------------------------------------

saveWorkbook(wb, filename, overwrite = TRUE)
cat(filename, " created", fill = TRUE)
rm(list = ls())
