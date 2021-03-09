
# Create spreadsheet for single-year data - part 2

library(tidyverse)

source("R/00_functions.R")
source("R/00_strings.R")

filename <- "output/All single year.xlsx"

hbai <- readRDS("data/tidyhbai.rds") %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  filter(gvtregn == "Scotland") %>%
  group_by(year)

latestyear <- max(levels(labels$years$formatted))

## 9 Median household income by age group --------------------------------------

bhc <- hbai %>%
  getmediansbhc() %>%
  mutate_if(is.numeric, ~round2(., 0))

numbers_pp <- select(bhc, year, pp) %>%
  spread(year, pp) %>%
  mutate(Group = "All people")

numbers_ch <- select(bhc, year, ch) %>%
  spread(year, ch) %>%
  mutate(Group = "Children")

numbers_wa <- select(bhc, year, wa) %>%
  spread(year, wa) %>%
  mutate(Group = "Working-age adults")

numbers_pn <- select(bhc, year, pn) %>%
  spread(year, pn) %>%
  mutate(Group = "Pensioners")

sample_pp <- select(bhc, year, pp_sample) %>%
  spread(year, pp_sample) %>%
  mutate(Group = "All people")

sample_ch <- select(bhc, year, ch_sample) %>%
  spread(year, ch_sample) %>%
  mutate(Group = "Children")

sample_wa <- select(bhc, year, wa_sample) %>%
  spread(year, wa_sample) %>%
  mutate(Group = "Working-age adults")

sample_pn <- select(bhc, year, pn_sample) %>%
  spread(year, pn_sample) %>%
  mutate(Group = "Pensioners")

numbers_bhc <- rbind(numbers_pp, numbers_ch, numbers_wa, numbers_pn) %>%
  select(Group, everything())

ahc <- hbai %>%
  getmediansahc() %>%
  mutate_if(is.numeric, ~round2(., 0))

numbers_pp <- select(ahc, year, pp) %>%
  spread(year, pp) %>%
  mutate(Group = "All people")

numbers_ch <- select(ahc, year, ch) %>%
  spread(year, ch) %>%
  mutate(Group = "Children")

numbers_wa <- select(ahc, year, wa) %>%
  spread(year, wa) %>%
  mutate(Group = "Working-age adults")

numbers_pn <- select(ahc, year, pn) %>%
  spread(year, pn) %>%
  mutate(Group = "Pensioners")

sample_pp <- select(ahc, year, pp_sample) %>%
  spread(year, pp_sample) %>%
  mutate(Group = "All people")

sample_ch <- select(ahc, year, ch_sample) %>%
  spread(year, ch_sample) %>%
  mutate(Group = "Children")

sample_wa <- select(ahc, year, wa_sample) %>%
  spread(year, wa_sample) %>%
  mutate(Group = "Working-age adults")

sample_pn <- select(ahc, year, pn_sample) %>%
  spread(year, pn_sample) %>%
  mutate(Group = "Pensioners")

numbers_ahc <- rbind(numbers_pp, numbers_ch, numbers_wa, numbers_pn) %>%
  select(Group, everything())

sample <- rbind(sample_pp, sample_ch, sample_wa, sample_pn) %>%
  select(Group, everything())

# Put all input for the spreadsheet into a list
data <- list(file = filename,
             sheet = "9 Median",
             sheettitle = "9. Median household income",
             dfs = list(numbers_bhc, numbers_ahc, sample),
             formats = c("GBP", "GBP", "num"),
             totalrow = TRUE,
             titles = c("Median income by age group",
                        "Median income by age group",
                        "Sample sizes"),
             subtitles = c(str_c("Before housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                           str_c("After housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                           "Number of families in each group in the survey sample, Scotland"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

## 10 Income decile points -----------------------------------------------------

bhc <- hbai %>%
  getdecptsbhc() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

ahc <- hbai %>%
  getdecptsahc() %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

# Put all input for the spreadsheet into a list
data <- list(sheet = "10 Dec pts",
             file = filename,
             sheettitle = "10. Income decile points",
             dfs = list(bhc, ahc),
             formats = c("GBP", "GBP"),
             titles = c("Household income decile points",
                        "Household income decile points"),
             subtitles = c(str_c("Before housing costs: Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland"),
                           str_c("After housing costs: Median weekly equivalised household income decile points in £ (in ", latestyear, " prices), Scotland")),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 11 Income decile shares -----------------------------------------------------

bhc <- hbai %>%
  getdecsharesbhc() %>%
  ungroup() %>%
  mutate(share = round2(share / 1000000, n = 0),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

ahc <- hbai %>%
  getdecsharesahc() %>%
  ungroup() %>%
  mutate(share = round2(share / 1000000, n = 0),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

# Put all input for the spreadsheet into a list
data <- list(sheet = "11 Dec shrs",
             file = filename,
             sheettitle = "11. Income decile shares",
             dfs = list(bhc, ahc),
             formats = c("GBP", "GBP"),
             titles = c("Household income decile shares",
                        "Household income decile shares"),
             subtitles = c(str_c("Before housing costs: Annual household income shares in £ million (in ", latestyear, " prices), Scotland"),
                           str_c("After housing costs: Annual household income shares in £ million (in ", latestyear, " prices), Scotland")),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 12 Palma and Gini -----------------------------------------------------------

palma_bhc <- hbai %>%
  getdecsharesbhc() %>%
  mutate(Palma = share[10] / sum(share[1:4]),
         Palma = roundpct(Palma),
         Measure = "Before housing costs",
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma_ahc <- hbai %>%
  getdecsharesahc() %>%
  mutate(Palma = share[10] / sum(share[1:4]),
         Palma = roundpct(Palma),
         Measure = "After housing costs",
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$formatted,
                       ordered = TRUE)) %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  spread(year, Gini)

gini_ahc <- hbai %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  spread(year, Gini)

gini <- rbind(gini_bhc, gini_ahc)

# Put all input for the spreadsheet into a list
data <- list(file = filename,
             sheet = "12 Palma Gini",
             sheettitle = "12. Income inequality measures",
             dfs = list(palma, gini),
             formats = c("pct", "pct"),
             titles = c("Palma ratio of income inequality",
                        "Gini coefficient of income inequality"),
             subtitles = c("Income share of the top 10% divided by the the bottom 40% of the household population, Scotland",
                           "Gini coefficient"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset")

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 13 Poverty thresholds -------------------------------------------------------

hbai_latest <- hbai %>%
  ungroup() %>%
  filter(yearn == max(yearn))

weekly_bhc <- getpovertythresholdsbhc(hbai_latest) %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_bhc <- getpovertythresholdsbhc(hbai_latest) %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

weekly_ahc <- getpovertythresholdsahc(hbai_latest) %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_ahc <- getpovertythresholdsahc(hbai_latest) %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

# Put all input for the spreadsheet into a list
data <- list(file = filename,
             sheet = "13 Pov thrshlds",
             sheettitle = "13. Poverty and other income thresholds",
             dfs = list(weekly_bhc, weekly_ahc, annual_bhc, annual_ahc),
             formats = c("GBP", "GBP", "GBP", "GBP"),
             titles = c("Weekly income thresholds for different household types",
                        "Weekly income thresholds for different household types",
                        "Annual income thresholds for different household types",
                        "Annual income thresholds for different household types"),
             subtitles = c(paste0("Before housing costs: Income is in £, and after tax and transfers, Scotland ", latestyear),
                           paste0("After housing costs: Income is in £, and after tax and transfers, Scotland ", latestyear),
                           paste0("Before housing costs: Income is in £, and after tax and transfers, Scotland ", latestyear),
                           paste0("After housing costs: Income is in £, after tax and transfers, Scotland ", latestyear)),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

# Fix col widths, text-wrap some cells
wb <- loadWorkbook(filename)
setColWidths(wb, data$sheet, cols = 2, widths = 70)
setColWidths(wb, data$sheet, cols = 3:6, widths = 12)
addStyle(wb, data$sheet, cols = 3:6, rows = 4:61,
         style = createStyle(wrapText = TRUE), gridExpand = TRUE, stack = TRUE)
saveWorkbook(wb, filename, overwrite = TRUE)

# TOC --------------------------------------------------------------------------

headings <- list(location = c(0, 4, 8, 11),

                 titles = c("Headline poverty measures - after housing costs",
                            "Headline poverty measures - before housing costs",
                            "Material deprivation",
                            "Income and income inequality"))

createContentSheet(filename, headings)

rm(list = ls())




