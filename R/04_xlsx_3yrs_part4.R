# Create spreadsheet for 3-year averaged data

library(tidyverse)

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_strings.R")

filename <- "output/All three-year average.xlsx"

hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland") %>%
  group_by(yearn)

latestyear <- max(levels(labels$years$formatted))

## 37 Medians ------------------------------------------------------------------

bhc <- hbai %>%
  getmediansbhc() %>%
  mutate_at(vars(c("pp", "ch", "wa", "pn")), get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L)

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
  mutate_at(vars(c("pp", "ch", "wa", "pn")), get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L)

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

data <- list(file = filename,
             sheet = "37 Median",
             sheettitle = "37. Median household income",
             dfs = list(numbers_bhc, numbers_ahc, sample),
             formats = c("GBP", "GBP", "num"),
             totalrow = TRUE,
             titles = c("Median income by age group",
                        "Median income by age group",
                        "Sample sizes"),
             subtitles = c(str_c("Before housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                           str_c("After housing costs: Median weekly equivalised household income in £ (in ", latestyear, " prices), Scotland"),
                           str_c("Number of families in each group in the combined three-year survey sample, Scotland ", latestyear)),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = NULL)

# Create spreadsheet and first worksheet
createWideSpreadsheet(data)

## 38 Decile points ------------------------------------------------------------

bhc <- hbai %>%
  getdecptsbhc() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods,
                       ordered = TRUE)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~get3yraverage(.)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

ahc <- hbai %>%
  getdecptsahc() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods,
                       ordered = TRUE)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~get3yraverage(.)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

# Put all input for the spreadsheet into a list
data <- list(sheet = "38 Dec pts",
             file = filename,
             sheettitle = "38. Income decile points",
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

# Set column widths
wb <- loadWorkbook(filename)
setColWidths(wb, data$sheet, cols = 2, widths = 7)
saveWorkbook(wb, filename, overwrite = TRUE)

## 39 Decile shares ------------------------------------------------------------

bhc <- hbai %>%
  getdecsharesbhc() %>%
  group_by(Decile) %>%
  mutate(share = get3yraverage(share),
         share = round2(share / 1000000, n = 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

ahc <- hbai %>%
  getdecsharesahc() %>%
  group_by(Decile) %>%
  mutate(share = get3yraverage(share),
         share = round2(share / 1000000, n = 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

# Put all input for the spreadsheet into a list
data <- list(sheet = "39 Dec shrs",
             file = filename,
             sheettitle = "39. Income decile shares",
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

# Set column widths
wb <- loadWorkbook(filename)
setColWidths(wb, data$sheet, cols = 2, widths = 7)
saveWorkbook(wb, filename, overwrite = TRUE)

## 40 Palma and Gini -----------------------------------------------------------

palma_bhc <- hbai %>%
  getdecsharesbhc() %>%
  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = get3yraverage(Palma),
         Palma = roundpct(Palma),
         Measure = "Before housing costs") %>%
  filter(Decile == 10,
         yearn >= 3) %>%
  ungroup() %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma_ahc <- hbai %>%
  getdecsharesahc() %>%
  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = get3yraverage(Palma),
         Palma = roundpct(Palma),
         Measure = "After housing costs") %>%
  filter(Decile == 10,
         yearn >= 3) %>%
  ungroup() %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini_ahc <- hbai %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini <- rbind(gini_bhc, gini_ahc)

data <- list(file = filename,
             sheet = "40 Palma Gini",
             sheettitle = "40. Income inequality measures",
             dfs = list(palma, gini),
             formats = c("pct", "pct"),
             titles = c("Palma ratio of income inequality",
                        "Gini coefficient of income inequality"),
             subtitles = c("Income share of the top 10% divided by the the bottom 40% of the household population, Scotland",
                           "Gini coefficient"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset")

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

## 41 Poverty thresholds -------------------------------------------------------

hbai1 <- filter(ungroup(hbai), yearn == max(yearn))
hbai2 <- filter(ungroup(hbai), yearn == max(yearn) - 1)
hbai3 <- filter(ungroup(hbai), yearn == max(yearn) - 2)

bhc1 <- getpovertythresholdsbhc(hbai1)
bhc2 <- getpovertythresholdsbhc(hbai2)
bhc3 <- getpovertythresholdsbhc(hbai3)

weekly_bhc <- (bhc1[, 2:9] + bhc2[, 2:9] + bhc3[, 2:9]) / 3
weekly_bhc$Measure <- bhc1$Measure

weekly_bhc <- weekly_bhc %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_bhc <- (bhc1[, 2:9] + bhc2[, 2:9] + bhc3[, 2:9]) / 3
annual_bhc$Measure <- bhc1$Measure

annual_bhc <- annual_bhc %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

ahc1 <- getpovertythresholdsahc(hbai1)
ahc2 <- getpovertythresholdsahc(hbai2)
ahc3 <- getpovertythresholdsahc(hbai3)

weekly_ahc <- (ahc1[, 2:9] + ahc2[, 2:9] + ahc3[, 2:9]) / 3
weekly_ahc$Measure <- ahc1$Measure

weekly_ahc <- weekly_ahc %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_ahc <- (ahc1[, 2:9] + ahc2[, 2:9] + ahc3[, 2:9]) / 3
annual_ahc$Measure <- ahc1$Measure

annual_ahc <- annual_ahc %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

# Put all input for the spreadsheet into a list
data <- list(file = filename,
             sheet = "41 Pov thrshlds",
             sheettitle = "41. Poverty and other income thresholds",
             dfs = list(weekly_bhc, weekly_ahc, annual_bhc, annual_ahc),
             formats = c("GBP", "GBP", "GBP", "GBP"),
             titles = c("Weekly income thresholds for different household types",
                        "Weekly income thresholds for different household types",
                        "Annual income thresholds for different household types",
                        "Annual income thresholds for different household types"),
             subtitles = c(paste0("Before housing costs: Income is in £, and after tax and transfers, Scotland ", max(levels(labels$years$periods))),
                           paste0("After housing costs: Income is in £, and after tax and transfers, Scotland ", max(levels(labels$years$periods))),
                           paste0("Before housing costs: Income is in £, and after tax and transfers, Scotland ", max(levels(labels$years$periods))),
                           paste0("After housing costs: Income is in £, after tax and transfers, Scotland ", max(levels(labels$years$periods)))),
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

## 42 Income sources (BHC only) by income decile -------------------------------

hbai1 <- filter(ungroup(hbai), yearn == max(yearn))
hbai2 <- filter(ungroup(hbai), yearn == max(yearn) - 1)
hbai3 <- filter(ungroup(hbai), yearn == max(yearn) - 2)

df1 <- getsources(hbai1)
df2 <- getsources(hbai2)
df3 <- getsources(hbai3)

df <- data.frame(df1[1])
df[2] <- (df1[2] + df2[2] + df3[2])/3
df[3] <- (df1[3] + df2[3] + df3[3])/3
df[4] <- (df1[4] + df2[4] + df3[4])/3
df[5] <- (df1[5] + df2[5] + df3[5])/3
df[6] <- (df1[6] + df2[6] + df3[6])/3

df <- df %>%
  mutate(Decile = fct_relevel(Decile, "All", after = 0L)) %>%
  mutate_if(is.numeric, roundpct) %>%
  arrange(Decile)

data <- list(file = filename,
             sheet = "42 Sources",
             sheettitle = "42. Income sources",
             dfs = list(df),
             formats = "pct",
             totalrow = TRUE,
             titles = "Income sources by income decile",
             subtitles = paste0("Gross household income by income type as a share of total income, Scotland ", max(levels(labels$years$periods))),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset",
             footnotes = c("1. Income here is gross income before housing costs. Income deciles are based on equivalised net income before housing costs.",
                           "2. Due to volatility of the data, single-year estimates of these statistics are not available."))

# Create spreadsheet and new worksheet
createWideSpreadsheet(data)

# Set column widths and text-wrap some cells
wb <- loadWorkbook(filename)
setColWidths(wb, data$sheet, cols = 2, widths = 7)
setColWidths(wb, data$sheet, cols = 3:7, widths = 12)
addStyle(wb, data$sheet, cols = 3:7, rows = 7,
         style = createStyle(wrapText = TRUE), gridExpand = TRUE, stack = TRUE)
saveWorkbook(wb, filename, overwrite = TRUE)

# TOC --------------------------------------------------------------------------

headings <- list(location = c(0, 4, 8, 11, 26, 40),
                 titles = c("Headline poverty measures - after housing costs",
                            "Headline poverty measures - before housing costs",
                            "Material deprivation",
                            "Poverty characteristics - after housing costs",
                            "Child poverty characteristics - after housing costs",
                            "Income and income inequality"))

createContentSheet(filename, headings)

rm(list = ls())
