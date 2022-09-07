# prelims ----------------------------------------------------------------------
library(tidyverse)
library(analysistools)
source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds") %>% filter(gvtregn == "Scotland")
adult <- readRDS("data/tidyadult.rds") %>% filter(gvtregn == "Scotland")

latestyear <- max(levels(labels$years$formatted))

tables_1yr <- list()

# :----------------------- -----------------------------------------------------
# 1 rel ------------------------------------------------------------------------
tables_1yr$relAHC <- list(rates = getheadlines(hbai, pov = "low60ahc",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low60ahc",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low60ahc",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low60ahc",
                                                threeyr = FALSE)[["sample"]])

# 2 abs ------------------------------------------------------------------------
tables_1yr$absAHC <- list(rates = getheadlines(hbai, pov = "low60ahcabs",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low60ahcabs",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low60ahcabs",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low60ahcabs",
                                                threeyr = FALSE)[["sample"]])
# 3 sev ------------------------------------------------------------------------
tables_1yr$sevAHC <- list(rates = getheadlines(hbai, pov = "low50ahc",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low50ahc",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low50ahc",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low50ahc",
                                                threeyr = FALSE)[["sample"]])
# 4 rel BHC --------------------------------------------------------------------
tables_1yr$relBHC <- list(rates = getheadlines(hbai, pov = "low60bhc",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low60bhc",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low60bhc",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low60bhc",
                                                threeyr = FALSE)[["sample"]])
# 5 abs BHC --------------------------------------------------------------------
tables_1yr$absBHC <- list(rates = getheadlines(hbai, pov = "low60bhcabs",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low60bhcabs",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low60bhcabs",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low60bhcabs",
                                                threeyr = FALSE)[["sample"]])
# 6 sev BHC --------------------------------------------------------------------
tables_1yr$sevBHC <- list(rates = getheadlines(hbai, pov = "low50bhc",
                                               threeyr = FALSE)[["rates"]],
                          comps = getheadlines(hbai, pov = "low50bhc",
                                               threeyr = FALSE)[["comps"]],
                          numbers = getheadlines(hbai, pov = "low50bhc",
                                                 threeyr = FALSE)[["numbers"]],
                          sample = getheadlines(hbai, pov = "low50bhc",
                                                threeyr = FALSE)[["sample"]])
# 7 cmd ------------------------------------------------------------------------
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
  mutate(year = get_years(yearn))

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

tables_1yr$cmd <- list(rates = rates,
                       numbers = numbers,
                       sample = sample)

# 8 pmd ------------------------------------------------------------------------
pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older") %>%
  mutate(year = get_years(yearn))

tables_1yr$pmd <- list(rates = pmd %>%
                         select(Group, year, rate) %>%
                         spread(year, rate),
                       numbers = pmd %>%
                         select(Group, year, number) %>%
                         spread(year, number),
                       sample = pmd  %>%
                         select(Group, year, sample) %>%
                         spread(year, sample))

# :----------------------- -----------------------------------------------------
# 9 medians --------------------------------------------------------------------
bhc <- hbai %>%
  group_by(yearn) %>%
  getmediansbhc() %>%
  mutate(year = get_years(yearn)) %>%
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
  group_by(yearn) %>%
  getmediansahc() %>%
  mutate(year = get_years(yearn)) %>%
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

tables_1yr$medians <- list(bhc = numbers_bhc,
                           ahc = numbers_ahc,
                           sample = sample)

# 10 decile points -------------------------------------------------------------
bhc <- hbai %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  mutate(year = get_years(yearn)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

ahc <- hbai %>%
  group_by(yearn) %>%
  getdecptsahc() %>%
  mutate(year = get_years(yearn)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(year = get_years(yearn),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$decilepoints <- list(bhc = bhc,
                                ahc = ahc,
                                sample = sample)
# 11 decile shares -------------------------------------------------------------
bhc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesbhc() %>%
  group_by(Decile) %>%
  mutate(share = round2(share / 1000000, 0)) %>%
  mutate(year = get_years(yearn)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

ahc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesahc() %>%
  group_by(Decile) %>%
  mutate(share = round2(share / 1000000, 0)) %>%
  mutate(year = get_years(yearn)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(year = get_years(yearn),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$decileshares <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)
# 12 Palma & Gini --------------------------------------------------------------
palma_bhc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesbhc() %>%
  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = roundpct(Palma),
         Measure = "Before housing costs") %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  mutate(year = get_years(yearn)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma_ahc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesahc() %>%
  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = roundpct(Palma),
         Measure = "After housing costs") %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  mutate(year = get_years(yearn)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  mutate(year = get_years(yearn)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini_ahc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  mutate(year = get_years(yearn)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini <- rbind(gini_bhc, gini_ahc)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(year = get_years(yearn),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$palmagini <- list(palma = palma,
                         gini = gini,
                         sample = sample)
# 13 poverty thresholds --------------------------------------------------------

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

tables_1yr$thresholds <- list(weekly_bhc = weekly_bhc,
                          annual_bhc = annual_bhc,
                          weekly_ahc = weekly_ahc,
                          annual_ahc = annual_ahc)

# :----------------------- -----------------------------------------------------
# save all ---------------------------------------------------------------------
saveRDS(tables_1yr, "data/tables_1yr.rds")
rm(list = ls())




