# Note: this is how we dealt with the bad 2021 data.

# - all 2021 weights set to 0
# - added na.rm = TRUE to all getnyrtable, getrollingmean, getrollingtotal,
#   getheadlines & summarise_data function calls
# - wtd.medians and wtd.quantile don't deal with an all-zero weights vector, so
#   needed to be dealt with like so: in each table, filter bad year out and then
#   add it back in before taking 3-year average

# missing codes

# numeric and character missing codes (will be automatically replaced in spreadsheets)
# NA -> [u]: unreliable due to small sample size
# 99991 -> [b]: break in the time series
# 99992 -> [x]: not available for another reason (data accuracy, data wasn't collected etc.)"),
# 99993 -> [low]: low figure but not a real zero

# load -------------------------------------------------------------------------

library(tidyverse)
library(analysistools)
source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland")

adult <- readRDS("data/tidyadult.rds") %>%
  filter(gvtregn == "Scotland")

tables <- list()

latestyear <- max(levels(labels$years$formatted))
periods <- levels(labels$years$periods)

# :----------------------- -----------------------------------------------------
# rel --------------------------------------------------------------------------
tables$relAHC <- list(rates = getheadlines(hbai, pov = "low60ahc", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60ahc", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60ahc", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60ahc", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# abs --------------------------------------------------------------------------
tables$absAHC <- list(rates = getheadlines(hbai, pov = "low60ahcabs", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60ahcabs", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60ahcabs", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60ahcabs", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# sev --------------------------------------------------------------------------
tables$sevAHC <- list(rates = getheadlines(hbai, pov = "low50ahc", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low50ahc", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low50ahc", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low50ahc", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# rel BHC ----------------------------------------------------------------------
tables$relBHC <- list(rates = getheadlines(hbai, pov = "low60bhc", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60bhc", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60bhc", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60bhc", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# abs BHC ----------------------------------------------------------------------
tables$absBHC <- list(rates = getheadlines(hbai, pov = "low60bhcabs", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60bhcabs", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60bhcabs", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60bhcabs", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# sev BHC ----------------------------------------------------------------------
tables$sevBHC <- list(rates = getheadlines(hbai, pov = "low50bhc", na.rm = TRUE,
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low50bhc", na.rm = TRUE,
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low50bhc", na.rm = TRUE,
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low50bhc", na.rm = TRUE,
                                            threeyr = TRUE)[["sample"]])

# cmd --------------------------------------------------------------------------
cmd_ahc <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_ahc_new <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdahc_new", weight = "gs_newch") %>%
  rbind(cmd_ahc) %>%
  mutate(Measure = "New measure, after housing costs")
cmd_ahc <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, after housing costs") %>%
  rbind(cmd_ahc_new) %>%
  group_by(Measure) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd_bhc <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_bhc_new <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdbhc_new", weight = "gs_newch") %>%
  rbind(cmd_bhc) %>%
  mutate(Measure = "New measure, before housing costs")
cmd_bhc <- filter(hbai, depchldh != "No children in the household") %>%
  getpovby(pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, before housing costs") %>%
  rbind(cmd_bhc_new) %>%
  group_by(Measure) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd <- rbind(cmd_ahc, cmd_bhc)  %>%
  mutate(year = get_periods(yearn))

rates <- cmd  %>%
  select(year, rate, Measure) %>%
  pivot_wider(names_from = year, values_from = rate,

              # missing code for time series break
              values_fill = list(rate = 99992)) %>%

  # missing code for time series break
  mutate("2009-12" = 99991) %>%

  select(1:6, "2009-12", everything())

numbers <- cmd %>%
  select(year, number, Measure) %>%
  pivot_wider(names_from = year, values_from = number,

              # missing code for time series break
              values_fill = list(number = 99992)) %>%

  mutate("2009-12" = 99991) %>%
  select(1:6, "2009-12", everything())

sample <- cmd %>%
  select(year, sample, Measure) %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  pivot_wider(names_from = year, values_from = sample) %>%
  mutate("2009-12" = 99991) %>%
  select(1:6, "2009-12", everything()) %>%
  mutate(Measure = ifelse(Measure == "Old measure, after housing costs",
                          "Old measure", "New measure")) %>%
  replace(is.na(.), 99992)

tables$cmd <- list(rates = rates,
                   numbers = numbers,
                   sample = sample)

# pmd --------------------------------------------------------------------------
pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older",
         year = get_periods(yearn))

tables$pmd <- list(rates = pmd %>%
                     select(Group, year, rate) %>%
                     pivot_wider(names_from = year, values_from = rate),
                   numbers = pmd %>%
                     select(Group, year, number) %>%
                     pivot_wider(names_from = year, values_from = number),
                   sample = pmd  %>%
                     select(Group, year, sample) %>%
                     pivot_wider(names_from = year, values_from = sample))
# :----------------------- -----------------------------------------------------
# hd agebands (pp) -------------------------------------------------------------

rel <- getpovby(hbai, by = "agehdband8", weight = "gs_newpp") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(hbai, pov = "low50ahc", by = "agehdband8", weight = "gs_newpp") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$ageband_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# disability (pp) -------------------------------------------------------------------
rel_pp <- getpovby(hbai, by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_pp <- getpovby(hbai, pov = "low50ahc", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ch <- getpovby(hbai, by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ch <- getpovby(hbai, pov = "low50ahc", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ad <- getpovby(hbai, by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ad <- getpovby(hbai, pov = "low50ahc", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

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

# mark break in time series (numbers only)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

tables$disab_pp <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                        rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                        rel_numbers = rel_numbers,

                        sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                        sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                        sev_numbers = sev_numbers,

                        sample = rbind(sample1, sample2, sample3))

# disability no bens (pp) -----------------------------------------------------------
rel_pp <- getpovby(hbai, pov = "low60ahc_dis", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_pp <- getpovby(hbai, pov = "low50ahc_dis", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ch <- getpovby(hbai, pov = "low60ahc_dis", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ch <- getpovby(hbai, pov = "low50ahc_dis", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ad <- getpovby(hbai, pov = "low60ahc_dis", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ad <- getpovby(hbai, pov = "low50ahc_dis", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

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

# mark break in time series (numbers only)
rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

tables$disab_nobens_pp <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                               rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                               rel_numbers = rel_numbers,

                               sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                               sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                               sev_numbers = sev_numbers,

                               sample = rbind(sample1, sample2, sample3))
# ethgrphh (pp) ---------------------------------------------------------------------

rel <- hbai %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)") %>%
  mutate(ethgrphh = fct_drop(ethgrphh)) %>%
  getpovby(by = "ethgrphh") %>%
  summarise_data(5, na.rm = TRUE)

sev <- hbai %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)")  %>%
  mutate(ethgrphh = fct_drop(ethgrphh)) %>%
  getpovby(pov = "low50ahc", by = "ethgrphh") %>%
  summarise_data(5, na.rm = TRUE)

age <- hbai %>%
  rbind(hbai %>% filter(ethgrphh != "(Missing)") %>% mutate(ethgrphh = "All")) %>%

  # get hrp info
  left_join(adult %>%
              filter(hrpid == 1, gvtregn == "Scotland") %>%
              select(sernum, benunit, hrpid),
            by = c("sernum", "benunit")) %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)",
         hrpid == 1,
         # exclude bad data year
         yearn != 27) %>%
  group_by(yearn, ethgrphh) %>%
  summarise(age = wtd.median(agehd, gs_newbu),
            sample = sum(gs_newbu > 0, na.rm = TRUE)) %>%

  # add year back in
  full_join(data.frame(yearn = 27,
                       ethgrphh = unique(.$ethgrphh))) %>%
  arrange(yearn) %>%

  group_by(ethgrphh) %>%
  mutate(age = analysistools::getrollingmean(age, 5, na.rm = TRUE),
         sample = analysistools::getrollingtotal(sample, 5, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yearn >= 12) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = get_periods(yearn, n = 5),
         ethgrphh = factor(ethgrphh,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other"))) %>%
  select(year, age, ethgrphh) %>%
  arrange(ethgrphh) %>%
  pivot_wider(names_from = year, values_from = age) %>%
  rename(Group = ethgrphh)


tables$ethgrphh_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),

                           age = age,
                           sample = splitntranspose(rel, "sample"))

# depchldh (pp) ----------------------------------------------------------------

rel <- getpovby(hbai, by = "depchldh") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(hbai, pov = "low50ahc", by = "depchldh") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$depchldh_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# tenhbai (pp) -----------------------------------------------------------------

rel <- getpovby(hbai, by = "tenhbai") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 12)

sev <- getpovby(hbai, pov = "low50ahc", by = "tenhbai") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 12)

tables$tenhbai_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# urinds (pp) ------------------------------------------------------------------

rel <- getpovby(hbai, by = "urinds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 15)

sev <- getpovby(hbai, pov = "low50ahc", by = "urinds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 15)

tables$urinds_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                         rel_comps = splitntranspose(rel, "composition"),
                         rel_numbers = splitntranspose(rel, "number"),

                         sev_rates = splitntranspose(sev, "rate"),
                         sev_comps = splitntranspose(sev, "composition"),
                         sev_numbers = splitntranspose(sev, "number"),
                         sample = splitntranspose(rel, "sample"))

# SIMD (NEW) (pp) --------------------------------------------------------------
# tables only

rel <- hbai %>%
  filter(imds != "(Missing)") %>%
  getpovby(by = "imds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "1", "2", "3", "4", "5", "6",
                                         "7", "8", "9", "10", "(Missing)")))

sev <- hbai %>%
  filter(imds != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "imds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "1", "2", "3", "4", "5", "6",
                                         "7", "8", "9", "10", "(Missing)")))

tables$simd_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                       rel_comps = splitntranspose(rel, "composition"),
                       rel_numbers = splitntranspose(rel, "number"),

                       sev_rates = splitntranspose(sev, "rate"),
                       sev_comps = splitntranspose(sev, "composition"),
                       sev_numbers = splitntranspose(sev, "number"),
                       sample = splitntranspose(rel, "sample"))

# :----------------------- -----------------------------------------------------
# loneparenthh (ch) -----------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "loneparenthh", weight = "gs_newch") %>%
  summarise_data(na.rm = TRUE)

sev <- getpovby(hbai, pov = "low50ahc", by = "loneparenthh", weight = "gs_newch") %>%
  summarise_data(na.rm = TRUE)

tables$loneparenthh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                               rel_comps = splitntranspose(rel, "composition"),
                               rel_numbers = splitntranspose(rel, "number"),
                               sev_rates = splitntranspose(sev, "rate"),
                               sev_comps = splitntranspose(sev, "composition"),
                               sev_numbers = splitntranspose(sev, "number"),
                               sample = splitntranspose(rel, "sample"))

# depchldh_ch (ch) ------------------------------------------------------------------

rel <- hbai %>%
  getpovby(pov = "low60ahc", by = "depchldh_ch", weight = "gs_newch") %>%
  filter(groupingvar != "No children in the household") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- hbai  %>%
  getpovby(pov = "low50ahc", by = "depchldh_ch", weight = "gs_newch") %>%
  filter(groupingvar != "No children in the household") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$depchldh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),
                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# child age (ch) --------------------------------------------------------------------

rel0 <- getpovby(hbai, weight = "gs_newch") %>% getnyrtable(na.rm = TRUE)
rel1 <- getpovby(hbai, weight = "wgt0_4") %>% getnyrtable(na.rm = TRUE)
rel2 <- getpovby(hbai, weight = "wgt5_12") %>% getnyrtable(na.rm = TRUE)
rel3 <- getpovby(hbai, weight = "wgt13plus") %>% getnyrtable(na.rm = TRUE)

rel <- rbind(rel0, rel1, rel2, rel3) %>% mutate(year = get_periods(yearn))

sev0 <- getpovby(hbai, pov = "low50ahc", weight = "gs_newch") %>% getnyrtable(na.rm = TRUE)
sev1 <- getpovby(hbai, pov = "low50ahc", weight = "wgt0_4") %>% getnyrtable(na.rm = TRUE)
sev2 <- getpovby(hbai, pov = "low50ahc", weight = "wgt5_12") %>% getnyrtable(na.rm = TRUE)
sev3 <- getpovby(hbai, pov = "low50ahc", weight = "wgt13plus") %>% getnyrtable(na.rm = TRUE)

sev <- rbind(sev0, sev1, sev2, sev3) %>% mutate(year = get_periods(yearn))

rel_rates <- rel %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, rate) %>%
  pivot_wider(names_from = year, values_from = rate) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

rel_comps <- rel %>%
  group_by(year) %>%
  mutate(composition = number / number[1],
         composition = roundpct(composition)) %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, composition) %>%
  pivot_wider(names_from = year, values_from = composition) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

rel_numbers <- rel %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, number) %>%
  pivot_wider(names_from = year, values_from = number) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sev_rates <- sev %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, rate) %>%
  pivot_wider(names_from = year, values_from = rate) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sev_comps <- sev %>%
  group_by(year) %>%
  mutate(composition = number / number[1],
         composition = roundpct(composition)) %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, composition) %>%
  pivot_wider(names_from = year, values_from = composition) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sev_numbers <- sev %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, number) %>%
  pivot_wider(names_from = year, values_from = number) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sample <- rel %>%
  select(weight, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

tables$age_ch <- list(rel_rates = rel_rates,
                      rel_comps = rel_comps,
                      rel_numbers = rel_numbers,
                      sev_rates = sev_rates,
                      sev_comps = sev_comps,
                      sev_numbers = sev_numbers,
                      sample = sample)

# babyhh (ch) -----------------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "babyhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  # mark missing (code 99992) the years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, sample))

sev <- getpovby(hbai, pov = "low50ahc", by = "babyhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  # mark missing (code 99992) the years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, sample))

tables$baby_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                       rel_comps = splitntranspose(rel, "composition"),
                       rel_numbers = splitntranspose(rel, "number"),
                       sev_rates = splitntranspose(sev, "rate"),
                       sev_comps = splitntranspose(sev, "composition"),
                       sev_numbers = splitntranspose(sev, "number"),
                       sample = splitntranspose(rel, "sample"))

# youngmumhh (ch) -------------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "youngmumhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)",
         yearn >= 4) %>%
  summarise_data(na.rm = TRUE) %>%
  # mark missing (code 99992) the years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, sample))

sev <- getpovby(hbai, pov = "low50ahc", by = "youngmumhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)",
         yearn >= 4) %>%
  summarise_data(na.rm = TRUE) %>%
  # mark missing (code 99992) the years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", 99992, sample))

tables$youngmum_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),
                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# ecobu (ch) -------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "ecobu", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "ecobu", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

tables$ecobu_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                        rel_comps = splitntranspose(rel, "composition"),
                        rel_numbers = splitntranspose(rel, "number"),
                        sev_rates = splitntranspose(sev, "rate"),
                        sev_comps = splitntranspose(sev, "composition"),
                        sev_numbers = splitntranspose(sev, "number"),
                        sample = splitntranspose(rel, "sample"))

# workinghh (ch) ---------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "workinghh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "workinghh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

tables$workinghh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                            rel_comps = splitntranspose(rel, "composition"),
                            rel_numbers = splitntranspose(rel, "number"),
                            sev_rates = splitntranspose(sev, "rate"),
                            sev_comps = splitntranspose(sev, "composition"),
                            sev_numbers = splitntranspose(sev, "number"),
                            sample = splitntranspose(rel, "sample"))

# tenhbai (ch) -----------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "tenhbai", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 12)

sev <- getpovby(hbai, pov = "low50ahc", by = "tenhbai", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 12)

tables$tenhbai_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),
                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# urinds (ch) ------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "urinds", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 15)

sev <- getpovby(hbai, pov = "low50ahc", by = "urinds", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 15)

tables$urinds_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                         rel_comps = splitntranspose(rel, "composition"),
                         rel_numbers = splitntranspose(rel, "number"),
                         sev_rates = splitntranspose(sev, "rate"),
                         sev_comps = splitntranspose(sev, "composition"),
                         sev_numbers = splitntranspose(sev, "number"),
                         sample = splitntranspose(rel, "sample"))

# disability (ch) --------------------------------------------------------
rel_pp <- getpovby(hbai, by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_pp <- getpovby(hbai, pov = "low50ahc", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ch <- getpovby(hbai, by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ch <- getpovby(hbai, pov = "low50ahc", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ad <- getpovby(hbai, by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ad <- getpovby(hbai, pov = "low50ahc", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

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

rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

tables$disab_ch <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                        rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                        rel_numbers = rel_numbers,
                        sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                        sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                        sev_numbers = sev_numbers,
                        sample = rbind(sample1, sample2, sample3))

# disability no bens (ch) ------------------------------------------------
rel_pp <- getpovby(hbai, pov = "low60ahc_dis", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_pp <- getpovby(hbai, pov = "low50ahc_dis", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ch <- getpovby(hbai, pov = "low60ahc_dis", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ch <- getpovby(hbai, pov = "low50ahc_dis", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

rel_ad <- getpovby(hbai, pov = "low60ahc_dis", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev_ad <- getpovby(hbai, pov = "low50ahc_dis", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

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

rel_numbers <- rbind(rel_numbers1, rel_numbers2, rel_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

sev_numbers <- rbind(sev_numbers1, sev_numbers2, sev_numbers3) %>%
  mutate("2010-13" = ifelse(Group == "All", get("2010-13"), 99991),
         "2011-14" = ifelse(Group == "All", get("2011-14"), 99991))

tables$disab_nobens_ch <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                               rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                               rel_numbers = rel_numbers,
                               sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                               sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                               sev_numbers = sev_numbers,
                               sample = rbind(sample1, sample2, sample3))

# ethgrphh (ch) ----------------------------------------------------------

rel <- hbai %>%
  filter(ethgrphh != "(Missing)",
         yearn >= 8) %>%
  mutate(ethgrphh = fct_drop(ethgrphh)) %>%
  getpovby(by = "ethgrphh", weight = "gs_newch") %>%
  summarise_data(5, na.rm = TRUE)

sev <- hbai %>%
  filter(ethgrphh != "(Missing)",
         yearn >= 8) %>%
  mutate(ethgrphh = fct_drop(ethgrphh)) %>%
  getpovby(pov = "low50ahc", by = "ethgrphh", weight = "gs_newch") %>%
  summarise_data(5, na.rm = TRUE)

age <- hbai %>%
  rbind(hbai %>% filter(ethgrphh != "(Missing)") %>% mutate(ethgrphh = "All")) %>%

  # get hrp info
  left_join(adult %>%
              filter(hrpid == 1, gvtregn == "Scotland") %>%
              select(sernum, benunit, hrpid),
            by = c("sernum", "benunit")) %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)",
         hrpid == 1,
         gs_newch > 0) %>%
  group_by(yearn, ethgrphh) %>%
  summarise(age = wtd.median(agehd, gs_newbu),
            sample = sum(gs_newbu > 0, na.rm = TRUE)) %>%
  group_by(ethgrphh) %>%
  mutate(age = analysistools::getrollingmean(age, 5, na.rm = TRUE),
         sample = analysistools::getrollingtotal(sample, 5, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yearn >= 12) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = get_periods(yearn, n = 5),
         ethgrphh = factor(ethgrphh,
                           levels = c("All", "White - British",
                                      "White - Other",
                                      "Asian or Asian British",
                                      "Mixed, Black or Black British, and Other"))) %>%
  select(year, age, ethgrphh) %>%
  arrange(ethgrphh) %>%
  pivot_wider(names_from = year, values_from = age) %>%
  rename(Group = ethgrphh)

tables$ethgrphh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),
                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           age = age,
                           sample = splitntranspose(rel, "sample"))

# ethgrphh_2f (ch) ------------------------------------------------------------------

rel <- hbai %>%
  filter(ethgrphh_2f != "(Missing)",
         yearn >= 8) %>%
  mutate(ethgrphh_2f = fct_drop(ethgrphh_2f)) %>%
  getpovby(by = "ethgrphh_2f", weight = "gs_newch") %>%
  summarise_data(na.rm = TRUE)

sev <- hbai %>%
  filter(ethgrphh_2f != "(Missing)",
         yearn >= 8) %>%
  mutate(ethgrphh_2f = fct_drop(ethgrphh_2f)) %>%
  getpovby(pov = "low50ahc", by = "ethgrphh_2f", weight = "gs_newch") %>%
  summarise_data(na.rm = TRUE)

age <- hbai %>%
  rbind(hbai %>% filter(ethgrphh_2f != "(Missing)") %>% mutate(ethgrphh_2f = "All")) %>%
  # get hrp info
  left_join(adult %>%
              filter(hrpid == 1, gvtregn == "Scotland") %>%
              select(sernum, benunit, hrpid),
            by = c("sernum", "benunit")) %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)",
         hrpid == 1,
         gs_newch > 0) %>%
  group_by(yearn, ethgrphh_2f) %>%
  summarise(age = wtd.median(agehd, gs_newbu),
            sample = sum(gs_newbu > 0, na.rm = TRUE)) %>%
  group_by(ethgrphh_2f) %>%
  mutate(age = analysistools::getrollingmean(age, 3, na.rm = TRUE),
         sample = analysistools::getrollingtotal(sample, 3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yearn >= 10) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = get_periods(yearn, n = 5),
         ethgrphh_2f = factor(ethgrphh_2f,
                           levels = c("All", "White - British",
                                      "White - Other",
                                      "Minority ethnic"))) %>%
  select(year, age, ethgrphh_2f) %>%
  arrange(ethgrphh_2f) %>%
  pivot_wider(names_from = year, values_from = age) %>%
  rename(Group = ethgrphh_2f)

tables$ethgrphh_2f_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                              rel_comps = splitntranspose(rel, "composition"),
                              rel_numbers = splitntranspose(rel, "number"),
                              sev_rates = splitntranspose(sev, "rate"),
                              sev_comps = splitntranspose(sev, "composition"),
                              sev_numbers = splitntranspose(sev, "number"),
                              age = age,
                              sample = splitntranspose(rel, "sample"))

# priority groups (ch) --------------------------------------------------------------
# JUST DO LATEST PERIOD

hbai3yr <- filter(hbai, yearn >= max(yearn) - 2)

# all children
all_rel <- filter(tables$relAHC$rates, Group == "Children")[[max(periods)]]
all_abs <- filter(tables$absAHC$rates, Group == "Children")[[max(periods)]]
all_cmd <- filter(tables$cmd$rates,
                  Measure == "New measure, after housing costs")[[max(periods)]]

# disabled
dis_rel <- filter(tables$disab_ch$rel_rates,
                  Group == "In household with disabled person(s)")[[max(periods)]]

dis_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "dispp_hh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "In household with disabled person(s)") %>%
  pull(rate)

dis_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "dispp_hh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "In household with disabled person(s)") %>%
  pull(rate)

# 3+ children
many_rel <- filter(tables$depchldh_ch$rel_rates,
                  Group == "3 or more children in the household")[[max(periods)]]

many_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "depchldh_ch", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "3 or more children in the household") %>%
  pull(rate)

many_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "depchldh_ch", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "3 or more children in the household") %>%
  pull(rate)

# Baby
baby_rel <- filter(tables$baby_ch$rel_rates,
                   Group == "Youngest child is younger than 1")[[max(periods)]]

baby_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "babyhh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Youngest child is younger than 1") %>%
  pull(rate)

baby_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "babyhh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Youngest child is younger than 1") %>%
  pull(rate)

# Ethnic
eth_rel <- filter(tables$ethgrphh_2f_ch$rel_rates,
                   Group == "Minority ethnic")[[max(periods)]]

eth_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "ethgrphh_2f", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Minority ethnic") %>%
  pull(rate)

eth_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "ethgrphh_2f", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Minority ethnic") %>%
  pull(rate)

# Lone parent
lone_rel <- filter(tables$loneparenthh_ch$rel_rates,
                  Group == "Single parent in household")[[max(periods)]]

lone_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "loneparenthh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Single parent in household") %>%
  pull(rate)

lone_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "loneparenthh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  getnyrtable(na.rm = TRUE) %>%
  samplesizecheck() %>%
  filter(groupingvar == "Single parent in household") %>%
  pull(rate)

# note that sample sizes are too small for last group - mothers under 25 in hhld

groups <- c("All children",
            "3 or more children in the household",
            "Disabled household member(s)",
            "Youngest child in the household is under 1",
            "Minority ethnic household",
            "Single parent in the household",
            "Mother under 25 in household")

rel <- data.frame(Group = groups,
                  Rate = c(all_rel, many_rel, dis_rel, baby_rel, eth_rel,
                           lone_rel, NA))

abs <- data.frame(Group = groups,
                  Rate = c(all_abs, many_abs, dis_abs, baby_abs, eth_abs,
                           lone_abs, NA))

cmd <- data.frame(Group = groups,
                  Rate = c(all_cmd, many_cmd, dis_cmd, baby_cmd, eth_cmd,
                           lone_cmd, NA))


sample <- data.frame(Group = c("All children",
                               "3 or more children in the household",
                               "Disabled household member(s)",
                               "Youngest child in the household is under 1",
                               "Minority ethnic household",
                               "Single parent in the household",
                               "Mother under 25 in household"),
                     Sample = c(filter(tables$relAHC$sample, Group == "Children")[[max(periods)]],
                                filter(tables$depchldh_ch$sample, Group == "3 or more children in the household")[[max(periods)]],

                                filter(tables$disab_ch$sample, Group == "In household with disabled person(s)")[[max(periods)]],
                                filter(tables$baby_ch$sample, Group == "Youngest child is younger than 1")[[max(periods)]],
                                filter(tables$ethgrphh_2f_ch$sample, Group == "Minority ethnic")[[max(periods)]],
                                filter(tables$loneparenthh_ch$sample, Group == "Single parent in household")[[max(periods)]],
                                filter(tables$youngmum_ch$sample, Group == "Mother under 25 in household")[[max(periods)]]   ))


tables$priority <- list(rel = rel,
                        abs = abs,
                        cmd = cmd,
                        sample = sample)

# :----------------------- -----------------------------------------------------
# ecobu (wa) -------------------------------------------------------------------

rel <- getpovby(hbai, by = "ecobu", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "ecobu", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

tables$ecobu_wa <- list(rel_rates = splitntranspose(rel, "rate"),
                        rel_comps = splitntranspose(rel, "composition"),
                        rel_numbers = splitntranspose(rel, "number"),

                        sev_rates = splitntranspose(sev, "rate"),
                        sev_comps = splitntranspose(sev, "composition"),
                        sev_numbers = splitntranspose(sev, "number"),
                        sample = splitntranspose(rel, "sample"))


# workinghh (wa) ---------------------------------------------------------------

rel <- getpovby(hbai, by = "workinghh", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "workinghh", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE) %>%
  filter(yearn >= 5)

tables$workinghh_wa <- list(rel_rates = splitntranspose(rel, "rate"),
                            rel_comps = splitntranspose(rel, "composition"),
                            rel_numbers = splitntranspose(rel, "number"),

                            sev_rates = splitntranspose(sev, "rate"),
                            sev_comps = splitntranspose(sev, "composition"),
                            sev_numbers = splitntranspose(sev, "number"),
                            sample = splitntranspose(rel, "sample"))

# :----------------------- -----------------------------------------------------
# newfambu (ad) ----------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "newfambu", weight = "gs_newad") %>%
  summarise_data(na.rm = TRUE)
sev <- getpovby(hbai, pov = "low50ahc", by = "newfambu", weight = "gs_newad") %>%
  summarise_data(na.rm = TRUE)

tables$newfambu_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# agebands (ad - discontinued) ----------------------------------------------------------------
# discontinued due to too many missing values in adult age variable

# sex binary (NEW) (ad) --------------------------------------------------------
# tables only? And numbers in report commentary for reference

rel <- adult %>%
  getpovby(by = "sex", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE)

sev <- adult %>%
  getpovby(pov = "low50ahc", by = "sex", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE)

tables$sex_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                      rel_comps = splitntranspose(rel, "composition"),
                      rel_numbers = splitntranspose(rel, "number"),

                      sev_rates = splitntranspose(sev, "rate"),
                      sev_comps = splitntranspose(sev, "composition"),
                      sev_numbers = splitntranspose(sev, "number"),
                      sample = splitntranspose(rel, "sample"))

# sex / singlehh (ad) ------------------------------------------------------

rel <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(by = "singlehh", weight = "gs_newad") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "singlehh", weight = "gs_newad") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

tables$singlehh_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))



# sexual identity (NEW) (ad) ------------------------------------------------
# decide: include missings as category? if not, set lump_n to 1

rel <- filter(adult, yearn >= 18) %>% #filter(adult, sidqn != "(Missing)") %>%
  mutate(sidqn = fct_lump_n(sidqn, 2)) %>%
  getpovby(by = "sidqn", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "Heterosexual / straight",
                                         "Other", "(Missing)"))) %>%
  arrange(groupingvar)

sev <- filter(adult, yearn >= 18) %>% #filter(adult, sidqn != "(Missing)") %>%
  mutate(sidqn = fct_lump_n(sidqn, 2)) %>%
  getpovby(pov = "low50ahc", by = "sidqn", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "Heterosexual / straight",
                                         "Other", "(Missing)"))) %>%
  arrange(groupingvar)

age <- adult %>%
  mutate(sidqn = fct_lump_n(sidqn, 2)) %>%
  rbind(adult %>% mutate(sidqn = "All")) %>%
  filter(yearn >= 18,
         # exclude bad data year
         yearn != 27) %>%

  group_by(yearn, sidqn) %>%
  summarise(age = wtd.median(age, adultwgt),
            sample = sum(adultwgt > 0, na.rm = TRUE)) %>%

  # add year back in
  full_join(data.frame(yearn = 27,
                       sidqn = unique(.$sidqn))) %>%
  arrange(yearn) %>%

  group_by(sidqn) %>%
  mutate(age = analysistools::getrollingmean(age, 3, na.rm = TRUE),
         sample = analysistools::getrollingtotal(sample, 3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yearn >= 20) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = get_periods(yearn)) %>%
  select(year, age, sidqn) %>%
  arrange(sidqn) %>%
  pivot_wider(names_from = year, values_from = age) %>%
  rename(Group = sidqn) %>%
  mutate(Group = factor(Group, levels = c("All", "Heterosexual / straight",
                                          "Other", "(Missing)"))) %>%
  arrange(Group)

tables$sexid_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                        rel_comps = splitntranspose(rel, "composition"),
                        rel_numbers = splitntranspose(rel, "number"),

                        sev_rates = splitntranspose(sev, "rate"),
                        sev_comps = splitntranspose(sev, "composition"),
                        sev_numbers = splitntranspose(sev, "number"),

                        age = age,
                        sample = splitntranspose(rel, "sample"))

# marital (ad) -------------------------------------------------------------

rel <- getpovby(adult, by = "marital", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE)

sev <- getpovby(adult, pov = "low50ahc", by = "marital", weight = "adultwgt") %>%
  summarise_data(na.rm = TRUE)

tables$marital_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# religsc (ad) -------------------------------------------------------------

rel <- adult %>%
  filter(religsc != "(Missing)") %>%
  mutate(religsc = fct_drop(religsc)) %>%
  getpovby(by = "religsc", weight = "adultwgt") %>%
  summarise_data(5, na.rm = TRUE)

sev <- adult %>%
  filter(religsc != "(Missing)") %>%
  mutate(religsc = fct_drop(religsc)) %>%
  getpovby(pov = "low50ahc", by = "religsc", weight = "adultwgt") %>%
  summarise_data(5, na.rm = TRUE)

age <- adult %>%
  rbind(adult %>% filter(religsc != "(Missing)") %>% mutate(religsc = "All")) %>%
  filter(religsc != "(Missing)",
         # exclude bad data year
         yearn != 27) %>%
  group_by(yearn, religsc) %>%
  summarise(age = wtd.median(age, adultwgt),
            sample = sum(adultwgt > 0, na.rm = TRUE)) %>%

  # add year back in
  full_join(data.frame(yearn = 27,
                       religsc = unique(.$religsc))) %>%
  arrange(yearn) %>%

  group_by(religsc) %>%
  mutate(age = analysistools::getrollingmean(age, 5, na.rm = TRUE),
         sample = analysistools::getrollingtotal(sample, 5, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(yearn >= 22) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = get_periods(yearn, n = 5),
         religsc = factor(religsc,
                          levels = c("All",
                                     "Church of Scotland",
                                     "Roman Catholic",
                                     "Other Christian",
                                     "Muslim",
                                     "Other religion",
                                     "No religion"))) %>%
  select(year, age, religsc) %>%
  arrange(religsc) %>%
  pivot_wider(names_from = year, values_from = age) %>%
  rename(Group = religsc)

tables$religsc_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),

                          age = age,
                          sample = splitntranspose(rel, "sample"))

# NOT USED - country of origin (ad) ----------------------------------------
# bit inconclusive and not a long time series, probably not to be included in
# report.

rel <- adult %>%
  filter(yearn >= 19,
         corign != "(Missing)") %>%
  getpovby(by = "corign", weight = "adultwgt") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

sev <- adult %>%
  filter(yearn >= 19,
         corign != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "corign", weight = "adultwgt") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data(na.rm = TRUE)

tables$corign_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                         rel_comps = splitntranspose(rel, "composition"),
                         rel_numbers = splitntranspose(rel, "number"),

                         sev_rates = splitntranspose(sev, "rate"),
                         sev_comps = splitntranspose(sev, "composition"),
                         sev_numbers = splitntranspose(sev, "number"),

                         sample = splitntranspose(rel, "sample"))


# :----------------------- -----------------------------------------------------

# food security ----------------------------------------------------------------

total_pp <- hbai %>%
  mutate(Group = gvtregn) %>%
  getfoodsecbyGroup() %>%
  filter(Group != "All") %>%
  mutate(Group = "People")

total_ch <- hbai %>%
  mutate(Group = gvtregn) %>%
  getfoodsecbyGroup(weight = "gs_newch") %>%
  filter(Group != "All") %>%
  mutate(Group = "Children")

total_wa <- hbai %>%
  mutate(Group = gvtregn) %>%
  getfoodsecbyGroup(weight = "gs_newwa") %>%
  filter(Group != "All") %>%
  mutate(Group = "Working-age adults")

total_pn <- hbai %>%
  mutate(Group = gvtregn) %>%
  getfoodsecbyGroup(weight = "gs_newpn") %>%
  filter(Group != "All") %>%
  mutate(Group = "Pensioners")

total <- rbind(total_pp, total_ch, total_wa, total_pn) %>%
  mutate(Group = factor(Group,
                        levels = c("People", "Children", "Working-age adults",
                                   "Pensioners"))) %>%
  arrange(Group)

total_comp <- select(total, Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

total_num <- select(total, Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

total_sample <- select(total, Group, Sample) %>%
  distinct()

tables$foodsec <- list(comps = total_comp, numbers = total_num,
                           sample = total_sample)

# food security - poverty pp ---------------------------------------------------

# relative poverty

pp_rel <- hbai %>%
  mutate(Group = low60ahc) %>%
  getfoodsecbyGroup() %>%
  filter(Group == 1) %>%
  mutate(Group = "People")

ch_rel <- hbai %>%
  mutate(Group = low60ahc) %>%
  getfoodsecbyGroup(weight = "gs_newch") %>%
  filter(Group == 1) %>%
  mutate(Group = "Children")

wa_rel <- hbai %>%
  mutate(Group = low60ahc) %>%
  getfoodsecbyGroup(weight = "gs_newwa") %>%
  filter(Group == 1) %>%
  mutate(Group = "Working-age adults")

pn_rel <- hbai %>%
  mutate(Group = low60ahc) %>%
  getfoodsecbyGroup(weight = "gs_newpn") %>%
  filter(Group == 1) %>%
  mutate(Group = "Pensioners")

# severe poverty

pp_sev <- hbai %>%
  mutate(Group = low50ahc) %>%
  getfoodsecbyGroup() %>%
  filter(Group == 1) %>%
  mutate(Group = "People")

ch_sev <- hbai %>%
  mutate(Group = low50ahc) %>%
  getfoodsecbyGroup(weight = "gs_newch") %>%
  filter(Group == 1) %>%
  mutate(Group = "Children")

wa_sev <- hbai %>%
  mutate(Group = low50ahc) %>%
  getfoodsecbyGroup(weight = "gs_newwa") %>%
  filter(Group == 1) %>%
  mutate(Group = "Working-age adults")

pn_sev <- hbai %>%
  mutate(Group = low50ahc) %>%
  getfoodsecbyGroup(weight = "gs_newpn") %>%
  filter(Group == 1) %>%
  mutate(Group = "Pensioners")

# combine all

rel <- rbind(pp_rel, ch_rel, wa_rel, pn_rel) %>%
  mutate(Group = factor(Group,
                        levels = c("People", "Children", "Working-age adults",
                                   "Pensioners"))) %>%
  arrange(Group)

sev <- rbind(pp_sev, ch_sev, wa_sev, pn_sev) %>%
  mutate(Group = factor(Group,
                        levels = c("People", "Children", "Working-age adults",
                                   "Pensioners"))) %>%
  arrange(Group)

rel_comp <- rel %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

rel_num <- rel %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

rel_sample <- rel %>%
  select(Group, Sample) %>%
  distinct()

sev_comp <- sev %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

sev_num <- sev %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sev_sample  <- sev %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_pov_pp <- list(rel_comp = rel_comp,
                           rel_num = rel_num,
                           rel_sample = rel_sample,
                           sev_comp = sev_comp,
                           sev_num = sev_num,
                           sev_sample = sev_sample)

# food security - hdage pp -----------------------------------------------------

fs <- hbai %>%
  mutate(Group = agehdband8) %>%
  getfoodsecbyGroup()

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_hdage_pp <- list(comp = comp,
                           num = num,
                           sample = sample)

# food security - hdsex pp -----------------------------------------------------

fs <- hbai %>%
  mutate(Group = factor(sexhd, levels = c(1, 2),
                        labels = c("Male", "Female"))) %>%
  getfoodsecbyGroup()

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_hdsex_pp <- list(comp = comp,
                             num = num,
                             sample = sample)


# food security - singlesex ad -------------------------------------------------

fs <- hbai %>%
  filter(singlehh != "(Missing)") %>%
  mutate(Group = singlehh) %>%
  getfoodsecbyGroup(weight = "gs_newad")

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993)) %>%

  # deal with special case which would otherwise show as [low]
  mutate(Marginal = ifelse(Group == "Male working-age adult with dependent children",
                           NA, Marginal))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_singlesex_ad <- list(comp = comp,
                                    num = num,
                                    sample = sample)

# food security - sexual identity ad -------------------------------------------

fs <- adult %>%
  mutate(Group = fct_lump_n(sidqn, 2),
         Group = factor(Group, levels = c("All", "Heterosexual / straight",
                                          "Other", "(Missing)"))) %>%
  getfoodsecbyGroup(weight = "adultwgt")

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_sexid_ad <- list(comp = comp,
                             num = num,
                             sample = sample)

# food security - disability pp ------------------------------------------------

fs <- hbai %>%
  mutate(Group = dispp_hh) %>%
  getfoodsecbyGroup()

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_dis_pp <- list(comp = comp,
                           num = num,
                           sample = sample)

# food security - hd ethnic pp -------------------------------------------------

fs <- hbai %>%
  filter(ethgrphh != "(Missing)") %>%
  mutate(Group = ethgrphh) %>%
  getfoodsecbyGroup()

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_ethnic_pp <- list(comp = comp,
                             num = num,
                             sample = sample)

# food security - religion ad --------------------------------------------------

fs <- adult %>%
  filter(religsc != "(Missing)") %>%
  mutate(Group = fct_drop(religsc)) %>%
  getfoodsecbyGroup(weight = "adultwgt")

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_religion_ad <- list(comp = comp,
                             num = num,
                             sample = sample)

# food security - family type ad -----------------------------------------------
# (for parenthood)

fs <- hbai %>%
  mutate(Group = newfambu) %>%
  getfoodsecbyGroup()

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_newfambu_ad <- list(comp = comp,
                                    num = num,
                                    sample = sample)


# food security - marital ad ---------------------------------------------------
# marriage is protected characteristic

fs <- adult %>%
  mutate(Group = marital) %>%
  getfoodsecbyGroup(weight = "adultwgt")

comp <- fs %>%
  select(Group, foodsec, composition) %>%
  pivot_wider(names_from = foodsec, values_from = composition,
              # mark missing combinations due to no cases
              values_fill = list(composition = 99993))

num <- fs %>%
  select(Group, foodsec, number) %>%
  pivot_wider(names_from = foodsec, values_from = number)

sample <- fs %>%
  select(Group, Sample) %>%
  distinct()

tables$foodsec_marital_ad <- list(comp = comp,
                                num = num,
                                sample = sample)


# :----------------------- -----------------------------------------------------
# medians ----------------------------------------------------------------------

bhc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  summarise(pp = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                              weights = gs_newpp),
            ch = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                              weights = gs_newch),
            wa = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                              weights = gs_newwa),
            pn = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                              weights = gs_newpn),
            pp_sample = sum(gs_newpp > 0, na.rm = TRUE),
            ch_sample = sum(gs_newch > 0, na.rm = TRUE),
            wa_sample = sum(gs_newwa > 0, na.rm = TRUE),
            pn_sample = sum(gs_newpn > 0, na.rm = TRUE )) %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate_at(vars(c("pp", "ch", "wa", "pn")),
            analysistools::getrollingmean, na.rm = TRUE) %>%
  mutate_at(vars(contains("sample")), analysistools::getrollingtotal, na.rm = TRUE) %>%
  mutate(year = get_periods(yearn)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L)

numbers_pp <- select(bhc, year, pp) %>%
  pivot_wider(names_from = year, values_from = pp) %>%
  mutate(Group = "All people")

numbers_ch <- select(bhc, year, ch) %>%
  pivot_wider(names_from = year, values_from = ch) %>%
  mutate(Group = "Children")

numbers_wa <- select(bhc, year, wa) %>%
  pivot_wider(names_from = year, values_from = wa) %>%
  mutate(Group = "Working-age adults")

numbers_pn <- select(bhc, year, pn) %>%
  pivot_wider(names_from = year, values_from = pn) %>%
  mutate(Group = "Pensioners")

sample_pp <- select(bhc, year, pp_sample) %>%
  pivot_wider(names_from = year, values_from = pp_sample) %>%
  mutate(Group = "All people")

sample_ch <- select(bhc, year, ch_sample) %>%
  pivot_wider(names_from = year, values_from = ch_sample) %>%
  mutate(Group = "Children")

sample_wa <- select(bhc, year, wa_sample) %>%
  pivot_wider(names_from = year, values_from = wa_sample) %>%
  mutate(Group = "Working-age adults")

sample_pn <- select(bhc, year, pn_sample) %>%
  pivot_wider(names_from = year, values_from = pn_sample) %>%
  mutate(Group = "Pensioners")

numbers_bhc <- rbind(numbers_pp, numbers_ch, numbers_wa, numbers_pn) %>%
  select(Group, everything())

ahc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  summarise(pp = analysistools::wtd.median(s_oe_ahc * ahcpubdef / ahcyrdef,
                              weights = gs_newpp),
            ch = analysistools::wtd.median(s_oe_ahc * ahcpubdef / ahcyrdef,
                              weights = gs_newch),
            wa = analysistools::wtd.median(s_oe_ahc * ahcpubdef / ahcyrdef,
                              weights = gs_newwa),
            pn = analysistools::wtd.median(s_oe_ahc * ahcpubdef / ahcyrdef,
                              weights = gs_newpn),
            pp_sample = sum(gs_newpp > 0, na.rm = TRUE),
            ch_sample = sum(gs_newch > 0, na.rm = TRUE),
            wa_sample = sum(gs_newwa > 0, na.rm = TRUE),
            pn_sample = sum(gs_newpn > 0, na.rm = TRUE )) %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate_at(vars(c("pp", "ch", "wa", "pn")), analysistools::getrollingmean, na.rm = TRUE) %>%
  mutate_at(vars(contains("sample")), analysistools::getrollingtotal, na.rm = TRUE) %>%
  mutate(year = get_periods(yearn)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L)

numbers_pp <- select(ahc, year, pp) %>%
  pivot_wider(names_from = year, values_from = pp) %>%
  mutate(Group = "All people")

numbers_ch <- select(ahc, year, ch) %>%
  pivot_wider(names_from = year, values_from = ch) %>%
  mutate(Group = "Children")

numbers_wa <- select(ahc, year, wa) %>%
  pivot_wider(names_from = year, values_from = wa) %>%
  mutate(Group = "Working-age adults")

numbers_pn <- select(ahc, year, pn) %>%
  pivot_wider(names_from = year, values_from = pn) %>%
  mutate(Group = "Pensioners")

sample_pp <- select(ahc, year, pp_sample) %>%
  pivot_wider(names_from = year, values_from = pp_sample) %>%
  mutate(Group = "All people")

sample_ch <- select(ahc, year, ch_sample) %>%
  pivot_wider(names_from = year, values_from = ch_sample) %>%
  mutate(Group = "Children")

sample_wa <- select(ahc, year, wa_sample) %>%
  pivot_wider(names_from = year, values_from = wa_sample) %>%
  mutate(Group = "Working-age adults")

sample_pn <- select(ahc, year, pn_sample) %>%
  pivot_wider(names_from = year, values_from = pn_sample) %>%
  mutate(Group = "Pensioners")

numbers_ahc <- rbind(numbers_pp, numbers_ch, numbers_wa, numbers_pn) %>%
  select(Group, everything())

sample <- rbind(sample_pp, sample_ch, sample_wa, sample_pn) %>%
  select(Group, everything())

tables$medians <- list(bhc = numbers_bhc,
                       ahc = numbers_ahc,
                       sample = sample)

# decile points ----------------------------------------------------------------

bhc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  getdecptsbhc() %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate(year = get_periods(yearn)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~analysistools::getrollingmean(., na.rm = TRUE)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  pivot_wider(names_from = year, values_from = value)

ahc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  getdecptsahc() %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate(year = get_periods(yearn)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~analysistools::getrollingmean(., na.rm = TRUE)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  pivot_wider(names_from = year, values_from = value)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  mutate(sample = analysistools::getrollingtotal(sample, 3, na.rm = TRUE),
         year = get_periods(yearn),
         Group = "All") %>%
  filter(yearn >= 3) %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$decilepoints <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)

# decile shares ----------------------------------------------------------------

bhc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_bhc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_bhc * gs_newpp) * bhcpubdef[1] / bhcyrdef[1] * 365/7) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(Decile) %>%
  mutate(share = analysistools::getrollingmean(share, na.rm = TRUE),
         share = round2(share / 1000000, 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(year, share, Decile) %>%
  pivot_wider(names_from = year, values_from = share)

ahc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_ahc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_ahc * gs_newpp) * ahcpubdef[1] / ahcyrdef[1] * 365/7) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(Decile) %>%
  mutate(share = analysistools::getrollingmean(share, na.rm = TRUE),
         share = round2(share / 1000000, 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(year, share, Decile) %>%
  pivot_wider(names_from = year, values_from = share)

sample <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_bhc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(Decile) %>%
  mutate(sample = analysistools::getrollingtotal(sample, 3, na.rm = TRUE),
         year = get_periods(yearn)) %>%
  filter(yearn >= 3) %>%
  select(-yearn) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$decileshares <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)

# Palma & Gini -----------------------------------------------------------------

palma_bhc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_bhc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_bhc * gs_newpp)) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = analysistools::getrollingmean(Palma, na.rm = TRUE),
         Palma = roundpct(Palma),
         Measure = "Before housing costs") %>%
  filter(Decile == 10,
         yearn >= 3) %>%
  ungroup() %>%
  mutate(year = get_periods(yearn)) %>%
  select(Measure, year, Palma) %>%
  pivot_wider(names_from = year, values_from = Palma)

palma_ahc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_ahc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_ahc * gs_newpp)) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = analysistools::getrollingmean(Palma, na.rm = TRUE),
         Palma = roundpct(Palma),
         Measure = "After housing costs") %>%
  filter(Decile == 10,
         yearn >= 3) %>%
  ungroup() %>%
  mutate(year = get_periods(yearn)) %>%
  select(Measure, year, Palma) %>%
  pivot_wider(names_from = year, values_from = Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = analysistools::getrollingmean(Gini, na.rm = TRUE),
         Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(-yearn) %>%
  pivot_wider(names_from = year, values_from = Gini)

gini_ahc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = analysistools::getrollingmean(Gini, na.rm = TRUE),
         Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(-yearn) %>%
  pivot_wider(names_from = year, values_from = Gini)

gini <- rbind(gini_bhc, gini_ahc)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  mutate(sample = analysistools::getrollingtotal(sample, 3, na.rm = TRUE),
         year = get_periods(yearn),
         Group = "All") %>%
  filter(yearn >= 3) %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$palmagini <- list(palma = palma,
                         gini = gini,
                         sample = sample)

# poverty thresholds -----------------------------------------------------------
# ADAPTED TO BAD DATA ISSUE - Shousl usually be a three-year average

hbai1 <- filter(hbai, yearn == max(yearn))
hbai2 <- filter(hbai, yearn == max(yearn) - 1)
hbai3 <- filter(hbai, yearn == max(yearn) - 2)

bhc1 <- getpovertythresholdsbhc(hbai1)
#bhc2 <- getpovertythresholdsbhc(hbai2)
bhc3 <- getpovertythresholdsbhc(hbai3)

weekly_bhc <- (bhc1[, 2:9] + bhc3[, 2:9]) / 2
weekly_bhc$Measure <- bhc1$Measure

weekly_bhc <- weekly_bhc %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_bhc <- (bhc1[, 2:9] + bhc3[, 2:9]) / 2
annual_bhc$Measure <- bhc1$Measure

annual_bhc <- annual_bhc %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

ahc1 <- getpovertythresholdsahc(hbai1)
#ahc2 <- getpovertythresholdsahc(hbai2)
ahc3 <- getpovertythresholdsahc(hbai3)

weekly_ahc <- (ahc1[, 2:9] + ahc3[, 2:9]) / 2
weekly_ahc$Measure <- ahc1$Measure

weekly_ahc <- weekly_ahc %>%
  select(Measure, weekly1, weekly2, weekly3, weekly4) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  rename("Single person with no children" = weekly1,
         "Couple with no children" = weekly2,
         "Single person with children aged 5 and 14" = weekly3,
         "Couple with children aged 5 and 14" = weekly4)

annual_ahc <- (ahc1[, 2:9] + ahc3[, 2:9]) / 2
annual_ahc$Measure <- ahc1$Measure

annual_ahc <- annual_ahc %>%
  select(Measure, annual1, annual2, annual3, annual4) %>%
  mutate_if(is.numeric, ~round2(., -2)) %>%
  rename("Single person with no children" = annual1,
         "Couple with no children" = annual2,
         "Single person with children aged 5 and 14" = annual3,
         "Couple with children aged 5 and 14" = annual4)

tables$thresholds <- list(weekly_bhc = weekly_bhc,
                          annual_bhc = annual_bhc,
                          weekly_ahc = weekly_ahc,
                          annual_ahc = annual_ahc)

# income sources ---------------------------------------------------------------
# ADAPTED TO BAD DATA ISSUE - Shousl usually be a three-year average

hbai1 <- filter(hbai, yearn == max(yearn))
hbai2 <- filter(hbai, yearn == max(yearn) - 1)
hbai3 <- filter(hbai, yearn == max(yearn) - 2)

df1 <- getsources(hbai1)
#df2 <- getsources(hbai2)
df3 <- getsources(hbai3)

df <- data.frame(df1[1])
df[2] <- (df1[2] + df3[2])/2
df[3] <- (df1[3] + df3[3])/2
df[4] <- (df1[4] + df3[4])/2
df[5] <- (df1[5] + df3[5])/2
df[6] <- (df1[6] + df3[6])/2

sources <- df %>%
  mutate(Decile = fct_relevel(Decile, "All", after = 0L)) %>%
  mutate_if(is.numeric, roundpct) %>%
  arrange(Decile)

tables$sources <- list(sources = sources)

# distribution -----------------------------------------------------------------
# ADAPTED TO BAD DATA

tables$distribution <- list()

tables$distribution$dist <- hbai %>%
  filter(yearn >= max(yearn) - 2,
         yearn != 27) %>%
  group_by(yearn) %>%
  mutate(income = s_oe_bhc * bhcpubdef / bhcyrdef) %>%
  select(yearn, gs_newpp, income) %>%
  ungroup()

tables$distribution$distdecs <- hbai %>%
  filter(yearn >= max(yearn) - 2,
         yearn != 27) %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  gather(x, value, -yearn) %>%
  group_by(x) %>%
  # ADAPTED TO BAD DATA
  mutate(value = analysistools::getrollingmean(value, 2)) %>%
  ungroup() %>%
  filter(yearn == max(yearn)) %>%

  mutate(xpos = lag(value) + 1/2*(value - lag(value)),
         xpos = ifelse(x == "1", value/2 + 50, xpos),
         xpos = ifelse(x == "10", (lag(value) + 50), xpos)) %>%
  select(-yearn)

tables$distribution$distthresh <- hbai %>%
  filter(yearn >= max(yearn) - 2,
         yearn != 27) %>%
  group_by(yearn) %>%
  summarise(UKmedian = max(mdoebhc * bhcpubdef / bhcyrdef),
            Scotmedian = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                                      weights = gs_newpp),
            povthresh = 0.6 * UKmedian) %>%
  ungroup() %>%
  summarise(UKmedian = mean(UKmedian),
            Scotmedian = mean(Scotmedian),
            povthresh = mean(povthresh))

# :----------------------- -----------------------------------------------------
# median ages ------------------------------------------------------------------

# gender (pensioners)
gender <- hbai %>%
  filter(singlehh %in% c("Female pensioner", "Male pensioner"),
         yearn >= max(yearn) - 2,
         yearn != 27) %>%
  mutate(over79 = ifelse(agehd >= 80, 1 , 0)) %>%
  group_by(singlehh, over79) %>%
  summarise(pensioners = sum(gs_newad)) %>%
  group_by(singlehh) %>%
  mutate(share = pensioners/sum(pensioners),
         share = fmtpct(share)) %>%
  filter(over79 == 1) %>%
  select(singlehh, share)

# ethnicity
ethnic <- hbai %>%
  filter(yearn >= max(yearn) - 4,
         yearn != 27,
         benunit == 1) %>%
  group_by(ethgrphh) %>%
  summarise(age = wtd.median(agehd, gs_newbu))

# religion
religion <- adult %>%
  filter(yearn >= max(yearn) - 4,
         yearn != 27) %>%
  group_by(religsc) %>%
  summarise(age = wtd.median(age, adultwgt))

tables$median_ages <- list(gender = gender,
                           ethnic = ethnic,
                           religion = religion)

# equivalence scale ------------------------------------------------------------
tables$equivalence <- data.frame(Member = c("First adult",
                                            "Spouse",
                                            "Additional adults",
                                            "Children aged 0-13",
                                            "Children aged 14+"),
                                 BHC = c(0.67,
                                         0.33,
                                         0.33,
                                         0.20,
                                         0.33),
                                 AHC = c(0.58,
                                         0.42,
                                         0.42,
                                         0.20,
                                         0.42))

names(tables$equivalence) <- c("Household member", "Before housing costs",
                                "After housing costs")

# :----------------------- -----------------------------------------------------
# save all ---------------------------------------------------------------------
saveRDS(tables, "data/tables.rds")
rm(list = ls())

cat("3 yr Scot data aggregated", fill = TRUE)



