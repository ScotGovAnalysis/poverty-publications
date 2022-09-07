# prelims ----------------------------------------------------------------------
library(tidyverse)
library(analysistools)
source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds") %>% filter(gvtregn == "Scotland")
adult <- readRDS("data/tidyadult.rds") %>% filter(gvtregn == "Scotland")

# on re-run, use existing dataset
# (but note that any new elements will be added at the end)
if (file.exists("data/tables.rds")) {
  tables <- readRDS("data/tables.rds")
} else {tables <- list()}

latestyear <- max(levels(labels$years$formatted))
periods <- levels(labels$years$periods)

# :----------------------- -----------------------------------------------------
# 1 rel ------------------------------------------------------------------------
tables$relAHC <- list(rates = getheadlines(hbai, pov = "low60ahc",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60ahc",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60ahc",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60ahc",
                                            threeyr = TRUE)[["sample"]])

# 2 abs ------------------------------------------------------------------------
tables$absAHC <- list(rates = getheadlines(hbai, pov = "low60ahcabs",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60ahcabs",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60ahcabs",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60ahcabs",
                                            threeyr = TRUE)[["sample"]])

# 3 sev ------------------------------------------------------------------------
tables$sevAHC <- list(rates = getheadlines(hbai, pov = "low50ahc",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low50ahc",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low50ahc",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low50ahc",
                                            threeyr = TRUE)[["sample"]])

# 4 rel BHC --------------------------------------------------------------------
tables$relBHC <- list(rates = getheadlines(hbai, pov = "low60bhc",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60bhc",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60bhc",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60bhc",
                                            threeyr = TRUE)[["sample"]])

# 5 abs BHC --------------------------------------------------------------------
tables$absBHC <- list(rates = getheadlines(hbai, pov = "low60bhcabs",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low60bhcabs",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low60bhcabs",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low60bhcabs",
                                            threeyr = TRUE)[["sample"]])

# 6 sev BHC --------------------------------------------------------------------
tables$sevBHC <- list(rates = getheadlines(hbai, pov = "low50bhc",
                                           threeyr = TRUE)[["rates"]],
                      comps = getheadlines(hbai, pov = "low50bhc",
                                           threeyr = TRUE)[["comps"]],
                      numbers = getheadlines(hbai, pov = "low50bhc",
                                             threeyr = TRUE)[["numbers"]],
                      sample = getheadlines(hbai, pov = "low50bhc",
                                            threeyr = TRUE)[["sample"]])

# 7 cmd ------------------------------------------------------------------------
cmd_ahc <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_ahc_new <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdahc_new", weight = "gs_newch") %>%
  rbind(cmd_ahc) %>%
  mutate(Measure = "New measure, after housing costs")
cmd_ahc <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdahc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, after housing costs") %>%
  rbind(cmd_ahc_new) %>%
  group_by(Measure) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd_bhc <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn >= 18)
cmd_bhc_new <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdbhc_new", weight = "gs_newch") %>%
  rbind(cmd_bhc) %>%
  mutate(Measure = "New measure, before housing costs")
cmd_bhc <- filter(hbai, gs_newch > 0) %>%
  getpovby(pov = "cmdbhc", weight = "gs_newch") %>%
  filter(yearn <= 17) %>%
  mutate(Measure = "Old measure, before housing costs") %>%
  rbind(cmd_bhc_new) %>%
  group_by(Measure) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  filter(type != "cmdahc_new")

cmd <- rbind(cmd_ahc, cmd_bhc)  %>%
  mutate(year = get_periods(yearn))

rates <- cmd  %>%
  select(year, rate, Measure) %>%
  spread(year, rate) %>%
  mutate("2009-12" = NA) %>%
  select(1:6, "2009-12", everything())
numbers <- cmd %>%
  select(year, number, Measure) %>%
  spread(year, number) %>%
  mutate("2009-12" = NA) %>%
  select(1:6, "2009-12", everything())
sample <- cmd %>%
  select(year, sample, Measure) %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  spread(year, sample)  %>%
  mutate("2009-12" = NA) %>%
  select(1:6, "2009-12", everything()) %>%
  mutate(Measure = ifelse(Measure == "Old measure, after housing costs",
                          "Old measure", "New measure"))

tables$cmd <- list(rates = rates,
                   numbers = numbers,
                   sample = sample)

# 8 pmd ------------------------------------------------------------------------
pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older",
         year = get_periods(yearn))

tables$pmd <- list(rates = pmd %>%
                     select(Group, year, rate) %>%
                     spread(year, rate),
                   numbers = pmd %>%
                     select(Group, year, number) %>%
                     spread(year, number),
                   sample = pmd  %>%
                     select(Group, year, sample) %>%
                     spread(year, sample))
# :----------------------- -----------------------------------------------------
# 9 newfambu (adults) ----------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "newfambu", weight = "gs_newad") %>%
  summarise_data()
sev <- getpovby(hbai, pov = "low50ahc", by = "newfambu", weight = "gs_newad") %>%
  summarise_data()

tables$newfambu_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# 10 depchldh ------------------------------------------------------------------

rel <- getpovby(hbai, by = "depchldh") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(hbai, pov = "low50ahc", by = "depchldh") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$depchldh_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# 11 ecobu (working-age adults) ------------------------------------------------

rel <- getpovby(hbai, by = "ecobu", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "ecobu", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

tables$ecobu_wa <- list(rel_rates = splitntranspose(rel, "rate"),
                        rel_comps = splitntranspose(rel, "composition"),
                        rel_numbers = splitntranspose(rel, "number"),

                        sev_rates = splitntranspose(sev, "rate"),
                        sev_comps = splitntranspose(sev, "composition"),
                        sev_numbers = splitntranspose(sev, "number"),
                        sample = splitntranspose(rel, "sample"))

# 12 workinghh (working-age adults) --------------------------------------------

rel <- getpovby(hbai, by = "workinghh", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "workinghh", weight = "gs_newwa") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

tables$workinghh_wa <- list(rel_rates = splitntranspose(rel, "rate"),
                            rel_comps = splitntranspose(rel, "composition"),
                            rel_numbers = splitntranspose(rel, "number"),

                            sev_rates = splitntranspose(sev, "rate"),
                            sev_comps = splitntranspose(sev, "composition"),
                            sev_numbers = splitntranspose(sev, "number"),
                            sample = splitntranspose(rel, "sample"))

# 13 tenhbai -------------------------------------------------------------------

rel <- getpovby(hbai, by = "tenhbai") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 12)

sev <- getpovby(hbai, pov = "low50ahc", by = "tenhbai") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 12)

tables$tenhbai_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# 14 urinds --------------------------------------------------------------------

rel <- getpovby(hbai, by = "urinds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 15)

sev <- getpovby(hbai, pov = "low50ahc", by = "urinds") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 15)

tables$urinds_pp <- list(rel_rates = splitntranspose(rel, "rate"),
                         rel_comps = splitntranspose(rel, "composition"),
                         rel_numbers = splitntranspose(rel, "number"),

                         sev_rates = splitntranspose(sev, "rate"),
                         sev_comps = splitntranspose(sev, "composition"),
                         sev_numbers = splitntranspose(sev, "number"),
                         sample = splitntranspose(rel, "sample"))

# 15 agebands (adults) ---------------------------------------------------------

rel <- getpovby(adult, by = "ageband", weight = "adultwgt") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- getpovby(adult, pov = "low50ahc", by = "ageband", weight = "adultwgt") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$ageband_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# 16 sex / singlehh (adults) ---------------------------------------------------

rel <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(by = "singlehh", weight = "gs_newad") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev <- filter(hbai, singlehh != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "singlehh", weight = "gs_newad") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

tables$singlehh_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),

                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# 17 marital (adults) ----------------------------------------------------------

rel <- getpovby(adult, by = "marital", weight = "adultwgt") %>%
  summarise_data()

sev <- getpovby(adult, pov = "low50ahc", by = "marital", weight = "adultwgt") %>%
  summarise_data()

tables$marital_ad <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),

                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# 18 disability ----------------------------------------------------------------
rel_pp <- getpovby(hbai, by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_pp <- getpovby(hbai, pov = "low50ahc", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ch <- getpovby(hbai, by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ch <- getpovby(hbai, pov = "low50ahc", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ad <- getpovby(hbai, by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ad <- getpovby(hbai, pov = "low50ahc", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

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

tables$disab_pp <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                        rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                        rel_numbers = rbind(rel_numbers1, rel_numbers2, rel_numbers3),

                        sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                        sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                        sev_numbers = rbind(sev_numbers1, sev_numbers2, sev_numbers3),

                        sample = rbind(sample1, sample2, sample3))

# 19 disability no bens --------------------------------------------------------
rel_pp <- getpovby(hbai, pov = "low60ahc_dis", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_pp <- getpovby(hbai, pov = "low50ahc_dis", by = "dispp_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ch <- getpovby(hbai, pov = "low60ahc_dis", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ch <- getpovby(hbai, pov = "low50ahc_dis", by = "disch_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ad <- getpovby(hbai, pov = "low60ahc_dis", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ad <- getpovby(hbai, pov = "low50ahc_dis", by = "disad_hh") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

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

tables$disab_nobens_pp <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                               rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                               rel_numbers = rbind(rel_numbers1, rel_numbers2, rel_numbers3),

                               sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                               sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                               sev_numbers = rbind(sev_numbers1, sev_numbers2, sev_numbers3),

                               sample = rbind(sample1, sample2, sample3))
# 20 ethgrphh ------------------------------------------------------------------

rel <- getpovby(hbai, by = "ethgrphh") %>%
  filter(yearn >= 8,
         groupingvar != "(Missing)") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

sev <- getpovby(hbai, pov = "low50ahc", by = "ethgrphh") %>%
  filter(yearn >= 8,
         groupingvar != "(Missing)") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

age <- hbai %>%
  rbind(hbai %>% filter(ethgrphh != "(Missing)") %>% mutate(ethgrphh = "All")) %>%

  # get hrp info
  left_join(adult %>%
              filter(hrpid == 1, gvtregn == "Scotland") %>%
              select(sernum, benunit, hrpid),
            by = c("sernum", "benunit")) %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)",
         hrpid == 1) %>%
  group_by(yearn, ethgrphh) %>%
  summarise(age = wtd.median(agehd, gs_newbu),
            sample = n()) %>%
  group_by(ethgrphh) %>%
  mutate(age = getrollingmean(age, 5),
         sample = getrollingtotal(sample, 5)) %>%
  ungroup() %>%
  filter(yearn >= 12) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr),
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

# 21 religsc (adults) ----------------------------------------------------------

rel <- adult %>%
  filter(religsc != "(Missing)") %>%
  getpovby(by = "religsc", weight = "adultwgt") %>%
  filter(groupingvar != "(Missing)") %>%
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
                                         "No religion")))

sev <- adult %>%
  filter(religsc != "(Missing)") %>%
  getpovby(pov = "low50ahc", by = "religsc", weight = "adultwgt") %>%
  filter(groupingvar != "(Missing)") %>%
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
                                         "No religion")))

age <- adult %>%
  rbind(adult %>% filter(religsc != "(Missing)") %>% mutate(religsc = "All")) %>%
  filter(religsc != "(Missing)") %>%
  group_by(yearn, religsc) %>%
  summarise(age = wtd.median(age, adultwgt),
            sample = n()) %>%
  group_by(religsc) %>%
  mutate(age = getrollingmean(age, 5),
         sample = getrollingtotal(sample, 5)) %>%
  ungroup() %>%
  filter(yearn >= 22) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr),
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

# 22 food security -------------------------------------------------------------

total <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "All people") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         composition = mean(composition),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup() %>%
  mutate(foodsec = fct_rev(foodsec))

total_comp <- select(total, Group, foodsec, composition) %>%
  spread(foodsec, composition)
total_num <- select(total, Group, foodsec, number) %>%
  spread(foodsec, number)
total_sample <- select(total, Group, sample)[1, ]

relpov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahc == 1) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In relative poverty") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = mean(composition),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup()

relpov_comp <- select(relpov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
relpov_num <- select(relpov, Group, foodsec, number) %>%
  spread(foodsec, number)
relpov_sample <- select(relpov, Group, sample)[1, ]

abspov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahcabs == 1) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newpp),
         sample = sum(gs_newpp > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            sample = max(sample),
            povsample = sum(gs_newpp > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In absolute poverty") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = mean(composition),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup()

abspov_comp <- select(abspov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
abspov_num <- select(abspov, Group, foodsec, number) %>%
  spread(foodsec, number)
abspov_sample  <- select(abspov, Group, sample)[1, ]
sample <- rbind(total_sample, relpov_sample, abspov_sample) %>%
  rename(Sample = sample)

tables$foodsec_pp <- list(comps = rbind(total_comp, relpov_comp, abspov_comp),
                          numbers = rbind(total_num, relpov_num, abspov_num),
                          sample = sample)

# :----------------------- -----------------------------------------------------
# 23 loneparenthh --------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "loneparenthh", weight = "gs_newch") %>%
  summarise_data()

sev <- getpovby(hbai, pov = "low50ahc", by = "loneparenthh", weight = "gs_newch") %>%
  summarise_data()

tables$loneparenthh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                               rel_comps = splitntranspose(rel, "composition"),
                               rel_numbers = splitntranspose(rel, "number"),
                               sev_rates = splitntranspose(sev, "rate"),
                               sev_comps = splitntranspose(sev, "composition"),
                               sev_numbers = splitntranspose(sev, "number"),
                               sample = splitntranspose(rel, "sample"))

# 24 depchldh_ch ---------------------------------------------------------------

rel <- hbai %>%
  getpovby(pov = "low60ahc", by = "depchldh_ch", weight = "gs_newch") %>%
  filter(groupingvar != "No children in the household") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

sev <- hbai  %>%
  getpovby(pov = "low50ahc", by = "depchldh_ch", weight = "gs_newch") %>%
  filter(groupingvar != "No children in the household") %>%
  summarise_data() %>%
  mutate(groupingvar = fct_relevel(groupingvar, "All", after = 0L))

tables$depchldh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),
                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# 25 child age -----------------------------------------------------------------

rel0 <- getpovby(hbai, weight = "gs_newch") %>% get3yrtable()
rel1 <- getpovby(hbai, weight = "wgt0_4") %>% get3yrtable()
rel2 <- getpovby(hbai, weight = "wgt5_12") %>% get3yrtable()
rel3 <- getpovby(hbai, weight = "wgt13plus") %>% get3yrtable()

rel <- rbind(rel0, rel1, rel2, rel3) %>% mutate(year = get_periods(yearn))

sev0 <- getpovby(hbai, pov = "low50ahc", weight = "gs_newch") %>% get3yrtable()
sev1 <- getpovby(hbai, pov = "low50ahc", weight = "wgt0_4") %>% get3yrtable()
sev2 <- getpovby(hbai, pov = "low50ahc", weight = "wgt5_12") %>% get3yrtable()
sev3 <- getpovby(hbai, pov = "low50ahc", weight = "wgt13plus") %>% get3yrtable()

sev <- rbind(sev0, sev1, sev2, sev3) %>% mutate(year = get_periods(yearn))

rel_rates <- rel %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, rate) %>%
  spread(year, rate) %>%
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
  spread(year, composition) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

rel_numbers <- rel %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, number) %>%
  spread(year, number) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sev_rates <- sev %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, rate) %>%
  spread(year, rate) %>%
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
  spread(year, composition) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sev_numbers <- sev %>%
  samplesizecheck() %>%
  roundall() %>%
  select(weight, year, number) %>%
  spread(year, number) %>%
  rename(Group = weight) %>%
  mutate(Group = factor(Group,
                        levels = c("gs_newch", "wgt0_4", "wgt5_12", "wgt13plus"),
                        labels = c("All", "0-4", "5-12", "13-19"))) %>%
  arrange(Group)

sample <- rel %>%
  select(weight, year, sample) %>%
  spread(year, sample) %>%
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

# 26 babyhh --------------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "babyhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  # remove years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, sample))

sev <- getpovby(hbai, pov = "low50ahc", by = "babyhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  # remove years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, sample))

tables$baby_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                       rel_comps = splitntranspose(rel, "composition"),
                       rel_numbers = splitntranspose(rel, "number"),
                       sev_rates = splitntranspose(sev, "rate"),
                       sev_comps = splitntranspose(sev, "composition"),
                       sev_numbers = splitntranspose(sev, "number"),
                       sample = splitntranspose(rel, "sample"))

# 27 youngmumhh ----------------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "youngmumhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)",
         yearn >= 4) %>%
  summarise_data() %>%
  # remove years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, sample))

sev <- getpovby(hbai, pov = "low50ahc", by = "youngmumhh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)",
         yearn >= 4) %>%
  summarise_data() %>%
  # remove years where individual age data was unavailable
  mutate(number = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, number),
         rate = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, rate),
         composition = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, composition),
         sample = ifelse(yearn %in% seq(9, 16, 1) & groupingvar != "All", NA, sample))

tables$youngmum_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                           rel_comps = splitntranspose(rel, "composition"),
                           rel_numbers = splitntranspose(rel, "number"),
                           sev_rates = splitntranspose(sev, "rate"),
                           sev_comps = splitntranspose(sev, "composition"),
                           sev_numbers = splitntranspose(sev, "number"),
                           sample = splitntranspose(rel, "sample"))

# 28 ecobu (children) ----------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "ecobu", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "ecobu", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

tables$ecobu_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                        rel_comps = splitntranspose(rel, "composition"),
                        rel_numbers = splitntranspose(rel, "number"),
                        sev_rates = splitntranspose(sev, "rate"),
                        sev_comps = splitntranspose(sev, "composition"),
                        sev_numbers = splitntranspose(sev, "number"),
                        sample = splitntranspose(rel, "sample"))

# 29 workinghh (children) ------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "workinghh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

sev <- getpovby(hbai, pov = "low50ahc", by = "workinghh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 5)

tables$workinghh_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                            rel_comps = splitntranspose(rel, "composition"),
                            rel_numbers = splitntranspose(rel, "number"),
                            sev_rates = splitntranspose(sev, "rate"),
                            sev_comps = splitntranspose(sev, "composition"),
                            sev_numbers = splitntranspose(sev, "number"),
                            sample = splitntranspose(rel, "sample"))

# 30 tenhbai (children) --------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "tenhbai", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 12)

sev <- getpovby(hbai, pov = "low50ahc", by = "tenhbai", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 12)

tables$tenhbai_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                          rel_comps = splitntranspose(rel, "composition"),
                          rel_numbers = splitntranspose(rel, "number"),
                          sev_rates = splitntranspose(sev, "rate"),
                          sev_comps = splitntranspose(sev, "composition"),
                          sev_numbers = splitntranspose(sev, "number"),
                          sample = splitntranspose(rel, "sample"))

# 31 urinds (children) ---------------------------------------------------------

rel <- getpovby(hbai, pov = "low60ahc", by = "urinds", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 15)

sev <- getpovby(hbai, pov = "low50ahc", by = "urinds", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data() %>%
  filter(yearn >= 15)

tables$urinds_ch <- list(rel_rates = splitntranspose(rel, "rate"),
                         rel_comps = splitntranspose(rel, "composition"),
                         rel_numbers = splitntranspose(rel, "number"),
                         sev_rates = splitntranspose(sev, "rate"),
                         sev_comps = splitntranspose(sev, "composition"),
                         sev_numbers = splitntranspose(sev, "number"),
                         sample = splitntranspose(rel, "sample"))

# 32 disability (children) -----------------------------------------------------
rel_pp <- getpovby(hbai, by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_pp <- getpovby(hbai, pov = "low50ahc", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ch <- getpovby(hbai, by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ch <- getpovby(hbai, pov = "low50ahc", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ad <- getpovby(hbai, by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ad <- getpovby(hbai, pov = "low50ahc", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

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

tables$disab_ch <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                        rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                        rel_numbers = rbind(rel_numbers1, rel_numbers2, rel_numbers3),
                        sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                        sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                        sev_numbers = rbind(sev_numbers1, sev_numbers2, sev_numbers3),
                        sample = rbind(sample1, sample2, sample3))

# 33 disability no bens (children) ---------------------------------------------
rel_pp <- getpovby(hbai, pov = "low60ahc_dis", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_pp <- getpovby(hbai, pov = "low50ahc_dis", by = "dispp_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ch <- getpovby(hbai, pov = "low60ahc_dis", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ch <- getpovby(hbai, pov = "low50ahc_dis", by = "disch_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

rel_ad <- getpovby(hbai, pov = "low60ahc_dis", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

sev_ad <- getpovby(hbai, pov = "low50ahc_dis", by = "disad_hh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  summarise_data()

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

tables$disab_nobens_ch <- list(rel_rates = rbind(rel_rates1, rel_rates2, rel_rates3),
                               rel_comps = rbind(rel_comps1, rel_comps2, rel_comps3),
                               rel_numbers = rbind(rel_numbers1, rel_numbers2, rel_numbers3),
                               sev_rates = rbind(sev_rates1, sev_rates2, sev_rates3),
                               sev_comps = rbind(sev_comps1, sev_comps2, sev_comps3),
                               sev_numbers = rbind(sev_numbers1, sev_numbers2, sev_numbers3),
                               sample = rbind(sample1, sample2, sample3))

# 34 ethgrphh (children) -------------------------------------------------------

rel <- getpovby(hbai, by = "ethgrphh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  filter(yearn >= 8) %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

sev <- getpovby(hbai, pov = "low50ahc", by = "ethgrphh", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  filter(yearn >= 8) %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr)) %>%
  mutate(groupingvar = factor(groupingvar,
                              levels = c("All", "White - British",
                                         "White - Other",
                                         "Asian or Asian British",
                                         "Mixed, Black or Black British, and Other")))

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
            sample = n()) %>%
  group_by(ethgrphh) %>%
  mutate(age = getrollingmean(age, 5),
         sample = getrollingtotal(sample, 5)) %>%
  ungroup() %>%
  filter(yearn >= 12) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$period5yr),
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

# 35 ethgrphh_2f ---------------------------------------------------------------

rel <- getpovby(hbai, by = "ethgrphh_2f", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  filter(yearn >= 8) %>%
  summarise_data()

sev <- getpovby(hbai, pov = "low50ahc", by = "ethgrphh_2f", weight = "gs_newch") %>%
  filter(groupingvar != "(Missing)") %>%
  filter(yearn >= 8) %>%
  summarise_data()

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
            sample = n()) %>%
  group_by(ethgrphh_2f) %>%
  mutate(age = getrollingmean(age, 3),
         sample = getrollingtotal(sample, 3)) %>%
  ungroup() %>%
  filter(yearn >= 10) %>%
  mutate(age = ifelse(sample < 50, NA, round2(age, 0)),
         year = factor(yearn,
                       levels = labels$years$numbered,
                       labels = labels$years$periods),
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

# 36 food security (children) --------------------------------------------------

total <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newch),
         sample = sum(gs_newch > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newch),
            sample = max(sample),
            povsample = sum(gs_newch > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "All children") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = mean(composition),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup() %>%
  mutate(foodsec = fct_rev(foodsec))

total_comp <- select(total, Group, foodsec, composition) %>%
  spread(foodsec, composition)
total_num <- select(total, Group, foodsec, number) %>%
  spread(foodsec, number)
total_sample <- select(total, Group, sample)[1, ]

relpov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahc == 1) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newch),
         sample = sum(gs_newch > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newch),
            sample = max(sample),
            povsample = sum(gs_newch > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In relative poverty") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = mean(composition),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup()

relpov_comp <- select(relpov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
relpov_num <- select(relpov, Group, foodsec, number) %>%
  spread(foodsec, number)
relpov_sample <- select(relpov, Group, sample)[1, ]

abspov <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         low60ahcabs == 1) %>%
  group_by(yearn) %>%
  mutate(population = sum(gs_newch),
         sample = sum(gs_newch > 0, na.rm = TRUE)) %>%
  group_by(yearn, foodsec) %>%
  summarise(number = sum(gs_newch),
            sample = max(sample),
            povsample = sum(gs_newch > 0, na.rm = TRUE),
            composition = number / max(population),
            Group = "In absolute poverty") %>%
  group_by(foodsec) %>%
  mutate(number = mean(number),
         sample = sum(sample),
         povsample = sum(povsample),
         number = ifelse(povsample >= 100, number, NA),
         number = roundpop(number),
         composition = mean(composition),
         composition = ifelse(sample >= 100, composition, NA),
         composition = roundpct(composition)) %>%
  filter(yearn == max(yearn)) %>%
  ungroup()

abspov_comp <- select(abspov, Group, foodsec, composition) %>%
  spread(foodsec, composition)
abspov_num <- select(abspov, Group, foodsec, number) %>%
  spread(foodsec, number)
abspov_sample  <- select(abspov, Group, sample)[1, ]
sample <- rbind(total_sample, relpov_sample, abspov_sample) %>%
  rename(Sample = sample)

tables$foodsec_ch <- list(comps = rbind(total_comp, relpov_comp, abspov_comp),
                          numbers = rbind(total_num, relpov_num, abspov_num),
                          sample = sample)

# 37 priority groups -----------------------------------------------------------
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
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "In household with disabled person(s)") %>%
  pull(rate)

dis_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "dispp_hh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "In household with disabled person(s)") %>%
  pull(rate)

# 3+ children
many_rel <- filter(tables$depchldh_ch$rel_rates,
                  Group == "3 or more children in the household")[[max(periods)]]

many_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "depchldh_ch", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "3 or more children in the household") %>%
  pull(rate)

many_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "depchldh_ch", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "3 or more children in the household") %>%
  pull(rate)

# Baby
baby_rel <- filter(tables$baby_ch$rel_rates,
                   Group == "Youngest child is younger than 1")[[max(periods)]]

baby_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "babyhh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "Youngest child is younger than 1") %>%
  pull(rate)

baby_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "babyhh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "Youngest child is younger than 1") %>%
  pull(rate)

# Ethnic
eth_rel <- filter(tables$ethgrphh_2f_ch$rel_rates,
                   Group == "Minority ethnic")[[max(periods)]]

eth_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "ethgrphh_2f", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "Minority ethnic") %>%
  pull(rate)

eth_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "ethgrphh_2f", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "Minority ethnic") %>%
  pull(rate)

# Lone parent
lone_rel <- filter(tables$loneparenthh_ch$rel_rates,
                  Group == "Single parent in household")[[max(periods)]]

lone_abs <- getpovby(hbai3yr, pov = "low60ahcabs", by = "loneparenthh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(groupingvar == "Single parent in household") %>%
  pull(rate)

lone_cmd <- getpovby(hbai3yr, pov = "cmdahc", by = "loneparenthh", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
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

tables$priority <- list(rel = rel,
                        abs = abs,
                        cmd = cmd)

# :----------------------- -----------------------------------------------------
# 38 medians -------------------------------------------------------------------

bhc <- hbai %>%
  group_by(yearn) %>%
  getmediansbhc() %>%
  mutate_at(vars(c("pp", "ch", "wa", "pn")), get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate(year = get_periods(yearn)) %>%
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
  group_by(yearn) %>%
  getmediansahc() %>%
  mutate_at(vars(c("pp", "ch", "wa", "pn")), get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate(year = get_periods(yearn)) %>%
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

tables$medians <- list(bhc = numbers_bhc,
                       ahc = numbers_ahc,
                       sample = sample)

# 39 decile points -------------------------------------------------------------

bhc <- hbai %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  mutate(year = get_periods(yearn)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~get3yraverage(.)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

ahc <- hbai %>%
  group_by(yearn) %>%
  getdecptsahc() %>%
  mutate(year = get_periods(yearn)) %>%
  arrange(yearn) %>%
  mutate_if(is.numeric, ~get3yraverage(.)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  tail(-2L) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  spread(year, value)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(sample = getrollingtotal(sample, 3),
         year = get_periods(yearn),
         Group = "All") %>%
  filter(yearn >= 3) %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$decilepoints <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)

# 40 decile shares -------------------------------------------------------------

bhc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesbhc() %>%
  group_by(Decile) %>%
  mutate(share = get3yraverage(share),
         share = round2(share / 1000000, 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

ahc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesahc() %>%
  group_by(Decile) %>%
  mutate(share = get3yraverage(share),
         share = round2(share / 1000000, 0)) %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(year, share, Decile) %>%
  spread(year, share)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(sample = getrollingtotal(sample, 3),
         year = get_periods(yearn),
         Group = "All") %>%
  filter(yearn >= 3) %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$decileshares <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)

# 41 Palma & Gini --------------------------------------------------------------

palma_bhc <- hbai %>%
  group_by(yearn) %>%
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
  mutate(year = get_periods(yearn)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma_ahc <- hbai %>%
  group_by(yearn) %>%
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
  mutate(year = get_periods(yearn)) %>%
  select(Measure, year, Palma) %>%
  spread(year, Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini_ahc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  filter(yearn >= 3) %>%
  mutate(year = get_periods(yearn)) %>%
  select(-yearn) %>%
  spread(year, Gini)

gini <- rbind(gini_bhc, gini_ahc)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = n()) %>%
  mutate(sample = getrollingtotal(sample, 3),
         year = get_periods(yearn),
         Group = "All") %>%
  filter(yearn >= 3) %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables$palmagini <- list(palma = palma,
                         gini = gini,
                         sample = sample)

# 42 poverty thresholds --------------------------------------------------------

hbai1 <- filter(hbai, yearn == max(yearn))
hbai2 <- filter(hbai, yearn == max(yearn) - 1)
hbai3 <- filter(hbai, yearn == max(yearn) - 2)

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

tables$thresholds <- list(weekly_bhc = weekly_bhc,
                          annual_bhc = annual_bhc,
                          weekly_ahc = weekly_ahc,
                          annual_ahc = annual_ahc)

# 43 income sources ------------------------------------------------------------

hbai1 <- filter(hbai, yearn == max(yearn))
hbai2 <- filter(hbai, yearn == max(yearn) - 1)
hbai3 <- filter(hbai, yearn == max(yearn) - 2)

df1 <- getsources(hbai1)
df2 <- getsources(hbai2)
df3 <- getsources(hbai3)

df <- data.frame(df1[1])
df[2] <- (df1[2] + df2[2] + df3[2])/3
df[3] <- (df1[3] + df2[3] + df3[3])/3
df[4] <- (df1[4] + df2[4] + df3[4])/3
df[5] <- (df1[5] + df2[5] + df3[5])/3
df[6] <- (df1[6] + df2[6] + df3[6])/3

sources <- df %>%
  mutate(Decile = fct_relevel(Decile, "All", after = 0L)) %>%
  mutate_if(is.numeric, roundpct) %>%
  arrange(Decile)

tables$sources <- list(sources = sources)

# xx distribution --------------------------------------------------------------
tables$distribution <- list()

tables$distribution$dist <- hbai %>%
  group_by(yearn) %>%
  mutate(income = s_oe_bhc * bhcpubdef / bhcyrdef) %>%
  select(yearn, gs_newpp, income) %>%
  ungroup() %>%
  filter(yearn >= max(yearn) - 2)

tables$distribution$distdecs <- hbai %>%
  filter(yearn >= max(yearn) - 2) %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  gather(x, value, -yearn) %>%
  group_by(x) %>%
  mutate(value = get3yraverage(value)) %>%
  ungroup() %>%
  filter(yearn == max(yearn)) %>%

  mutate(xpos = lag(value) + 1/2*(value - lag(value)),
         xpos = ifelse(x == "1", value/2 + 50, xpos),
         xpos = ifelse(x == "10", (lag(value) + 50), xpos))

tables$distribution$distthresh <- hbai %>%
  filter(yearn >= max(yearn) - 2) %>%
  group_by(yearn) %>%
  summarise(UKmedian = max(mdoebhc * bhcpubdef / bhcyrdef),
            Scotmedian = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                      probs = 0.5, weights = gs_newpp),
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
         yearn >= max(yearn) - 2) %>%
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
         benunit == 1) %>%
  group_by(ethgrphh) %>%
  summarise(age = wtd.median(agehd, gs_newbu))

# religion
religion <- adult %>%
  filter(yearn >= max(yearn) - 4) %>%
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



