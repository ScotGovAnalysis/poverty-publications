# Note re 2021 data

# weights in 2021 weights are set to 0, added na.rm = TRUE to all getnyrtable,
# getrollingmean, getrollingtotal, getheadlines & summarise_data function calls;
# wtd.medians and wtd.quantiles don't deal with an all-zero weights vector, so
# were dealt with table by table by filtering out bad year before quantile
# function call and adding it back in value set to missing (missing marker
# 99992) and sample size set to zero

# load -------------------------------------------------------------------------

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
  mutate(year = get_periods(yearn, n = 1))

rates <- cmd  %>%
  select(year, rate, Measure) %>%
  pivot_wider(names_from = year, values_from = rate) %>%
  replace(is.na(.), 99992)

numbers <- cmd %>%
  select(year, number, Measure) %>%
  pivot_wider(names_from = year, values_from = number) %>%
  replace(is.na(.), 99992)

sample <- cmd %>%
  select(year, sample, Measure) %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  pivot_wider(names_from = year, values_from = sample) %>%
  mutate(Measure = ifelse(Measure == "Old measure, after housing costs",
                          "Old measure", "New measure")) %>%
  replace(is.na(.), 99992)

tables_1yr$cmd <- list(rates = rates,
                       numbers = numbers,
                       sample = sample)

# 8 pmd ------------------------------------------------------------------------
pmd <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Group = "Pensioners aged 65 and older") %>%
  mutate(year = get_periods(yearn, n = 1))

tables_1yr$pmd <- list(rates = pmd %>%
                         select(Group, year, rate) %>%
                         pivot_wider(names_from = year, values_from = rate),
                       numbers = pmd %>%
                         select(Group, year, number) %>%
                         pivot_wider(names_from = year, values_from = number),
                       sample = pmd  %>%
                         select(Group, year, sample) %>%
                         pivot_wider(names_from = year, values_from = sample))

# :----------------------- -----------------------------------------------------
# 9 medians --------------------------------------------------------------------
bhc <- hbai %>%

  group_by(yearn) %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  summarise(pp = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                                           weights = gs_newpp),
            ch = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                                           weights = gs_newch),
            wa = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                                           weights = gs_newwa),
            pn = analysistools::wtd.median(s_oe_bhc * bhcpubdef / bhcyrdef,
                                           weights = gs_newpn)) %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate(year = get_periods(yearn, n = 1)) %>%
  mutate_if(is.numeric, ~round2(., 0))

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
                              weights = gs_newpn)) %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate(year = get_periods(yearn, n = 1)) %>%
  mutate_if(is.numeric, ~round2(., 0))

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

# samples

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(pp_sample = sum(gs_newpp > 0, na.rm = TRUE),
            ch_sample = sum(gs_newch > 0, na.rm = TRUE),
            wa_sample = sum(gs_newwa > 0, na.rm = TRUE),
            pn_sample = sum(gs_newpn > 0, na.rm = TRUE )) %>%
  mutate(year = get_periods(yearn, n = 1))

sample_pp <- select(sample, year, pp_sample) %>%
  pivot_wider(names_from = year, values_from = pp_sample) %>%
  mutate(Group = "All people")

sample_ch <- select(sample, year, ch_sample) %>%
  pivot_wider(names_from = year, values_from = ch_sample) %>%
  mutate(Group = "Children")

sample_wa <- select(sample, year, wa_sample) %>%
  pivot_wider(names_from = year, values_from = wa_sample) %>%
  mutate(Group = "Working-age adults")

sample_pn <- select(sample, year, pn_sample) %>%
  pivot_wider(names_from = year, values_from = pn_sample) %>%
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

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  getdecptsbhc() %>%

  # add year back in
  full_join(data.frame(yearn = 27)) %>%
  arrange(yearn) %>%

  mutate(year = get_periods(yearn, n = 1)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
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

  mutate(year = get_periods(yearn, n = 1)) %>%
  mutate_if(is.numeric, ~round2(., 0)) %>%
  select(-yearn) %>%
  gather(Decile, value, -year) %>%
  filter(Decile != "10") %>%
  pivot_wider(names_from = year, values_from = value)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = sum(gs_newpp > 0)) %>%
  mutate(year = get_periods(yearn, n = 1),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$decilepoints <- list(bhc = bhc,
                                ahc = ahc,
                                sample = sample)
# 11 decile shares -------------------------------------------------------------
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
  mutate(share = round2(share / 1000000, 0)) %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
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
  mutate(share = round2(share / 1000000, 0)) %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
  select(year, share, Decile) %>%
  pivot_wider(names_from = year, values_from = share)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = sum(gs_newpp > 0)) %>%
  mutate(year = get_periods(yearn, n = 1),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$decileshares <- list(bhc = bhc,
                            ahc = ahc,
                            sample = sample)
# 12 Palma & Gini --------------------------------------------------------------
palma_bhc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_bhc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_bhc * gs_newpp) * bhcpubdef[1] / bhcyrdef[1] * 365/7) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = roundpct(Palma),
         Measure = "Before housing costs") %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
  select(Measure, year, Palma) %>%
  pivot_wider(names_from = year, values_from = Palma)

palma_ahc <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

  group_by(yearn) %>%
  mutate(Decile = analysistools::getdeciles(s_oe_ahc, weights = gs_newpp)) %>%
  group_by(yearn, Decile) %>%
  summarise(share = sum(s_oe_ahc * gs_newpp) * ahcpubdef[1] / ahcyrdef[1] * 365/7) %>%

  # add year back in
  full_join(data.frame(yearn = 27, Decile = unique(.$Decile))) %>%
  arrange(yearn) %>%

  group_by(yearn) %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = roundpct(Palma),
         Measure = "After housing costs") %>%
  filter(Decile == 10) %>%
  ungroup() %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
  select(Measure, year, Palma) %>%
  pivot_wider(names_from = year, values_from = Palma)

palma <- rbind(palma_bhc, palma_ahc)

gini_bhc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "Before housing costs") %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
  select(-yearn) %>%
  pivot_wider(names_from = year, values_from = Gini)

gini_ahc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = roundpct(Gini),
         Measure = "After housing costs") %>%
  mutate(year = get_periods(yearn, n = 1)) %>%
  select(-yearn) %>%
  pivot_wider(names_from = year, values_from = Gini)

gini <- rbind(gini_bhc, gini_ahc)

sample <- hbai %>%
  group_by(yearn) %>%
  summarise(sample = sum(gs_newpp > 0)) %>%
  mutate(year = get_periods(yearn, n = 1),
         Group = "All") %>%
  select(Group, year, sample) %>%
  pivot_wider(names_from = year, values_from = sample)

tables_1yr$palmagini <- list(palma = palma,
                         gini = gini,
                         sample = sample)
# 13 poverty thresholds --------------------------------------------------------

hbai_latest <- hbai %>%

  # exclude bad data year
  filter(yearn != 27) %>%

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

# mark 20/21 data as missing ---------------------------------------------------
# (missing code 99992) in all but last table
for (i in 1:(length(tables_1yr) - 1)) {
  tables_1yr[[i]] <- lapply(tables_1yr[[i]], function(x) mutate(x, "2020/21" = 99992))
}

# save all ---------------------------------------------------------------------
saveRDS(tables_1yr, "data/tables_1yr.rds")
rm(list = ls())

cat("1 yr Scot data aggregated", fill = TRUE)



