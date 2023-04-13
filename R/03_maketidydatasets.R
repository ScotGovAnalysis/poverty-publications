
# load ---------------------------------------------------

library(tidyverse)
library(haven)
library(Hmisc)

source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/hbai_clean.rds")
househol <- readRDS("data/househol_clean.rds")
adult <- readRDS("data/adult_clean.rds")
child <- readRDS("data/child_clean.rds")
benefits <- readRDS("data/benefits_clean.rds")
inflationindex <- readRDS("data/inflationindex.rds")

# inflation index

inflationindex <- inflationindex %>%
  mutate(yearn = factor(year,
                        levels = labels[["years"]]$formatted,
                        labels = labels[["years"]]$numbered),
         yearn = as.numeric(yearn),
         ahcyrdef = inflation_ahc,
         bhcyrdef = inflation_bhc) %>%
  select(yearn, ahcyrdef, bhcyrdef)

# flags etc. --------------------------------------------------------

# * recode tenure ----------

hbai <- hbai %>%
  mutate(tenhbai = case_when(ptentyp2 %in% c(1, 2) ~ 3,
                             ptentyp2 %in% c(3, 4) ~ 4,
                             ptentyp2 == 5 ~ 1,
                             ptentyp2 == 6 ~ 2))

# * council ---------------------------------------------------------------------

council <- househol %>% select(year, sernum, lac, laua) %>%
  mutate(lac = factor(lac, levels = labels$lac$codes, labels = labels$lac$names),
         laua = factor(laua, levels = labels$laua$codes, labels = labels$laua$names),
         laua = case_when(!is.na(laua) ~ laua,
                          !is.na(lac)  ~ lac)) %>%
  select(-lac)

hbai <- left_join(hbai, council, by = c("sernum", "year"))

# * urban/rural & SIMD ----------------------------------------------------------

urinds <- househol %>% select(year, sernum, urinds, imds)
hbai <- left_join(hbai, urinds, by = c("sernum", "year"))

# * hh work status -------------------------------------------------------

workinghh <- hbai %>%
  mutate(working = case_when(ecobu %in% labels[["economic"]]$codes[1:5] ~ 1,
                             ecobu %in% labels[["economic"]]$codes[6:8] ~ 0)) %>%
  group_by(year, sernum) %>%
  summarise(workinghh = max(working))

hbai <- left_join(hbai, workinghh, by = c("year", "sernum"))

# * hh disability status -------------------------------------------------

disabledhh <- hbai %>%
  group_by(year, sernum) %>%
  summarise(disch_hh = max(discorkid),
            disad_hh = max(discorabflg)) %>%
  mutate(disch_hh = ifelse(disch_hh > 0, 1, 0),
         disch_hh = ifelse(year == "9495", NA, disch_hh),
         disad_hh = ifelse(disad_hh > 0, 1, 0),
         dispp_hh = ifelse(disch_hh + disad_hh > 0, 1, 0 ))

hbai <- left_join(hbai, disabledhh, by = c("year", "sernum"))

# * gender of single adult hhs -------------------------------------------

singlehhgender <- hbai %>%
  filter(adulth == 1) %>%
  mutate(singlehh = case_when(gs_newpn > 0 & sexhd == 1 ~ 1,
                              gs_newpn > 0 & sexhd == 2 ~ 2,
                              gs_newpn == 0 & sexhd == 1 & depchldh == 0 ~ 3,
                              gs_newpn == 0 & sexhd == 2 & depchldh == 0 ~ 4,
                              sexhd == 1 & depchldh > 0 ~ 5,
                              sexhd == 2 & depchldh > 0 ~ 6)) %>%
  select(year, sernum, benunit, singlehh)

hbai <- left_join(hbai, singlehhgender, by = c("sernum", "benunit", "year"))

# * lone parent in hh ----------------------------------------------------

loneparenthh <- hbai %>%
  mutate(loneparent = ifelse(newfambu == 5, 1, 0)) %>%
  group_by(year, sernum) %>%
  summarise(loneparenthh = max(loneparent))

hbai <- left_join(hbai, loneparenthh, by = c("year", "sernum"))

# * baby in hh -----------------------------------------------------------

babyhh <- child %>%
  mutate(baby = ifelse(age < 1, 1, 0)) %>%
  group_by(year, sernum) %>%
  summarise(babyhh = max(baby))

hbai <- left_join(hbai, babyhh, by = c("year", "sernum")) %>%
  mutate(babyhh = ifelse(is.na(babyhh), 0, babyhh),

         # individual child  age is missing in certain years
         babyhh = ifelse(year %in% c("0203", "0304", "0405", "0708"),
                             NA, babyhh))

# * young mother in hh ----------------------------------------------------

youngmumhh <- adult %>%
  # filter for parents only
  filter_at(vars(r01, r02, r03, r04, r05, r06, r07,
                 r08, r09, r10, r11, r12, r13, r14),
            any_vars(. %in% c(7, 8))) %>%
  # filter for women and by age
  mutate(youngmum = ifelse(age < 25 & sex == 2, 1, 0)) %>%
  group_by(year, sernum) %>%
  summarise(youngmumhh = max(youngmum))

hbai <- left_join(hbai, youngmumhh, by = c("year", "sernum")) %>%
  mutate(youngmumhh = ifelse(is.na(youngmumhh), 0, youngmumhh),

         # individual adult age is missing in certain years
         youngmumhh = ifelse(year %in% c("9495", "9596", "9697", "0203", "0304",
                                         "0405", "0506", "0607", "0708"),
                             NA, youngmumhh))

# weights ------------------------------------------------------------------

# * child weights -------------------------------------------------------

# alternative child definition: people aged 0-17

ch16_17 <- child %>%
  group_by(year, sernum, benunit) %>%
  summarise(kid16_17 = sum(age %in% c(16, 17)))

ad16_17 <- adult %>%
  group_by(year, sernum, benunit) %>%
  summarise(adult16_17 = sum(age %in% c(16, 17)))

hbai <- hbai %>%
  left_join(ch16_17, by = c("year", "sernum", "benunit")) %>%
  left_join(ad16_17, by = c("year", "sernum", "benunit")) %>%
  mutate(kid0_4 = kid0_1 + kid2_4,
         kid5_12 = kid5_7 + kid8_10 + kid11_12,
         kid13plus = kid13_15 + kid16plus,
         kid16_17 = ifelse(is.na(kid16_17), 0, kid16_17),
         adult16_17 = ifelse(is.na(adult16_17), 0, adult16_17),

         # UNCRC child definition (all people aged 0-17)
         pp0_17 = kid0_4 + kid5_12 + kid13_15 + kid16_17 + adult16_17,

         # UNCRC+ definition: all FRS children plus all 16-17 year-old adults
         kidplus = depchldb + adult16_17,

         wgt0_4 = ifelse(kid0_4 > 0, gs_newch * kid0_4 / depchldb, 0),
         wgt5_12 = ifelse(kid5_12 > 0, gs_newch * kid5_12 / depchldb, 0),
         wgt13plus = ifelse(kid13plus > 0, gs_newch * kid13plus / depchldb, 0),

         # UNCRC child definition (all people aged 0-17)
         wgt0_17 = ifelse(pp0_17 > 0, gs_newpp * pp0_17 / (depchldb + adultb), 0),

         # UNCRC+ definition: FRS children plus all 16-17 year-old adults
         wgt_kidplus = ifelse(kidplus > 0, gs_newpp * kidplus / (depchldb + adultb), 0))

# * pensioners 65+ --------------------------------------------

hbai <- hbai %>%
  mutate(wgt65 = case_when(gs_newpn >= 2 * gs_newbu & agehd >= 65 & agesp >= 65 ~ 2 * gs_newbu,
                           gs_newpn > 0 & (agehd >= 65 | agesp >= 65) ~ gs_newbu,
                           TRUE ~ 0))

# poverty flags ---------------------------------------------------------------

# * absolute poverty ------------------------------------------

hbai <- hbai %>%
  left_join(inflationindex, by = "yearn") %>%
  mutate(ahcpubdef = tail(inflationindex$ahcyrdef, 1L),
         bhcpubdef = tail(inflationindex$bhcyrdef, 1L),
         abs1011ahc = ifelse(yearn == 17, mdoeahc, 0),
         abs1011ahc = max(abs1011ahc),
         abs1011ahc = ifelse(yearn == 17, abs1011ahc,
                             abs1011ahc * ahcyrdef / inflationindex$ahcyrdef[17]),
         abs1011bhc = ifelse(yearn == 17, mdoebhc, 0),
         abs1011bhc = max(abs1011bhc),
         abs1011bhc = ifelse(yearn == 17, abs1011bhc,
                             abs1011bhc * bhcyrdef / inflationindex$bhcyrdef[17]),
         low60ahcabs = ifelse(s_oe_ahc < 0.6 * abs1011ahc, 1, 0),
         low60bhcabs = ifelse(s_oe_bhc < 0.6 * abs1011bhc, 1, 0))

# * working poverty, matdep -------------------

hbai <- hbai %>%
  mutate(workpovahc = ifelse(low60ahc == 1 & workinghh == 1, 1, 0),
         workpovbhc = ifelse(low60bhc == 1 & workinghh == 1, 1, 0),
         cmdahc = ifelse(low70ahc == 1 & mdch == 1, 1, 0),
         cmdahc_new = ifelse(low70ahc == 1 & mdchnew == 1, 1, 0),
         cmdbhc = ifelse(low70bhc == 1 & mdch == 1, 1, 0),
         cmdbhc_new = ifelse(low70bhc == 1 & mdchnew == 1, 1, 0))

# disability benefit amounts
benefits <- select(benefits, sernum, benamt, year)
hbai <- left_join(hbai, benefits, by = c("sernum", "year"))

# * poverty disability flags ----------------------------------------------------
hbai <- hbai %>%
  group_by(year) %>%
  mutate(benamt = ifelse(is.na(benamt), 0,
                         ifelse(benamt < 0, 0, benamt)),
         s_oe_ahc_dis = s_oe_ahc - benamt * ahcdef / eqoahchh,
         s_oe_bhc_dis = s_oe_bhc - benamt * bhcdef / eqobhchh,
         relpovahc_dis_threshold = 0.6 * wtd.quantile(s_oe_ahc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         relpovbhc_dis_threshold = 0.6 * wtd.quantile(s_oe_bhc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         sevpovahc_dis_threshold = 0.5 * wtd.quantile(s_oe_ahc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         sevpovbhc_dis_threshold = 0.5 * wtd.quantile(s_oe_bhc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         low60ahc_dis = ifelse(s_oe_ahc_dis < relpovahc_dis_threshold, 1, 0),
         low60bhc_dis = ifelse(s_oe_bhc_dis < relpovbhc_dis_threshold, 1, 0),
         low50ahc_dis = ifelse(s_oe_ahc_dis < sevpovahc_dis_threshold, 1, 0),
         low50bhc_dis = ifelse(s_oe_bhc_dis < sevpovbhc_dis_threshold, 1, 0)) %>%
  ungroup()

# * food security ---------------------------------------------------------------

foodsec1920 <- getfoodsec4(househol) %>% filter(year == "1920")

hbai1920 <- left_join(hbai %>% filter(year == "1920") %>% select(-foodsec),
                      foodsec1920, by = c("sernum", "year"))

hbainot1920 <- hbai %>%
  filter(year != "1920") %>%
  rename(foodsec_score = foodsec) %>%
  mutate(foodsec4 = case_when(hhshare == 1 & foodsec_score == 0 ~ 1,
                              hhshare == 1 & foodsec_score %in% c(1, 2) ~ 2,
                              hhshare == 1 & foodsec_score %in% c(3, 4, 5) ~ 3,
                              hhshare == 1 & foodsec_score >= 6 ~ 4))

hbai <- rbind(hbai1920, hbainot1920)

## tidy HBAI dataset ----------------------------------------------------

tidyhbai <- hbai %>%

  # add factor labels
  mutate(ecobu = factor(ecobu,
                        levels = labels[["economic"]]$codes,
                        labels = labels[["economic"]]$labels),
         kidecobu = factor(kidecobu,
                           levels = labels[["kideconomic"]]$codes,
                           labels = labels[["kideconomic"]]$labels),
         newfambu = factor(newfambu,
                           levels = labels[["familytype"]]$codes,
                           labels = labels[["familytype"]]$labels),
         tenhbai = factor(tenhbai,
                          levels = labels[["tenure"]]$codes,
                          labels = labels[["tenure"]]$labels),
         urinds = factor(urinds,
                         levels = labels[["urbrur"]]$codes,
                         labels = labels[["urbrur"]]$labels),
         imds = factor(imds, levels = seq(1, 10, 1), ordered = TRUE),

         agehdband7 = case_when(agehd <= 24 ~ "16-24",
                                agehd >= 25 & agehd <= 34 ~ "25-34",
                                agehd >= 35 & agehd <= 44 ~ "35-44",
                                agehd >= 45 & agehd <= 54 ~ "45-54",
                                agehd >= 55 & agehd <= 64 ~ "55-64",
                                agehd >= 65 & agehd <= 74 ~ "65-74",
                                agehd >= 75 ~ "75+"),
         agehdband8 = case_when(agehd <= 24 ~ "16-24",
                                agehd >= 25 & agehd <= 34 ~ "25-34",
                                agehd >= 35 & agehd <= 44 ~ "35-44",
                                agehd >= 45 & agehd <= 54 ~ "45-54",
                                agehd >= 55 & agehd <= 64 ~ "55-64",
                                agehd >= 65 & agehd <= 74 ~ "65-74",
                                agehd >= 75 & agehd <= 84 ~ "75-84",
                                agehd >= 85 ~ "85+"),
         workinghh = factor(workinghh,
                            levels = labels[["workinghh"]]$codes,
                            labels = labels[["workinghh"]]$labels),
         loneparenthh = factor(loneparenthh,
                               levels = labels[["loneparent"]]$codes,
                               labels = labels[["loneparent"]]$labels),
         babyhh = factor(babyhh,
                         levels = labels[["baby"]]$codes,
                         labels = labels[["baby"]]$labels),
         youngmumhh = factor(youngmumhh,
                             levels = labels[["youngmum"]]$codes,
                             labels = labels[["youngmum"]]$labels),
         disch_hh = factor(disch_hh,
                           levels = labels[["disch"]]$codes,
                           labels = labels[["disch"]]$labels),
         disad_hh = factor(disad_hh,
                           levels = labels[["disad"]]$codes,
                           labels = labels[["disad"]]$labels),
         dispp_hh = factor(dispp_hh,
                           levels = labels[["dispp"]]$codes,
                           labels = labels[["dispp"]]$labels),
         depchldh_ch = depchldh,
         depchldh = factor(depchldh,
                           levels = labels[["childno"]]$codes,
                           labels = labels[["childno"]]$labels),
         depchldh_ch = factor(depchldh_ch,
                              levels = labels[["childno_ch"]]$codes,
                              labels = labels[["childno_ch"]]$labels),
         gvtregn = factor(gvtregn,
                          levels = labels[["regions"]]$codes,
                          labels = labels[["regions"]]$labels),
         ethgrphh_2f = ethgrphh,
         ethgrphh = case_when(yearn >= 19 ~ factor(ethgrphh,
                                                   levels = labels[["ethnic1213"]]$codes,
                                                   labels = labels[["ethnic1213"]]$labels),
                              yearn == 18 ~ factor(ethgrphh,
                                                   levels = labels[["ethnic1112"]]$codes,
                                                   labels = labels[["ethnic1112"]]$labels),
                              yearn <= 17 ~ factor(ethgrphh,
                                                   levels = labels[["ethnic0203"]]$codes,
                                                   labels = labels[["ethnic0203"]]$labels),
                              yearn == 8 ~ factor(ethgrphh,
                                                  levels = labels[["ethnic0102"]]$codes,
                                                  labels = labels[["ethnic0102"]]$labels),
                              yearn <= 7 ~ factor(ethgrphh,
                                                  levels = labels[["ethnic9495"]]$codes,
                                                  labels = labels[["ethnic9495"]]$labels)),

         ethgrphh_2f = factor(ethgrphh_2f,
                              levels = labels[["ethnic_2f"]]$codes,
                              labels = labels[["ethnic_2f"]]$labels),
         singlehh = factor(singlehh,
                           levels = labels[["gender"]]$codes,
                           labels = labels[["gender"]]$labels),
         foodsec = factor(foodsec4,
                          levels = labels[["foodsecurity"]]$codes,
                          labels = labels[["foodsecurity"]]$labels))  %>%
  mutate_at(vars(c("ecobu", "kidecobu", "newfambu", "tenhbai", "urinds",
                   "workinghh", "disch_hh", "disad_hh", "dispp_hh",
                   "depchldh", "depchldh_ch", "gvtregn", "ethgrphh",
                   "ethgrphh_2f", "loneparenthh", "babyhh", "youngmumhh",
                   "singlehh", "foodsec", "imds")), fct_explicit_na) %>%

  # exclude data from 2021
  weightstozero(exclude = 27)

# tidy ADULT dataset ----------------------------------------------------

# add adult weights and poverty flags

pov_hbai <- tidyhbai %>%
  select(year, sernum, benunit, gs_newad, adultb, adulth, depchldh, depchldb,
         low50ahc, low60ahc, low50bhc, low60bhc, low60ahcabs, low60bhcabs,
         low50ahc_dis, low60ahc_dis, low50bhc_dis, low60bhc_dis,
         foodsec, gvtregn)

tidyadult <- left_join(adult, pov_hbai, by = c("sernum", "benunit", "year")) %>%
  mutate(adultwgt = gs_newad / adultb) %>%

  # missing gvtregn indicates people in the FRS but not the HBAI -> exclude
  filter(!is.na(gvtregn)) %>%

  # add factor labels
  mutate(yearn = factor(year, levels = labels$years$years,
                        labels = labels$years$numbered),
         yearn = as.numeric(yearn),
         marital = factor(marital,
                          levels = labels[["marital"]]$codes,
                          labels = labels[["marital"]]$labels),
         sex = factor(sex,
                      levels = c(1, 2),
                      labels = c("Male", "Female")),
         sidqn = factor(sidqn, levels = labels$sexid$codes,
                        labels = labels$sexid$labels),

         # recode, category change in 1314
         corign = case_when(yearn < 19 & corign %in% c(6, 7, 8, 9) ~ 10,
                            # Poland
                            yearn == 19 & corign == 9 & corigoth == 616 ~ 9,
                            # India
                            yearn == 19 & corign == 9 & corigoth == 356 ~ 7,
                            # Pakistan
                            yearn == 19 & corign == 9 & corigoth == 586 ~ 8,
                            # other
                            yearn == 19 & corign %in% c(6, 7, 8) ~ 10,
                            TRUE ~ corign),
         corign = factor(corign, levels = labels$corign$codes,
                         labels = labels$corign$labels),
         religsc = factor(religsc,
                          levels = labels[["religion"]]$codes,
                          labels = labels[["religion"]]$labels),
         ageband = case_when(age <= 24 ~ "16-24",
                             age >= 25 & age <= 34 ~ "25-34",
                             age >= 35 & age <= 44 ~ "35-44",
                             age >= 45 & age <= 54 ~ "45-54",
                             age >= 55 & age <= 64 ~ "55-64",
                             age > 65 ~ "65+"),
         hdage = case_when(hdage == 1 ~ "16-24",
                           hdage == 2 ~ "25-34",
                           hdage == 3 ~ "35-44",
                           hdage == 4 ~ "45-54",
                           hdage == 5 ~ "55-64",
                           hdage == 6 ~ "65+"),
         ageband = factor(ageband),
         empstatc = factor(empstatc, levels = labels$empstatc$codes,
                           labels = labels$empstatc$labels),
         empstati = factor(empstati, levels = labels$empstati$codes,
                           labels = labels$empstati$labels))  %>%
  mutate_at(vars(c("marital", "religsc", "ageband", "sidqn", "corign",
                   "empstatc", "empstati")),
            fct_explicit_na) %>%

  # exclude data from 2021
  weightstozero(exclude = 27)

# tidy CHILD dataset ----------------------------------------------------

pov_hbai <- tidyhbai %>%
  select(year, sernum, benunit, yearn, gs_newch, depchldb,
         low50ahc, low60ahc, low50bhc, low60bhc, low60ahcabs, low60bhcabs,
         low50ahc_dis, low60ahc_dis, low50bhc_dis, low60bhc_dis,
         cmdahc, cmdbhc,
         foodsec, gvtregn)

tidychild <- left_join(child, pov_hbai, by = c("sernum", "benunit", "year")) %>%

  # missing gvtregn indicates people in the FRS but not the HBAI -> exclude
  filter(!is.na(gvtregn)) %>%

  # add factor labels
  mutate(childwgt = gs_newch / depchldb,
         ageband = case_when(age <= 4 ~ "0-4 years",
                             age >= 5 & age <= 12 ~ "5-12 years",
                             age >= 13 ~ "13 years and over"),
         ageband = fct_explicit_na(ageband)) %>%

  # exclude data from 2021
  weightstozero(exclude = 27)

# save all ---------------------------------------------------------------------

saveRDS(tidyhbai, "data/tidyhbai.rds")
saveRDS(tidyadult, "data/tidyadult.rds")
saveRDS(tidychild, "data/tidychild.rds")

rm(list = ls())

cat("Tidy datasets created", fill = TRUE)


