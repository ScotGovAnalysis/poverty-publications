
# Load helpers and clean datasets
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

# Create tidy HBAI dataset -----------------------------------------------------

# get flags for poverty outcomes and hhld characteristics

# disability benefit amounts
hbai <- left_join(hbai, benefits, by = "sernum")

# baby in household
babyhh <- child %>%
  mutate(baby = ifelse(age < 1, 1, 0)) %>%
  group_by(sernum) %>%
  summarise(babyhh = max(baby))

hbai <- left_join(hbai, babyhh, by = "sernum")

# young mother in household
youngmumhh <- adult %>%
  # filter for parents only
  filter_at(vars(r01, r02, r03, r04, r05, r06, r07,
                 r08, r09, r10, r11, r12, r13, r14),
            any_vars(. %in% c(7, 8))) %>%
  # filter for mothers and by age
  mutate(youngmum = ifelse(age < 25 & sex == 2, 1, 0)) %>%
  group_by(sernum) %>%
  summarise(youngmumhh = max(youngmum))

hbai <- left_join(hbai, youngmumhh, by = "sernum")

# urban/rural
urinds <- househol %>% select(sernum, urinds)
hbai <- left_join(hbai, urinds, by = "sernum")

# food security
foodsec <- getfoodsec(househol)
hbai <- left_join(hbai, foodsec, by = "sernum") %>%
  mutate(foodsec = ifelse(year >= 26, foodsec, NA),
         foodsec_score = ifelse(year >= 26, foodsec_score, NA))

# household work status
workinghh <- hbai %>%
  mutate(working = ifelse(ecobu %in% labels[["economic"]]$codes[1:5],
                          1, 0)) %>%
  group_by(sernum) %>%
  summarise(workinghh = max(working))

hbai <- left_join(hbai, workinghh, by = "sernum")

# household disability status
disabledhh <- hbai %>%
  group_by(sernum) %>%
  summarise(disch_hh = max(discorkid),
            disad_hh = max(discorabflg)) %>%
  mutate(disch_hh = ifelse(disch_hh > 0, 1, 0),
         disad_hh = ifelse(disad_hh > 0, 1, 0),
         dispp_hh = ifelse(disch_hh + disad_hh > 0, 1, 0 ))

hbai <- left_join(hbai, disabledhh, by = "sernum")

# lone parent in household
loneparenthh <- hbai %>%
  mutate(loneparent = ifelse(newfambu == 5, 1, 0)) %>%
  group_by(sernum) %>%
  summarise(loneparenthh = max(loneparent))

hbai <- left_join(hbai, loneparenthh, by = "sernum")

# child weights (by age)
hbai <- hbai %>%
  mutate(kid0_4 = kid0_1 + kid2_4,
         kid5_12 = kid5_7 + kid8_10 + kid11_12,
         kid13_19 = kid13_15 + kid16plus,
         wgt0_4 = ifelse(kid0_4 > 0, gs_newch * kid0_4 / depchldb, 0),
         wgt5_12 = ifelse(kid5_12 > 0, gs_newch * kid5_12 / depchldb, 0),
         wgt13_19 = ifelse(kid13_19 > 0, gs_newch * kid13_19 / depchldb, 0))

# over-65s weights for "pensioner" deprivation
hbai <- hbai %>%
  mutate(wgt65 = case_when(agehd >= 65 & agesp >= 65 ~ 2 * gs_newbu,
                           agehd >= 65 | agesp >= 65 ~ gs_newbu,
                           TRUE ~ 0))

# poverty flags
hbai <- hbai %>%
  mutate(workpovahc = ifelse(low60ahc == 1 & workinghh == 1, 1, 0),
         workpovbhc = ifelse(low60bhc == 1 & workinghh == 1, 1, 0),
         cmdahc = ifelse(low70ahc == 1 & mdch == 1, 1, 0),
         cmdahc_new = ifelse(low70ahc == 1 & mdchnew == 1, 1, 0),
         cmdbhc = ifelse(low70bhc == 1 & mdch == 1, 1, 0),
         cmdbhc_new = ifelse(low70bhc == 1 & mdchnew == 1, 1, 0))

# poverty disability flags
hbai <- hbai %>%
  group_by(year) %>%
  mutate(benamt = ifelse(is.na(benamt), 0,
                         ifelse(benamt < 0, 0, benamt)),
         s_oe_ahc_dis = s_oe_ahc - benamt * ahcdef / eqoahchh,
         relpovahc_dis_threshold = 0.6 * wtd.quantile(s_oe_ahc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         sevpovahc_dis_threshold = 0.5 * wtd.quantile(s_oe_ahc_dis,
                                                      probs = 0.5,
                                                      weights = gs_newpp),
         low60ahc_dis = ifelse(s_oe_ahc_dis < relpovahc_dis_threshold, 1, 0),
         low50ahc_dis = ifelse(s_oe_ahc_dis < sevpovahc_dis_threshold, 1, 0)) %>%
  ungroup()

# gender of single adult households
singlehhgender <- hbai %>%
  filter(adulth == 1) %>%
  mutate(singlehh = case_when(gs_newpn > 0 & sexhd == 1 ~ 1,
                              gs_newpn > 0 & sexhd == 2 ~ 2,
                              gs_newpn == 0 & sexhd == 1 & depchldh == 0 ~ 3,
                              gs_newpn == 0 & sexhd == 2 & depchldh == 0 ~ 4,
                              sexhd == 1 & depchldh > 0 ~ 5,
                              sexhd == 2 & depchldh > 0 ~ 6)) %>%
  select(sernum, benunit, singlehh)

hbai <- left_join(hbai, singlehhgender, by = c("sernum", "benunit"))

# add factor labels
hbai <- hbai %>%
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
           ethgrphh = factor(ethgrphh,
                             levels = labels[["ethnic"]]$codes,
                             labels = labels[["ethnic"]]$labels),
           ethgrphh_2f = factor(ethgrphh_2f,
                                levels = labels[["ethnic_2f"]]$codes,
                                labels = labels[["ethnic_2f"]]$labels),
           singlehh = factor(singlehh,
                             levels = labels[["gender"]]$codes,
                             labels = labels[["gender"]]$labels))  %>%
    mutate_at(vars(c("ecobu", "kidecobu", "newfambu", "tenhbai", "urinds",
                     "workinghh", "disch_hh", "disad_hh", "dispp_hh", "depchldh",
                     "depchldh_ch", "gvtregn", "ethgrphh", "ethgrphh_2f",
                     "loneparenthh", "babyhh", "youngmumhh", "singlehh")),
              fct_explicit_na)

# create tidy hbai dataset for linking with adult dataset
tidyhbai <- hbai

saveRDS(tidyhbai, "data/tidyhbai.rds")

# Create tidy ADULT dataset ----------------------------------------------------

# add adult weights and poverty flags
pov_hbai <- tidyhbai %>%
  select(sernum, benunit, gs_newad, adultb, low50ahc, low60ahc, gvtregn)

adult <- left_join(adult, pov_hbai, by = c("sernum", "benunit")) %>%
  mutate(adultwgt = gs_newad / adultb)

# add factor labels
adult <- adult %>%
    mutate(marital = factor(marital,
                            levels = labels[["marital"]]$codes,
                            labels = labels[["marital"]]$labels),
           religsc = factor(religsc,
                            levels = labels[["religion"]]$codes,
                            labels = labels[["religion"]]$labels),
           ageband = case_when(age <= 24 ~ "16-24",
                               age >= 25 & age <= 34 ~ "25-34",
                               age >= 35 & age <= 44 ~ "35-44",
                               age >= 45 & age <= 54 ~ "45-54",
                               age >= 55 & age <= 64 ~ "55-64",
                               age >= 65 ~ "65+"),
           hdage = case_when(hdage == 1 ~ "16-24",
                             hdage == 2 ~ "25-34",
                             hdage == 3 ~ "35-44",
                             hdage == 4 ~ "45-54",
                             hdage == 5 ~ "55-64",
                             hdage == 6 ~ "65+"),
           ageband = ifelse(is.na(ageband), hdage, ageband),
           ageband = factor(ageband))  %>%
    mutate_at(vars(c("marital", "religsc", "ageband")), fct_explicit_na)

saveRDS(adult, "data/tidyadult.rds")

rm(list = ls())
