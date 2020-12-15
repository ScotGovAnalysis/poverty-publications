
# Prepare minimal tidy datasets


# Load helpers and clean datasets
source("R/00_functions.R")
source("R/00_strings.R")

years <- labels[["years"]]$years

hbai <- readRDS("data/hbai_clean.rds")
househol <- readRDS("data/househol_clean.rds")
adult <- readRDS("data/adult_clean.rds")
child <- readRDS("data/child_clean.rds")
benefits <- readRDS("data/benefits_clean.rds")

# Create tidy HBAI dataset ----------------------------------------------------------------------------


# get flags for poverty outcomes and hhld characteristics ---------------------------------------------
hbai <- lapply(hbai, getdisabilitybenefits)
hbai <- lapply(hbai, gethhbaby)
hbai <- lapply(hbai, gethhyoungmum)
hbai <- lapply(hbai, geturbanrural)
hbai <- lapply(hbai, gethhworkstatus)
hbai <- lapply(hbai, gethhdisabledstatus)
hbai <- lapply(hbai, gethhloneparentstatus)
hbai <- lapply(hbai, getchildweights)
hbai <- lapply(hbai, getpovertyflags)
hbai <- lapply(hbai, getpovdisabilityflags)
hbai <- lapply(hbai, gethhsinglegender)


# add factor labels -----------------------------------------------------------------------------------

for (year in years) {

  df <- hbai[[year]]

  df <- df %>%
    mutate(ecobu = factor(ecobu,
                          levels = labels[["economic"]]$codes,
                          labels = labels[["economic"]]$labels),
           kidecobu = factor(kidecobu,
                          levels = labels[["kideconomic"]]$codes,
                          labels = labels[["kideconomic"]]$labels),
           newfambu = factor(newfambu,
                          levels = labels[["familytype"]]$codes,
                          labels = labels[["familytype"]]$labels),
           ptentyp2 = factor(ptentyp2,
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
  mutate_at(vars(c("ecobu", "kidecobu", "newfambu", "ptentyp2", "urinds",
                   "workinghh", "disch_hh", "disad_hh", "dispp_hh", "depchldh",
                   "depchldh_ch", "gvtregn", "ethgrphh", "ethgrphh_2f",
                   "loneparenthh", "babyhh", "youngmumhh", "singlehh")),
            fct_explicit_na)

  hbai[[year]] <- df

}

# create tidy hbai dataset for linking with adult dataset
tidyhbai <- hbai

saveRDS(tidyhbai, "data/tidyhbai.rds")

# Create tidy ADULT dataset ---------------------------------------------------------------------------

# store year in comment attribute of each data frame --------------------------------------------------
names(adult) <- years

# add adult weights and poverty flags -----------------------------------------------------------------
adult <- lapply(adult, addpovflagsnadultwgt)

# add factor labels -----------------------------------------------------------------------------------
for (year in years) {

  df <- adult[[year]]

  df <- df %>%
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

  adult[[year]] <- df

}


saveRDS(adult, "data/tidyadult.rds")

rm(list = ls())
