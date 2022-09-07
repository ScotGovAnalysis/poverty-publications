
# Clean HBAI datasets: address variable changes, combine into single data frame

source("R/00_strings.R")
source("R/00_functions.R")

years = factor(labels$years$years,
               levels = labels$years$years)

hbai_clean <- vector("list", length(years))
names(hbai_clean) <- years

files_hbai <- readRDS("data/files_hbai.rds")

# Variable changes -------------------------------------------------------------

# ecobu and kidecobu new in 9697
# tenure new in 0304
# mdch new in 0405
# kid16_19 was kid16_18 until 0506
# mdpn new in 0910
# mdchnew in 1011 only
# ethgrphh categories changed in 1213 and a few times before
# discorabflg and discorkid new in 1213; previously, discorabflg = disabflg, discorkid = diskid;
# kid16_19 changed to kid16plus 2021
# hhshare new from 2021, before: hhstat (from 9697)

# 9495 to 9596 -----------------------------------------------------------------
for (year in years[1:2]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           ecobu = NA,
           kidecobu = NA,
           chbenhh = NA,
           hhshare = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare)

  hbai_clean[[year]] <- nextdataset

}
# 9697 to 9900 -----------------------------------------------------------------
for (year in years[3:6]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           chbenhh = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset

}
# 0001 to 0203 -----------------------------------------------------------------
for (year in years[7:9]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset

}
# 0304 -------------------------------------------------------------------------
for (year in years[10]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}
# 0405 to 0506 -----------------------------------------------------------------
for (year in years[11:12]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdchnew = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}
# 0607 to 0809 -----------------------------------------------------------------
for (year in years[13:15]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdpn = NA,
           mdchnew = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_19) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}
# 0910 -------------------------------------------------------------------------
for (year in years[16]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdchnew = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_19) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}
# 1011 -------------------------------------------------------------------------
for (year in years[17]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_19) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}
# 1112 -------------------------------------------------------------------------
for (year in years[18]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdchnew = NA,
           hhshare = hhstat,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16plus = kid16_19) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
}

# 1213 to 1920 -----------------------------------------------------------------
for (year in years[19:26]) {

nextdataset <- files_hbai[[year]]

colnames(nextdataset) <- tolower(colnames(nextdataset))

nextdataset <- nextdataset %>%
  mutate(mdchnew = NA,
         hhshare = hhstat,
         kid16plus = kid16_19) %>%
  select(sernum, benunit, gvtregn,
         gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
         depchldb, depchldh,
         adultb, adulth,
         kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
         mdch, mdchnew, mdpn,
         low50ahc, low60ahc, low70ahc,
         low50bhc, low60bhc, low70bhc,
         s_oe_ahc, s_oe_bhc,
         mdoeahc, mdoebhc,
         ahcdef, bhcdef, eqoahchh, eqobhchh,
         esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
         esmischh, espribhh, inchilhh, chbenhh, esginchh,
         ecobu, kidecobu, newfambu, ptentyp2,
         sexhd, sexsp, agehd, agesp,
         discorabflg, discorkid,
         ethgrphh, hhshare,
         esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
         esmischh, espribhh, inchilhh, chbenhh)

hbai_clean[[year]] <- nextdataset
remove(nextdataset)
}

# 2021 to latest year ----------------------------------------------------------
for (year in years[27:length(years)]) {

  nextdataset <- files_hbai[[year]]

  colnames(nextdataset) <- tolower(colnames(nextdataset))

  nextdataset <- nextdataset %>%
    mutate(mdchnew = NA) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh,
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
           mdch, mdchnew, mdpn,
           low50ahc, low60ahc, low70ahc,
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc,
           mdoeahc, mdoebhc,
           ahcdef, bhcdef, eqoahchh, eqobhchh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh, esginchh,
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp, agehd, agesp,
           discorabflg, discorkid,
           ethgrphh, hhshare,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
           esmischh, espribhh, inchilhh, chbenhh)

  hbai_clean[[year]] <- nextdataset
  remove(nextdataset)
}


for (year in years) {
  hbai_clean[[year]]$year <- year
}

# Combine into single data frame -----------------------------------------------
hbai_clean <- do.call(rbind, hbai_clean)

# remove some attributes to avoid warnings -------------------------------------
attr(hbai_clean$sernum, "format.sas") <- NULL
attr(hbai_clean$sernum, "label") <- NULL
attr(hbai_clean$benunit, "format.sas") <- NULL
attr(hbai_clean$benunit, "label") <- NULL

# Save and clear work space  ---------------------------------------------------
saveRDS(hbai_clean, "data/hbai_clean.rds")
rm(list = ls())
