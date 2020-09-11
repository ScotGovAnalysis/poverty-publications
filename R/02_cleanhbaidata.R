
# Clean datasets to reduce size and address variable name changes

source("R/00_strings.R")
source("R/00_functions.R")


  #############
  #   HBAI    #    
  #############


hbai_clean <- vector("list", length(years))
names(hbai_clean) <- years

# Variable changes

# ecobu and kidecobu new in 9697
# tenure new in 0304
# mdch new in 0405
# kid16_19 was kid16_18 until 0506
# mdpn new in 0910
# mdchnew in 1011 only
# ethgrphh new in 1213, not needed before as we don't do the time series
# discorabflg and discorkid new in 1213; previously, discorabflg = disabflg, discorkid = diskid;

# From 9495 to 9596
for (year in years[1:2]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           ecobu = NA,
           kidecobu = NA,
           chbenhh = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16_19 = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh)
  
  hbai_clean[[year]] <- nextdataset 
  
}
# From 9697 to 9900
for (year in years[3:6]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           chbenhh = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16_19 = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
  
}
# From 0001 to 0203
for (year in years[7:9]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           ptentyp2 = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16_19 = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
  
}
# For 0304
for (year in years[10]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdch = NA,
           mdchnew = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16_19 = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# From 0405 to 0506
for (year in years[11:12]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdchnew = NA,
           discorabflg = disabflg,
           discorkid = diskid,
           kid16_19 = kid16_18) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# From 0607 to 0809
for (year in years[13:15]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdpn = NA,
           mdchnew = NA,
           discorabflg = disabflg,
           discorkid = diskid) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# For 0910
for (year in years[16]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdchnew = NA,
           discorabflg = disabflg,
           discorkid = diskid) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# For 1011
for (year in years[17]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           discorabflg = disabflg,
           discorkid = diskid) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# For 1112
for (year in years[18]){
  
  nextdataset <- readRDS("data/files_hbai.rds")[[year]]
  
  colnames(nextdataset) <- tolower(colnames(nextdataset))
  
  nextdataset <- nextdataset %>%
    mutate(ethgrphh = NA,
           mdchnew = NA,
           discorabflg = disabflg,
           discorkid = diskid) %>%
    select(sernum, benunit, gvtregn,
           gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
           depchldb, depchldh, 
           adultb, adulth,
           kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
           mdch, mdchnew, mdpn, 
           low50ahc, low60ahc, low70ahc, 
           low50bhc, low60bhc, low70bhc,
           s_oe_ahc, s_oe_bhc, 
           ecobu, kidecobu, newfambu, ptentyp2,
           sexhd, sexsp,
           discorabflg, discorkid,
           ethgrphh,
           esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
           esmischh, espribhh, inchilhh, chbenhh)
  
  hbai_clean[[year]] <- nextdataset 
}
# From 1213 to latest year
for (year in years[19:length(years)]){

nextdataset <- readRDS("data/files_hbai.rds")[[year]]

colnames(nextdataset) <- tolower(colnames(nextdataset))
  
nextdataset <- nextdataset %>%
  mutate(mdchnew = NA) %>%
  select(sernum, benunit, gvtregn,
         gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,
         depchldb, depchldh, 
         adultb, adulth,
         kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16_19,
         mdch, mdchnew, mdpn, 
         low50ahc, low60ahc, low70ahc, 
         low50bhc, low60bhc, low70bhc,
         s_oe_ahc, s_oe_bhc, 
         ecobu, kidecobu, newfambu, ptentyp2,
         sexhd, sexsp,
         discorabflg, discorkid,
         ethgrphh,
         esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh, 
         esmischh, espribhh, inchilhh, chbenhh)

hbai_clean[[year]] <- nextdataset 

}


# Add inflation index
inflationindex <- readRDS("data/inflationindex.rds")
for (year in years){
  
  df <- hbai_clean[[year]]
  df$infl_ahc <- filter(inflationindex, years == year) %>% select(infl_ahc) %>% pull()
  df$infl_bhc <- filter(inflationindex, years == year) %>% select(infl_bhc) %>% pull()
  hbai_clean[[year]] <- df 
}

# Add absolute poverty thresholds
abspovthresholds <- hbai_clean[["1011"]] %>%
  summarise(abspovahc_threshold = 0.6 * wtd.quantile(s_oe_ahc*infl_ahc, probs = 0.5, weights = gs_newpp),
            abspovbhc_threshold = 0.6 * wtd.quantile(s_oe_bhc*infl_bhc, probs = 0.5, weights = gs_newpp)) %>%
  select(abspovahc_threshold, abspovbhc_threshold)

for (year in years){
  df <- hbai_clean[[year]] 
  df$abspovahc_threshold <- abspovthresholds[[1]]
  df$abspovbhc_threshold <- abspovthresholds[[2]]

  hbai_clean[[year]] <- df 
}

# Add factor levels and labels
for (year in years){
  
  df <- hbai_clean[[year]] %>%
    mutate(ecobu = factor(ecobu, levels = ecocodes, labels = econames),
           ecobu = forcats::fct_explicit_na(ecobu),
           kidecobu = factor(kidecobu, levels = kidecocodes, labels = kideconames),
           kidecobu = forcats::fct_explicit_na(kidecobu),
           newfambu = factor(newfambu, levels = famcodes, labels = famnames),
           newfambu = forcats::fct_explicit_na(newfambu),
           ptentyp2 = factor(ptentyp2, levels = tenurecodes, labels = tenurenames),
           ptentyp2 = forcats::fct_explicit_na(ptentyp2),
           depchldh = factor(depchldh, levels = childnocodes, labels = childnolabels),
           depchldh = forcats::fct_explicit_na(depchldh),
           gvtregn = factor(gvtregn, levels = regioncodes, labels = regionnames))
  
  hbai_clean[[year]] <- df
}

# Last: Add year attribute in comment to each dataset
for (year in years){
  df <- hbai_clean[[year]] 
  attr(df, "comment") <- year
  hbai_clean[[year]] <- df 
}

# Save and clear work space
saveRDS(hbai_clean, "data/hbai_clean.rds")
rm(list = ls())
