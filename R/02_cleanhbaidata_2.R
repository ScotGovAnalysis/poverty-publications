
# Clean HBAI datasets to reduce size and address variable name changes


source("R/00_strings.R")
source("R/00_functions.R")


hbai <- readRDS("data/hbai_training.rds")

colnames(hbai) <- tolower(colnames(hbai))

hbai <- hbai %>%
  select(sernum, benunit, gvtregn, year,

         # weights
         gs_newch, gs_newwa, gs_newpn, gs_newad, gs_newpp, gs_newbu,

         # characteristics
         depchldb, depchldh, adultb, adulth,
         kid0_1, kid2_4, kid5_7, kid8_10, kid11_12, kid13_15, kid16plus,
         agehd, agesp, sexhd, sexsp,
         ecobu, kidecobu, newfambu, tenhbai,
         discorabflg, discorkid,
         ethgrphh,

         # poverty flags
         mdch, mdpn,

         #mdchnew,
         low50ahc, low60ahc, low70ahc,
         low50bhc, low60bhc, low70bhc,
         low60ahcabs, low60bhcabs,

         # abs. poverty thresholds in latest survey year prices
         abs1011ahc, abs1011bhc,

         # deflators for latest (publication) year
         ahcpubdef, bhcpubdef,

         # deflators for survey year
         ahcyrdef, bhcyrdef,

         # in-(survey-)year deflators
         ahcdef, bhcdef,

         # equivalence factors
         eqoahchh, eqobhchh,

         # incomes
         s_oe_ahc, s_oe_bhc,
         esgjobhh, esgrsehh, esbenihh, esgocchh, esginvhh,
         esmischh, espribhh, inchilhh, chbenhh, esginchh)

# get both mdch and mdchnew for 2010/11

hbai1011 <- readRDS("data/hbai_clean.rds")[[17]] %>%
  select(sernum, benunit, mdch, mdchnew) %>%
  filter(!is.na(mdch) & !is.na(mdchnew)) %>%
  rename(mdchold = mdch)

attr(hbai1011$sernum, "format.sas") <- NULL

hbai <- left_join(hbai, hbai1011, by = c("sernum", "benunit")) %>%
  mutate(mdch = ifelse(year == 17, mdchold, mdch)) %>%
  select(-mdchold)


# Save and clear work space ----------------------------------------------------
#saveRDS(hbai_clean, "data/hbai_clean.rds")
#rm(list = ls())
