
library(tidyverse)
library(openxlsx)
library(haven)
library(stringr)
library(Hmisc)
library(scales)

# Data prep ----

# replace categories with other categories
decode <- function(x, search, replace, default = NULL) {
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
      } else {
        function(x) ifelse(x == search[1L],
                           replace[1L],
                           decode.fun(tail(search,  -1L),
                                      tail(replace, -1L),
                                      default)(x))
        }

  return(decode.fun(search, replace, default)(x))
}

gethhworkstatus <- function(df){

  # get household level work status
  workinghh <- df %>%
    mutate(working = ifelse(ecobu %in% labels[["economic"]]$codes[1:5],
                            1, 0)) %>%
    group_by(sernum) %>%
    summarise(workinghh = max(working))

  df %>%
    left_join(workinghh, by = "sernum")
}

gethhdisabledstatus <- function(df){

  # get household level disability status
  disabledhh <- df %>%
    group_by(sernum) %>%
    summarise(disch_hh = max(discorkid),
              disad_hh = max(discorabflg)) %>%
    mutate(disch_hh = ifelse(disch_hh > 0, 1, 0),
           disad_hh = ifelse(disad_hh > 0, 1, 0),
           dispp_hh = ifelse(disch_hh + disad_hh > 0, 1, 0 ))

  df %>%
    left_join(disabledhh, by = "sernum")
}

gethhloneparentstatus <- function(df){

  loneparenthh <- df %>%
    mutate(loneparent = ifelse(newfambu == 5, 1, 0)) %>%
    group_by(sernum) %>%
    summarise(loneparenthh = max(loneparent))

  df %>%
    left_join(loneparenthh, by = "sernum")
}

gethhsinglegender <- function(df) {

  singlehhgender <- df %>%
    filter(adulth == 1) %>%
    mutate(singlehh = case_when(gs_newpn > 0 & sexhd == 1 ~ 1,
                                gs_newpn > 0 & sexhd == 2 ~ 2,
                                gs_newpn == 0 & sexhd == 1 & depchldh == 0 ~ 3,
                                gs_newpn == 0 & sexhd == 2 & depchldh == 0 ~ 4,
                                sexhd == 1 & depchldh > 0 ~ 5,
                                sexhd == 2 & depchldh > 0 ~ 6)) %>%
    select(sernum, benunit, singlehh)

  df %>%
    left_join(singlehhgender, by = c("sernum", "benunit"))
}

getdisabilitybenefits <- function(df, benefits_datasets){

  # get the correct FRS househol dataset
  # (current year is stored in "comment" attribute)
  year <- comment(df)
  disbens <- benefits_datasets[[year]]

  # remove some attributes to avoid warnings
  attr(disbens$sernum, "format.sas") <- NULL
  attr(disbens$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL

  # join with hbai dataset
  df <- df %>%
    left_join(disbens, by = "sernum")

  attr(df, "comment") <- year
  df
}

gethhbaby <- function(df, child_datasets){

  # get the correct FRS child dataset
  # (current year is stored in "comment" attribute)
  year <- comment(df)
  child <- child_datasets[[year]] %>%
    mutate(baby = ifelse(age < 1, 1, 0)) %>%
    group_by(sernum) %>%
    summarise(babyhh = max(baby))

  # remove some attributes to avoid warnings
  attr(child$sernum, "format.sas") <- NULL
  attr(child$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL

  # join with hbai dataset
  df <- df %>%
    left_join(child, by = "sernum")

  attr(df, "comment") <- year
  df
}

gethhyoungmum <- function(df, adult_datasets){

  # get the correct FRS adult dataset
  # (current year is stored in "comment" attribute)
  year <- comment(df)
  adult <- adult_datasets[[year]] %>%
    # filter for parents only
    filter_at(vars(r01, r02, r03, r04, r05, r06, r07,
                   r08, r09, r10, r11, r12, r13, r14),
              any_vars(. %in% c(7, 8))) %>%
    # filter for mothers and by age
    mutate(youngmum = ifelse(age < 25 & sex == 2, 1, 0)) %>%
    group_by(sernum) %>%
    summarise(youngmumhh = max(youngmum))

  # remove some attributes to avoid warnings
  attr(adult$sernum, "format.sas") <- NULL
  attr(adult$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL

  # join with hbai dataset
  df <- df %>%
    left_join(adult, by = "sernum")

  attr(df, "comment") <- year
  df
}

# get flags for poverty outcomes
getpovertyflags <- function(df){

  df %>%
    mutate(abspovahc = ifelse(s_oe_ahc < abspovahc_threshold *
                                inflahc_1011 / infl_ahc, 1, 0),
           abspovbhc = ifelse(s_oe_bhc < abspovbhc_threshold *
                                inflbhc_1011 / infl_bhc, 1, 0),
           workpovahc = ifelse(low60ahc == 1 & workinghh == 1, 1, 0),
           workpovbhc = ifelse(low60bhc == 1 & workinghh == 1, 1, 0),
           cmdahc = ifelse(low70ahc == 1 & mdch == 1, 1, 0),
           cmdahc_new = ifelse(low70ahc == 1 & mdchnew == 1, 1, 0),
           cmdbhc = ifelse(low70bhc == 1 & mdch == 1, 1, 0),
           cmdbhc_new = ifelse(low70bhc == 1 & mdchnew == 1, 1, 0))
}

getpovdisabilityflags <- function(df){
  df %>%
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
           low50ahc_dis = ifelse(s_oe_ahc_dis < sevpovahc_dis_threshold, 1, 0))

}

geturbanrural <- function(df, househol_datasets) {

  # get the correct FRS househol dataset
  # (current year is stored in "comment" attribute)
  year <- comment(df)
  urindshh <- househol_datasets[[year]]

  # remove some attributes to avoid warnings
  attr(urindshh$sernum, "format.sas") <- NULL
  attr(urindshh$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL

  # join with hbai dataset
  df <- df %>%
    left_join(urindshh, by = "sernum")

  attr(df, "comment") <- year
    df
}

getfoodsec <- function(df, househol_datasets) {

  year <- comment(df)

  foodsec <- househol_datasets[[year]] %>%
    replace(is.na(.), 0) %>%
    mutate(foodsec_score = 0,
           foodsec_score = ifelse(foodq1 %in% c(1, 2), foodsec_score + 1,
                                  foodsec_score),
           foodsec_score = ifelse(foodq2 %in% c(1, 2), foodsec_score + 1,
                                  foodsec_score),
           foodsec_score = ifelse(foodq3 %in% c(1, 2), foodsec_score + 1,
                                  foodsec_score),
           foodsec_score = ifelse(foodq4a == 1, foodsec_score + 1, foodsec_score),
           foodsec_score = ifelse(foodq4b >= 3 | foodq4c == 1, foodsec_score + 1,
                                  foodsec_score),
           foodsec_score = ifelse(foodq5 == 1, foodsec_score + 1, foodsec_score),
           foodsec_score = ifelse(foodq6 == 1, foodsec_score + 1, foodsec_score),
           foodsec_score = ifelse(foodq7 == 1, foodsec_score + 1, foodsec_score),
           foodsec_score = ifelse(foodq8a == 1, foodsec_score + 1, foodsec_score),
           foodsec_score = ifelse(foodq8b >= 3 | foodq8c == 1, foodsec_score + 1,
                                  foodsec_score),
           foodsec = case_when(foodsec_score %in% c(1, 2) ~ 2,
                               foodsec_score %in% c(3, 4, 5) ~ 3,
                               foodsec_score >= 6 ~ 4,
                               TRUE ~ 1),
           foodsec = factor(foodsec, levels = c(1, 2, 3, 4),
                            labels = c("high", "marginal", "low", "very low"),
                            ordered = TRUE)) %>%
    select(sernum, foodsec_score, foodsec)

  # join with hbai dataset
  df <- df %>%
    left_join(foodsec, by = "sernum")

  attr(df, "comment") <- year
  df
}

getchildweights <- function(df){

  df %>%
    mutate(kid0_4 = kid0_1 + kid2_4,
           kid5_12 = kid5_7 + kid8_10 + kid11_12,
           kid13_19 = kid13_15 + kid16_19,
           wgt0_4 = ifelse(kid0_4 > 0, gs_newch * kid0_4 / depchldb, 0),
           wgt5_12 = ifelse(kid5_12 > 0, gs_newch * kid5_12 / depchldb, 0),
           wgt13_19 = ifelse(kid13_19 > 0, gs_newch * kid13_19 / depchldb, 0))
}

getpmdweights <- function(df) {

  df %>%
    mutate(wgt65 = case_when(agehd >= 65 & agesp >= 65 ~ 2 * gs_newbu,
                             agehd >= 65 | agesp >= 65 ~ gs_newbu,
                             TRUE ~ 0))
}

# get poverty flags and adult weight from tidy hbai dataset
addpovflagsnadultwgt <- function(df, hbai_datasets){

  # get the correct tidy hbai dataset
  # current year is stored in "comment" attribute
  pov_hbai <- hbai_datasets[[comment(df)]]

  # remove some attributes to avoid warnings
  attr(pov_hbai$sernum, "format.sas") <- NULL
  attr(pov_hbai$sernum, "label") <- NULL
  attr(pov_hbai$benunit, "format.sas") <- NULL
  attr(pov_hbai$benunit, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL
  attr(df$benunit, "format.sas") <- NULL
  attr(df$benunit, "label") <- NULL

  pov_hbai <- pov_hbai %>%
    select(sernum, benunit, gs_newad, adultb, low50ahc, low60ahc, gvtregn)

  # join with adult dataset
  df %>%
    left_join(pov_hbai, by = c("sernum", "benunit")) %>%
    mutate(adultwgt = gs_newad / adultb)
}

# Data analysis and restructure ----

# get poverty numbers and rates for children, adults, pensioners, and people
getpov <- function(df, povvar){

  df$povvar <- df[[povvar]]

  df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm = TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    group_by(povvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm = TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    mutate(chrate = chnum / chn,
           warate = wanum / wan,
           pnrate = pnnum / pnn,
           pprate = ppnum / ppn,
           adrate = adnum / adn) %>%
    select(ppnum, chnum, wanum, pnnum, adnum,
           pprate, chrate, warate, pnrate, adrate, povvar,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn,
           groupsample_ad, povsample, povsample_ch, povsample_wa, povsample_pn,
           povsample_ad) %>%
    filter(povvar == 1) %>%
    ungroup() %>%
    select(-povvar)

}

# get pensioner (over-65s) material deprivation
getpmd <- function(df){

  df$povvar <- df[["mdpn"]]

  df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(pnn = sum(wgt65),
           groupsample_pn = sum(wgt65 > 0, na.rm = TRUE)) %>%
    group_by(povvar) %>%
    summarise(pnnum = sum(wgt65),
              pnn = max(pnn),
              groupsample_pn = max(groupsample_pn),
              povsample_pn = sum(wgt65 > 0, na.rm = TRUE)) %>%
    mutate(pnrate = pnnum / pnn) %>%
    select(pnnum, pnrate, povvar, groupsample_pn, povsample_pn) %>%
    filter(povvar == 1) %>%
    ungroup() %>%
    select(-povvar)

}

# get poverty numbers and rates by grouping variable
getpovby <- function(df, povvar, groupingvar){

  df$povvar <- df[[povvar]]
  df$groupingvar <- df[[groupingvar]]

  grouped <- df %>%
    filter(gvtregn == "Scotland") %>%
    group_by(groupingvar) %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm = TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    group_by(povvar, groupingvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm = TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum / chn,
           warate = wanum / wan,
           pnrate = pnnum / pnn,
           pprate = ppnum / ppn,
           adrate = adnum / adn,
           ppcomp = ppnum / sum(ppnum),
           chcomp = chnum / sum(chnum),
           wacomp = wanum / sum(wanum),
           pncomp = pnnum / sum(pnnum),
           adcomp = adnum / sum(adnum)) %>%
    ungroup() %>%
    select(groupingvar,
           ppnum, chnum, wanum, pnnum, adnum,
           pprate, chrate, warate, pnrate, adrate,
           ppcomp, chcomp, wacomp, pncomp, adcomp,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn,
           groupsample_ad, povsample, povsample_ch, povsample_wa, povsample_pn,
           povsample_ad)

  total <- df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm = TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    group_by(povvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm = TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum / chn,
           warate = wanum / wan,
           pnrate = pnnum / pnn,
           pprate = ppnum / ppn,
           adrate = adnum / adn,
           ppcomp = ppnum / sum(ppnum),
           chcomp = chnum / sum(chnum),
           wacomp = wanum / sum(wanum),
           pncomp = pnnum / sum(pnnum),
           adcomp = adnum / sum(adnum),
           groupingvar = "All") %>%
    ungroup() %>%
    select(groupingvar,
           ppnum, chnum, wanum, pnnum, adnum,
           pprate, chrate, warate, pnrate, adrate,
           ppcomp, chcomp, wacomp, pncomp, adcomp,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn,
           groupsample_ad, povsample, povsample_ch, povsample_wa, povsample_pn,
           povsample_ad)

  rbind(total, grouped) %>%
    mutate(groupingvar = factor(groupingvar,
                                levels = as.character(groupingvar)))

}

# get poverty number and rates for child age groups
getpovbychildage <- function(df, povvar){

  df$povvar <- df[[povvar]]

  df %>%
    filter(gvtregn == "Scotland",
           gs_newch > 0) %>%
    mutate(n0_4 = sum(wgt0_4),
           n5_12 = sum(wgt5_12),
           n13_19 = sum(wgt13_19),
           n_ch = sum(gs_newch),
           groupsample0_4 = sum(wgt0_4 > 0, na.rm = TRUE),
           groupsample5_12 = sum(wgt5_12 > 0, na.rm = TRUE),
           groupsample13_19 = sum(wgt13_19 > 0, na.rm = TRUE),
           groupsample_ch = n()) %>%
    group_by(povvar) %>%
    summarise(num0_4 = sum(wgt0_4),
              num5_12 = sum(wgt5_12),
              num13_19 = sum(wgt13_19),
              num_ch = sum(gs_newch),
              n0_4 = max(n0_4),
              n5_12 = max(n5_12),
              n13_19 = max(n13_19),
              n_ch = max(n_ch),
              groupsample0_4 = max(groupsample0_4),
              groupsample5_12 = max(groupsample5_12),
              groupsample13_19 = max(groupsample13_19),
              groupsample_ch = max(groupsample_ch),
              povsample0_4 = sum(wgt0_4 > 0, na.rm = TRUE),
              povsample5_12 = sum(wgt5_12 > 0, na.rm = TRUE),
              povsample13_19 = sum(wgt13_19 > 0, na.rm = TRUE),
              povsample_ch = n()) %>%
    filter(povvar == 1) %>%
    mutate(rate0_4 = num0_4 / n0_4,
           rate5_12 = num5_12 / n5_12,
           rate13_19 = num13_19 / n13_19,
           rate_ch = num_ch / n_ch,
           comp0_4 = num0_4 / num_ch,
           comp5_12 = num5_12 / num_ch,
           comp13_19 = num13_19 / num_ch,
           comp_ch = 1) %>%
    ungroup() %>%
    select(num0_4, num5_12, num13_19, num_ch,
           comp0_4, comp5_12, comp13_19, comp_ch,
           rate0_4, rate5_12, rate13_19, rate_ch,
           groupsample0_4, groupsample5_12, groupsample13_19, groupsample_ch,
           povsample0_4, povsample5_12, povsample13_19, povsample_ch)

}

# get poverty numbers and rates by grouping variable - tidy adult dataset
getpovby_adult <- function(df, povvar, groupingvar){

  df$povvar <- df[[povvar]]
  df$groupingvar <- df[[groupingvar]]

  grouped <- df %>%
    filter(gvtregn == "Scotland") %>%
    group_by(groupingvar) %>%
    mutate(adn = sum(adultwgt),
           groupsample_ad = sum(adultwgt > 0, na.rm = TRUE)) %>%
    group_by(povvar, groupingvar) %>%
    summarise(adnum = sum(adultwgt),
              adn = max(adn),
              groupsample_ad = max(groupsample_ad),
              povsample_ad = sum(adultwgt > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(adrate = adnum / adn,
           adcomp = adnum / sum(adnum)) %>%
    ungroup() %>%
    select(groupingvar,
           adnum, adrate, adcomp,
           groupsample_ad, povsample_ad)

  total <- df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(adn = sum(adultwgt),
           groupsample_ad = sum(adultwgt > 0, na.rm = TRUE)) %>%
    group_by(povvar) %>%
    summarise(adnum = sum(adultwgt),
              adn = max(adn),
              groupsample_ad = max(groupsample_ad),
              povsample_ad = sum(adultwgt > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(adrate = adnum / adn,
           adcomp = adnum / sum(adnum),
           groupingvar = "All") %>%
    ungroup() %>%
    select(groupingvar,
           adnum, adrate, adcomp,
           groupsample_ad, povsample_ad)

  rbind(total, grouped) %>%
    mutate(groupingvar = factor(groupingvar,
                                levels = as.character(groupingvar)))

}

getpovbynation <- function(df, povvar){

  df$povvar <- df[[povvar]]

  grouped <- df %>%
    group_by(gvtregn) %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm = TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    group_by(povvar, gvtregn) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm = TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum / chn,
           warate = wanum / wan,
           pnrate = pnnum / pnn,
           pprate = ppnum / ppn,
           adrate = adnum / adn) %>%
    ungroup() %>%
    select(gvtregn,
           pprate, chrate, warate, pnrate, adrate,
           ppnum, chnum, wanum, pnnum, adnum,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn,
           groupsample_ad, povsample, povsample_ch, povsample_wa, povsample_pn,
           povsample_ad)

  total <- df %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm = TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    group_by(povvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm = TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm = TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm = TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum / chn,
           warate = wanum / wan,
           pnrate = pnnum / pnn,
           pprate = ppnum / ppn,
           adrate = adnum / adn,
           gvtregn = "UK") %>%
    ungroup() %>%
    select(gvtregn,
           pprate, chrate, warate, pnrate, adrate,
           ppnum, chnum, wanum, pnnum, adnum,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn,
           groupsample_ad,
           povsample, povsample_ch, povsample_wa, povsample_pn, povsample_ad)

  rbind(total, grouped) %>%
    mutate(gvtregn = factor(gvtregn, levels = as.character(gvtregn)))
}

getworkpovbynation <- function(df, povvar){

  df$povvar <- df[[povvar]]

  grouped <- df %>%
    group_by(gvtregn, workinghh) %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa)) %>%
    group_by(povvar, gvtregn, workinghh) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa)) %>%
    filter(povvar == 1) %>%
    mutate(chcomp = chnum / sum(chnum),
           wacomp = wanum / sum(wanum)) %>%
    filter(workinghh == "Someone in paid work") %>%
    ungroup() %>%
    select(gvtregn, chcomp, wacomp)

  total <- df %>%
    group_by(workinghh) %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa)) %>%
    group_by(povvar, workinghh) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa)) %>%
    filter(povvar == 1) %>%
    mutate(chcomp = chnum / sum(chnum),
           wacomp = wanum / sum(wanum)) %>%
    filter(workinghh == "Someone in paid work") %>%
    ungroup() %>%
    mutate(gvtregn = "UK") %>%
    select(gvtregn, chcomp, wacomp)

  rbind(total, grouped) %>%
    mutate(gvtregn = factor(gvtregn, levels = as.character(gvtregn)))

}

getpriority <- function(df = hbai, povvar, groupingvar, group) {

  do.call(rbind.data.frame,
          lapply(df, getpovby, povvar = povvar,
                 groupingvar = groupingvar)) %>%
    addyearvar %>%
    group_by(groupingvar) %>%
    get3yrtable() %>%
    filter(years == max(levels(periods)),
           groupingvar == group) %>%
    samplesizecheck_num %>%
    select(years, groupingvar, chrate, groupsample_ch) %>%
    ungroup() %>%
    mutate(povvar = povvar,
           groupingvar = as.character(groupingvar))
}

checkandfmtUK <- function(df){

  df %>%
    group_by(gvtregn) %>%
    arrange(gvtregn, years) %>%
    mutate_at(vars(c(ends_with("rate")), ends_with("num"), ends_with("comp")),
              get3yraverage) %>%
    mutate_at(vars(contains("sample")), get3yrtotal) %>%
    mutate_at(vars(ends_with("num")), roundpop) %>%
    mutate_at(vars(c(ends_with("rate"), ends_with("comp"))), roundpct) %>%
    samplesizecheck %>%
    mutate(years = factor(years,
                          levels = labels[["years"]]$years,
                          labels = labels[["years"]]$periods)) %>%
    ungroup() %>%
    filter(!is.na(pprate))
}

# Add year variable to table
addyearvar <- function(df){

  df %>%
    rownames_to_column(var = "years") %>%
    mutate(years = str_sub(years, 1L, 4L),
           years = factor(years,
                          levels = unique(labels[["years"]]$years),
                          ordered = TRUE)) %>%
    select(years, everything())
}

getmediansbhc <- function(df){

  df %>%
    filter(gvtregn == "Scotland") %>%
    summarise(pp = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                weights = gs_newpp),
              ch = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                weights = gs_newch),
              wa = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                weights = gs_newwa),
              pn = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                weights = gs_newpn))

}

getmediansahc <- function(df){

  df %>%
    filter(gvtregn == "Scotland") %>%
    summarise(pp = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                weights = gs_newpp),
              ch = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                weights = gs_newch),
              wa = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                weights = gs_newwa),
              pn = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                weights = gs_newpn))

}

getdecptsbhc <- function(df){

  df <- df %>%
    filter(gvtregn == "Scotland")

  as.data.frame(wtd.quantile(df$s_oe_bhc * df$infl_bhc,
                             probs = seq(0.1, 0.9, 0.1),
                             weights = df$gs_newpp) ) %>% t()

}

getdecptsahc <- function(df){

  df <- df %>%
    filter(gvtregn == "Scotland")

  as.data.frame(wtd.quantile(df$s_oe_ahc * df$infl_ahc,
                             probs = seq(0.1, 0.9, 0.1),
                             weights = df$gs_newpp) ) %>% t()

}

getdecsharesbhc <- function(df){

  df <- df %>%
    filter(gvtregn == "Scotland")

  decs <- wtd.quantile(df$s_oe_bhc,
                       probs = seq(0.1, 1, 0.1),
                       weights = df$gs_newpp)

  df %>%
    mutate(decbhc = case_when(s_oe_bhc <= decs[1] ~ 1,
                              s_oe_bhc <= decs[2] ~ 2,
                              s_oe_bhc <= decs[3] ~ 3,
                              s_oe_bhc <= decs[4] ~ 4,
                              s_oe_bhc <= decs[5] ~ 5,
                              s_oe_bhc <= decs[6] ~ 6,
                              s_oe_bhc <= decs[7] ~ 7,
                              s_oe_bhc <= decs[8] ~ 8,
                              s_oe_bhc <= decs[9] ~ 9,
                              s_oe_bhc >  decs[9] ~ 10)) %>%
    group_by(decbhc) %>%
    summarise(share = 365/7 * sum(s_oe_bhc * infl_bhc * gs_newpp)) %>%
    spread(decbhc, share)

}

getdecsharesahc <- function(df){

  df <- df %>%
    filter(gvtregn == "Scotland")

  decs <- wtd.quantile(df$s_oe_ahc,
                       probs = seq(0.1, 1, 0.1),
                       weights = df$gs_newpp)

  df %>%
    mutate(decahc = case_when(s_oe_ahc <= decs[1] ~ 1,
                              s_oe_ahc <= decs[2] ~ 2,
                              s_oe_ahc <= decs[3] ~ 3,
                              s_oe_ahc <= decs[4] ~ 4,
                              s_oe_ahc <= decs[5] ~ 5,
                              s_oe_ahc <= decs[6] ~ 6,
                              s_oe_ahc <= decs[7] ~ 7,
                              s_oe_ahc <= decs[8] ~ 8,
                              s_oe_ahc <= decs[9] ~ 9,
                              s_oe_ahc >  decs[9] ~ 10)) %>%
    group_by(decahc) %>%
    summarise(share = 365/7 * sum(s_oe_ahc * infl_ahc * gs_newpp)) %>%
    spread(decahc, share)

}

getpalmabhc <- function(df){

  shares <- getdecsharesbhc(df)
  Palma <- shares[10] / sum(shares[1:4])
  colnames(Palma) <- "Palma"
  Palma
}

getpalmaahc <- function(df){

  shares <- getdecsharesahc(df)
  Palma <- shares[10] / sum(shares[1:4])
  colnames(Palma) <- "Palma"
  Palma
}

gini <- function(x, weights = rep(1, length = length(x))) {
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox] / sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu / nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

getginibhc <- function(df){

  df %>%
    filter(gvtregn == "Scotland") %>%
    summarise(Gini = gini(s_oe_bhc, weights = gs_newpp))
}


getginiahc <- function(df){

  df %>%
    filter(gvtregn == "Scotland") %>%
    summarise(Gini = gini(s_oe_ahc, weights = gs_newpp))
}

getpovertythresholdsbhc <- function(df){

  df1 <- df %>%
    mutate(UKmedian = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                   weights = gs_newpp),
           relpovbhc_threshold = 0.6 * UKmedian,
           abspovbhc_threshold = abspovbhc_threshold * inflbhc_1011 / infl_bhc) %>%
    filter(gvtregn == "Scotland") %>%
    summarise(UKmedian = max(UKmedian),
              Scotmedian = wtd.quantile(s_oe_bhc * infl_bhc, probs = 0.5,
                                        weights = gs_newpp),
              relpovbhc_threshold = max(relpovbhc_threshold),
              abspovbhc_threshold = max(abspovbhc_threshold))

  df2 <- as.data.frame(getdecptsbhc(filter(df, gvtregn == "Scotland")) )

  df <- cbind(df1, df2)
  row.names(df) <- NULL

  df <- as.data.frame(t(df)) %>%
    rownames_to_column(var = "measure") %>%
    rename(weekly2 = V1) %>%
    mutate(annual2 = weekly2 * 365/7,
           weekly1 = weekly2 * 0.67,
           annual1 = weekly1 * 365/7,
           weekly3 = weekly2 * 1.2,
           annual3 = weekly3 * 365/7,
           weekly4 = weekly2 * 1.53,
           annual4 = weekly4 * 365/7) %>%
    select(measure, weekly1, annual1, everything())

  df$measure <- decode(df$measure,
                       search = c("UKmedian", "Scotmedian",
                                  "relpovbhc_threshold", "abspovbhc_threshold",
                                  "10%", "20%", "30%", "40%", "50%", "60%",
                                  "70%", "80%", "90%"),
                       replace = c("UK median income", "Scottish median income",
                                   "Relative poverty threshold (60% of UK median income)",
                                   "Absolute poverty threshold (60% of inflation-adjusted 2010/11 UK median income)",
                                   "Scottish 1st income decile point",
                                   "Scottish 2nd income decile point",
                                   "Scottish 3rd income decile point",
                                   "Scottish 4th income decile point",
                                   "Scottish 5th income decile point",
                                   "Scottish 6th income decile point",
                                   "Scottish 7th income decile point",
                                   "Scottish 8th income decile point",
                                   "Scottish 9th income decile point"))

  df
}

getpovertythresholdsahc <- function(df){

  df1 <- df %>%
    mutate(UKmedian = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                   weights = gs_newpp),
           relpovahc_threshold = 0.6 * UKmedian,
           abspovahc_threshold = abspovahc_threshold * inflahc_1011 / infl_ahc) %>%
    filter(gvtregn == "Scotland") %>%
    summarise(UKmedian = max(UKmedian),
              Scotmedian = wtd.quantile(s_oe_ahc * infl_ahc, probs = 0.5,
                                        weights = gs_newpp),
              relpovahc_threshold = max(relpovahc_threshold),
              abspovahc_threshold = max(abspovahc_threshold))

  df2 <- as.data.frame(getdecptsahc(filter(df, gvtregn == "Scotland")) )

  df <- cbind(df1, df2)
  row.names(df) <- NULL

  df <- as.data.frame(t(df)) %>%
    rownames_to_column(var = "measure") %>%
    rename(weekly2 = V1) %>%
    mutate(annual2 = weekly2 * 365/7,
           weekly1 = weekly2 * 0.58,
           annual1 = weekly1 * 365/7,
           weekly3 = weekly2 * 1.2,
           annual3 = weekly3 * 365/7,
           weekly4 = weekly2 * 1.62,
           annual4 = weekly4 * 365/7) %>%
    select(measure, weekly1, annual1, everything())

  df$measure <- decode(df$measure,
                       search = c("UKmedian", "Scotmedian",
                                  "relpovahc_threshold", "abspovahc_threshold",
                                  "10%", "20%", "30%", "40%", "50%", "60%",
                                  "70%", "80%", "90%"),
                       replace = c("UK median income", "Scottish median income",
                                   "Relative poverty threshold (60% of UK median income)",
                                   "Absolute poverty threshold (60% of inflation-adjusted 2010/11 UK median income)",
                                   "Scottish 1st income decile point",
                                   "Scottish 2nd income decile point",
                                   "Scottish 3rd income decile point",
                                   "Scottish 4th income decile point",
                                   "Scottish 5th income decile point",
                                   "Scottish 6th income decile point",
                                   "Scottish 7th income decile point",
                                   "Scottish 8th income decile point",
                                   "Scottish 9th income decile point"))

  df
}

getsources <- function(df){

  data <- df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(earns = (esgjobhh + esgrsehh) * bhcdef / eqobhchh,
           bens = (esbenihh + chbenhh) * bhcdef / eqobhchh,
           pen = esgocchh * bhcdef / eqobhchh,
           inv = esginvhh * bhcdef / eqobhchh,
           misc = (esmischh + espribhh + inchilhh) * bhcdef / eqobhchh,
           total = esginchh * bhcdef / eqobhchh) %>%
    select(gvtregn, s_oe_bhc, infl_bhc, gs_newpp, earns, bens, pen, inv, misc,
           total)

  decs <- as.data.frame(wtd.quantile(data$s_oe_bhc,
                                     probs = seq(0.1, 0.9, 0.1),
                                     weights = data$gs_newpp) ) %>% t()

  data <- data %>%
    mutate(decbhc = case_when(s_oe_bhc <= decs[1] ~ 1,
                              s_oe_bhc <= decs[2] ~ 2,
                              s_oe_bhc <= decs[3] ~ 3,
                              s_oe_bhc <= decs[4] ~ 4,
                              s_oe_bhc <= decs[5] ~ 5,
                              s_oe_bhc <= decs[6] ~ 6,
                              s_oe_bhc <= decs[7] ~ 7,
                              s_oe_bhc <= decs[8] ~ 8,
                              s_oe_bhc <= decs[9] ~ 9,
                              s_oe_bhc >  decs[9] ~ 10))

  bydec <- data  %>%
    group_by(decbhc) %>%
    summarise(earnings_rate = sum(earns * gs_newpp) / sum(total * gs_newpp),
              benefits_rate = sum(bens * gs_newpp) / sum(total * gs_newpp),
              occpens_rate = sum(pen * gs_newpp) / sum(total * gs_newpp),
              investments_rate = sum(inv * gs_newpp) / sum(total * gs_newpp),
              other_rate = sum(misc * gs_newpp) / sum(total * gs_newpp)) %>%
    ungroup() %>%
    mutate(decbhc = factor(decbhc))

  tot <- data %>%
    summarise(earnings_rate = sum(earns * gs_newpp) / sum(total * gs_newpp),
              benefits_rate = sum(bens * gs_newpp) / sum(total * gs_newpp),
              occpens_rate = sum(pen * gs_newpp) / sum(total * gs_newpp),
              investments_rate = sum(inv * gs_newpp) / sum(total * gs_newpp),
              other_rate = sum(misc * gs_newpp) / sum(total * gs_newpp)) %>%
    mutate(decbhc = "All")

  rbind(bydec, tot)
}

getdistribution <- function(df) {

  df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(income = s_oe_bhc * infl_bhc) %>%
    select(gs_newpp, income)
}

getUKdeciles <- function(df) {

  df %>%
    summarise(x = list(enframe(wtd.quantile(s_oe_bhc * infl_bhc,
                                          probs = seq(0.1, 1, 0.1),
                                          weights = gs_newpp)))) %>%
    unnest(x)

}

splitntranspose <- function(df, measure){

  df$measure <- df[["measure"]]

  df %>%
    select(years, groupingvar, measure) %>%
    spread(years, measure)

}

getpersistentpoverty <- function(df) {

  names(df)[1] <- "nation"

  df %>%
    filter_at(1, all_vars(. %in% c("Total", "England", "Scotland", "Wales",
                                   "Northern Ireland"))) %>%
    gather(period, value, -nation)
}

# Formatting and rounding ----

round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

fmtpop <- function(x) {

  require(scales)

  ifelse(!is.na(x), comma(x), NA)
}

roundpop <- function(x) {

  require(scales)

  ifelse(!is.na(x), round2(x, -4), NA)
}

fmtpct <- function(x) {

  require(scales)

  ifelse(!is.na(x), percent2(x), NA)
}

roundpct <- function(x) {

  require(scales)

  ifelse(!is.na(x), round2(x, 2), NA)
}

# Change scales::percent() and scales::comma() functions
# to ensure correct rounding

percent2 <- function(x) {paste0(round2(x, 2) * 100, "%")}

comma2 <- function(x, accuracy = 1, scale = 1, prefix = "") {

  y <- round2(x, accuracy)
  scales::comma(x = y, accuracy = accuracy, scale = scale, prefix = prefix)
}


formatpov <- function(df){

  df %>%
    mutate(years = factor(years,
                          levels = labels[["years"]]$years,
                          labels = labels[["years"]]$formatted)) %>%
    mutate_at(vars(ends_with("num")), roundpop) %>%
    mutate_at(vars(ends_with("rate")), roundpct)
}

# Averages ----

get3yrcentavg <- function(x) {
  x <- (x + lag(x) + lead(x)) / 3
  }
get3yraverage <- function(x) {
  x <- (x + lag(x, 1L) + lag(x, 2L)) / 3
  }
get5yraverage <- function(x) {
  x <- (x + lag(x, 1L) + lag(x, 2L) + lag(x, 3L) + lag(x, 4L)) / 5
  }

get3yrtotal <- function(x) {
  x <- x + lag(x, 1L) + lag(x, 2L)
  }
get5yrtotal <- function(x) {
  x <- x + lag(x, 1L) + lag(x, 2L) + lag(x, 3L) + lag(x, 4L)
  }


get3yrtable <- function(df) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              get3yraverage) %>%
    mutate_at(vars(contains("sample")), get3yrtotal) %>%
    tail(-2L) %>%
    ungroup() %>%
    mutate(years = factor(years,
                          levels = labels[["years"]]$years,
                          labels = labels[["years"]]$periods),
           years = factor(years))
}

get5yrtable <- function(df) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              get5yraverage) %>%
    mutate_at(vars(contains("sample")), get5yrtotal) %>%
    tail(-4L) %>%
    ungroup() %>%
    mutate(years = factor(years,
                          levels = labels[["years"]]$years,
                          labels = labels[["years"]]$period5yr))
}

formatpov3yraverage <- function(df){

  df %>%
    get3yrtable() %>%
    mutate_at(vars(contains("num")), roundpop) %>%
    mutate_at(vars(contains(c("rate")), contains("comp")), roundpct)
}

formatpovby3yraverage <- function(df){

df %>%
  group_by(groupingvar) %>%
  arrange(groupingvar, years) %>%
  mutate_at(vars(c(ends_with("rate")), ends_with("num"), ends_with("comp")),
            get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate_at(vars(ends_with("num")), roundpop) %>%
  mutate_at(vars(c(ends_with("rate"), ends_with("comp"))), roundpct) %>%
  filter(groupingvar != "(Missing)") %>%
  ungroup() %>%
  filter(!is.na(adnum))
}

formatpovby5yraverage <- function(df){

  df %>%
    group_by(groupingvar) %>%
    arrange(groupingvar, years) %>%
    mutate_at(vars(c(ends_with("rate")), ends_with("num"), ends_with("comp")),
              get5yraverage) %>%
    mutate_at(vars(contains("sample")), get5yrtotal) %>%
    mutate_at(vars(ends_with("num")), roundpop) %>%
    mutate_at(vars(c(ends_with("rate"), ends_with("comp"))), roundpct) %>%
    filter(groupingvar != "(Missing)") %>%
    ungroup() %>%
    filter(!is.na(adnum))
}

# Sample size checks ----

samplesizecheck <- function(df){
  df %>%
    mutate(ppnum = ifelse(povsample < 100, NA, ppnum),
           pprate = ifelse(groupsample < 100, NA, pprate),
           chnum = ifelse(povsample_ch < 100, NA, chnum),
           chrate = ifelse(groupsample_ch < 100, NA, chrate),
           wanum = ifelse(povsample_wa < 100, NA, wanum),
           warate = ifelse(groupsample_wa < 100, NA, warate),
           pnnum = ifelse(povsample_pn < 100, NA, pnnum),
           pnrate = ifelse(groupsample_pn < 100, NA, pnrate),
           adnum = ifelse(povsample_ad < 100, NA, adnum),
           adrate = ifelse(groupsample_ad < 100, NA, adrate))
}

samplesizecheck_num <- function(df){
  df %>%
    mutate(ppnum = ifelse(povsample < 100, NA, ppnum),
           pprate = ifelse(groupsample < 100, NA, pprate),
           chnum = ifelse(povsample_ch < 100, NA, chnum),
           chrate = ifelse(groupsample_ch < 100, NA, chrate),
           wanum = ifelse(povsample_wa < 100, NA, wanum),
           warate = ifelse(groupsample_wa < 100, NA, warate),
           pnnum = ifelse(povsample_pn < 100, NA, pnnum),
           pnrate = ifelse(groupsample_pn < 100, NA, pnrate),
           adnum = ifelse(povsample_ad < 100, NA, adnum),
           adrate = ifelse(groupsample_ad < 100, NA, adrate))
}

samplesizecheck_childage <- function(df){
  df %>%
    mutate(num0_4 = ifelse(povsample0_4 < 100, NA, num0_4),
           rate0_4 = ifelse(groupsample0_4 < 100, NA, rate0_4),
           num5_12 = ifelse(povsample5_12 < 100, NA, num5_12),
           rate5_12 = ifelse(groupsample5_12 < 100, NA, rate5_12),
           num13_19 = ifelse(povsample13_19 < 100, NA, num13_19),
           rate13_19 = ifelse(groupsample13_19 < 100, NA, rate13_19))
}

samplesizecheck_ad_num <- function(df){
  df %>%
    mutate(adnum = ifelse(povsample_ad < 100, NA, adnum),
           adrate = ifelse(groupsample_ad < 100, NA, adrate))
}

samplesizecheck_ad <- function(df){
  df %>%
    mutate(adnum = ifelse(povsample_ad < 100, NA, adnum),
           adrate = ifelse(groupsample_ad < 100, NA, adrate))
}

# Spreadsheet functions ----


# Add header above header with merged cells
addUberheader <- function(wb = wb, sheet = 1, uberheaders = uberheaders,
                          row = 5){

  # Do we have an uberheader?
  if (is.vector(uberheaders)) {

  # Merged cells for uberheader

  uh <- data.frame("headers" = names(uberheaders), "cells" = uberheaders) %>%
    mutate(cumcells = cumsum(cells),
           startcol = 2,
           startcol = ifelse(row_number() == 1, 2, lag(startcol + cumcells)),
           endcol = startcol + cells - 1,
           colstring = str_c(startcol, ":", endcol),
           label = strrep(str_c(headers, ","), cells))

  uhlist <- vector("list", dim(uh)[1])

  for (i in 1:length(uhlist)){
    uhlist[[i]] <- c(uh$startcol[i], uh$endcol[i])
  }

  uhlabels <- paste(uh$label, sep = ",", collapse = "")
  uhlabels <- str_split(uhlabels, ",")
  uhlabels <- as.data.frame(t(as.data.frame(uhlabels))) %>%
    select(-last_col())

  lapply(uhlist, function(x) mergeCells(wb, sheet, cols = x[1]:x[2],
                                        rows = row))

  writeData(wb, sheet, uhlabels, startRow = row, startCol = 2, colNames = FALSE)
  }

}

# Create spreadsheet for headline poverty spreadsheets
createSpreadsheet <- function(data){

  df <- data[["df"]]
  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  title <- data[["title"]]
  subtitle <- data[["subtitle"]]
  headers <- data[["headers"]]
  uberheaders <- data[["uberheaders"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]

  # Styles for Excel outputs

  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "black")

  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  subtitleStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                               wrapText = FALSE, halign = "left")
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 10,
                             halign = "right", border = "bottom",
                             wrapText = FALSE)
  uberheaderStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 10,
                                 halign = "center",
                                 border = "TopBottomLeftRight",
                                 borderColour = "#D3D3D3", borderStyle = "thin",
                                 wrapText = TRUE)

  popStyle <- createStyle(numFmt = "#,##0", halign = "right")
  pctStyle <- createStyle(numFmt = "0%", halign = "right")
  popStyle_underline <- createStyle(numFmt = "#,##0", halign = "right",
                                    border = "bottom")
  pctStyle_underline <- createStyle(numFmt = "0%", halign = "right",
                                    border = "bottom")
  sourceStyle <- createStyle(fontName = "Segoe UI", fontSize = 10,
                             wrapText = FALSE, halign = "left")
  footnoteHeaderStyle <- createStyle(fontName = "Segoe UI Semibold",
                                     fontSize = 11, textDecoration = "BOLD",
                                     wrapText = FALSE, halign = "left")
  footnoteStyle <- createStyle(fontName = "Segoe UI", fontSize = 11,
                               wrapText = FALSE, halign = "left")

  # Calculate body dimensions
  endcol <- length(df) + 1
  endrow <- dim(df)[1] + 6

  # Get column numbers for populations and percentages
  nums <- grep("num", names(df), fixed = TRUE)
  rates <- grep("rate", names(df), fixed = TRUE)

  if (length(nums) > 0) {
    startcol_pop <- nums[1] + 1
    endcol_pop <- nums[length(nums)] + 1
  } else {
    startcol_pop <- 3
    endcol_pop <- length(df) + 1
  }

  if (length(rates > 0)) {
    startcol_pct <- rates[1] + 1
    endcol_pct <- rates[length(rates)] + 1
  } else {
    startcol_pct <- 1
    endcol_pct <- 1
  }

  # Transform headers into a data frame so they can be written as data
  headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))

  # If workbook already exists, open it and add sheet,
  # otherwise create new workbook
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)) {
      removeWorksheet(wb, sheetname)
      }
    } else {wb <- createWorkbook()}

  addWorksheet(wb, sheetname, gridLines = FALSE)

  # Title row
  writeData(wb, sheetname, title, startRow = 2, startCol = 2)

  # Subtitle row
  writeData(wb, sheetname, subtitle, startRow = 3, startCol = 2)

  # Uber header (above headers)
  addUberheader(wb, sheetname, uberheaders)

  # Data / body (with header)
  writeData(wb, sheetname, df, startRow = 6, startCol = 2,
            keepNA = TRUE, na.string = "--")
  writeData(wb, sheetname, headers, startRow = 6, startCol = 2,
            colNames = FALSE)

  # Data source
  writeData(wb, sheetname, source, startRow = endrow + 1, startCol = 2)

  # Footnotes
  if (is.vector(footnotes)) {
    writeData(wb, sheetname, "Notes", startRow = endrow + 3, startCol = 2)
    writeData(wb, sheetname, footnotes, startRow = endrow + 4, startCol = 2)
  }

  # Add styles

  # Title row
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle)

  # Subtitle row
  addStyle(wb, sheetname, rows = 3, cols = 2, style = subtitleStyle)

  # Uber header (above headers)
  if (is.vector(uberheaders)) {
    addStyle(wb, sheetname, rows = 5, cols = 3:endcol, style = uberheaderStyle)
    setRowHeights(wb, sheetname, rows = 5, heights = 30)
  }

  # Data / body (with header)
  addStyle(wb, sheetname, rows = 6, cols = 2:endcol, style = headerStyle)

  addStyle(wb, sheetname, rows = 7:(endrow - 1), cols = startcol_pop:endcol_pop,
           style = popStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow, cols = startcol_pop:endcol_pop,
           style = popStyle_underline, gridExpand = TRUE)

  addStyle(wb, sheetname, rows = 7:(endrow - 1), cols = startcol_pct:endcol_pct,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow, cols = startcol_pct:endcol_pct,
           style = pctStyle_underline, gridExpand = TRUE)

  addStyle(wb, sheetname, rows = endrow, cols = 2,
           style = createStyle(border = "bottom"))
  addStyle(wb, sheetname, rows = 7:endrow, cols = 2,
           style = createStyle(halign = "right"), stack = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = endrow + 1, cols = 2, style = sourceStyle)

  # Footnotes
  if (is.vector(footnotes)) {
  addStyle(wb, sheetname, rows = endrow + 3, cols = 2,
           style = footnoteHeaderStyle)
  addStyle(wb, sheetname, rows = (endrow + 4):(endrow + 4 + length(footnotes)),
           cols = 2, style = footnoteStyle)
  }

  setColWidths(wb, sheetname, cols = 3:endcol, widths = 17,
               ignoreMergedCells = TRUE)

  saveWorkbook(wb, filename, overwrite = TRUE)
}

# Create spreadsheet for poverty characteristics spreadsheets
createWideSpreadsheet <- function(data){

  df1 <- data[["df1"]]
  df2 <- data[["df2"]]
  df3 <- data[["df3"]]
  df4 <- data[["df4"]]
  df5 <- data[["df5"]]
  df6 <- data[["df6"]]
  df7 <- data[["df7"]]

  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  title_a <- data[["title_a"]]
  title_b <- data[["title_b"]]
  title_c <- data[["title_c"]]
  title_d <- data[["title_d"]]
  subtitle_a <- data[["subtitle_a"]]
  subtitle_b <- data[["subtitle_b"]]
  subtitle_c <- data[["subtitle_c"]]
  subtitle_d <- data[["subtitle_d"]]
  subsubtitle_rel <- data[["subsubtitle_rel"]]
  subsubtitle_sev <- data[["subsubtitle_sev"]]
  headers <- data[["headers"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]

  # Styles for Excel outputs

  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "black")

  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  subtitleStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                               wrapText = FALSE, halign = "left")
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 10,
                             halign = "right", border = "bottom",
                             wrapText = FALSE)
  popStyle <- createStyle(numFmt = "#,##0", halign = "right")
  popStyle_underline <- createStyle(numFmt = "#,##0", halign = "right",
                                    border = "bottom")
  pctStyle <- createStyle(numFmt = "0%", halign = "right")
  pctStyle_underline <- createStyle(numFmt = "0%", halign = "right",
                                    border = "bottom")
  sourceStyle <- createStyle(fontName = "Segoe UI", fontSize = 10,
                             wrapText = FALSE, halign = "left")
  footnoteHeaderStyle <- createStyle(fontName = "Segoe UI Semibold",
                                     fontSize = 11, textDecoration = "BOLD",
                                     wrapText = FALSE, halign = "left")
  footnoteStyle <- createStyle(fontName = "Segoe UI", fontSize = 11,
                               wrapText = FALSE, halign = "left")

  # Calculate body dimensions
  endcol <- length(df1) + 1
  nrows <- dim(df1)[1]

  startrow1 <- 7
  endrow1 <- nrows + startrow1 - 1
  startrow2 <- endrow1 + 5
  endrow2 <- nrows + startrow2 - 1
  startrow3 <- endrow2 + 9
  endrow3 <- nrows + startrow3 - 1
  startrow4 <- endrow3 + 5
  endrow4 <- nrows + startrow4 - 1
  startrow5 <- endrow4 + 9
  endrow5 <- nrows + startrow5 - 1
  startrow6 <- endrow5 + 5
  endrow6 <- nrows + startrow6 - 1
  startrow7 <- endrow6 + 7
  endrow7 <- nrows + startrow7 - 1

  # Transform headers into a data frame so they can be written as data
  headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))

  # If workbook already exists, open it and add sheet,
  # otherwise create new workbook
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)) {
      removeWorksheet(wb, sheetname)
      }
    } else {wb <- createWorkbook()}

  addWorksheet(wb, sheetname, gridLines = FALSE)

  # Write data

  # "A" tables - title row
  writeData(wb, sheetname, title_a, startRow = 2, startCol = 2)

  # "A" tables = subtitle row
  writeData(wb, sheetname, subtitle_a, startRow = 3, startCol = 2)

  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = 5, startCol = 2)

  # Header 1
  writeData(wb, sheetname, headers, startRow = 6, startCol = 2,
            colNames = FALSE)

  # Data / body 1
  writeData(wb, sheetname, df1, startRow = startrow1, startCol = 2, colNames = FALSE,
            keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow1 + 1, startCol = 2)

  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow1 + 3,
            startCol = 2)

  # Header 2
  writeData(wb, sheetname, headers, startRow = startrow2 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 2
  writeData(wb, sheetname, df2, startRow = startrow2, startCol = 2,
            colNames = FALSE, keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow2 + 1, startCol = 2)

  # "B" tables - title row
  writeData(wb, sheetname, title_b, startRow = endrow2 + 4, startCol = 2)

  # "B" tables - subtitle row
  writeData(wb, sheetname, subtitle_b, startRow = endrow2 + 5, startCol = 2)

  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = endrow2 + 7,
            startCol = 2)

  # Header 3
  writeData(wb, sheetname, headers, startRow = startrow3 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 3
  writeData(wb, sheetname, df3, startRow = startrow3, startCol = 2,
            colNames = FALSE, keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow3 + 1, startCol = 2)

  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow3 + 3,
            startCol = 2)

  # Header 4
  writeData(wb, sheetname, headers, startRow = startrow4 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 4
  writeData(wb, sheetname, df4, startRow = startrow4, startCol = 2,
            colNames = FALSE, keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow4 + 1, startCol = 2)

  # "C" tables - title row
  writeData(wb, sheetname, title_c, startRow = endrow4 + 4, startCol = 2)

  # "C" tables - subtitle row
  writeData(wb, sheetname, subtitle_c, startRow = endrow4 + 5, startCol = 2)

  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = endrow4 + 7,
            startCol = 2)

  # Header 5
  writeData(wb, sheetname, headers, startRow = startrow5 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 5
  writeData(wb, sheetname, df5, startRow = startrow5, startCol = 2,
            colNames = FALSE, keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow5 + 1, startCol = 2)

  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow5 + 3,
            startCol = 2)

  # Header 6
  writeData(wb, sheetname, headers, startRow = startrow6 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 6
  writeData(wb, sheetname, df6, startRow = startrow6, startCol = 2,
            colNames = FALSE, keepNA = TRUE, na.string = "..")

  # Data source
  writeData(wb, sheetname, source, startRow = endrow6 + 1, startCol = 2)

  # "D" tables - title row
  writeData(wb, sheetname, title_d, startRow = endrow6 + 4, startCol = 2)

  # "D" tables - subtitle row
  writeData(wb, sheetname, subtitle_d, startRow = endrow6 + 5, startCol = 2)

  # Header 7
  writeData(wb, sheetname, headers, startRow = startrow7 - 1, startCol = 2,
            colNames = FALSE)

  # Data / body 7
  writeData(wb, sheetname, df7, startRow = startrow7, startCol = 2,
            colNames = FALSE)

  # Data source
  writeData(wb, sheetname, source, startRow = endrow7 + 1, startCol = 2)

  # Footnotes
  if (is.vector(footnotes)) {
    writeData(wb, sheetname, "Notes", startRow = endrow7 + 3, startCol = 2)
    writeData(wb, sheetname, footnotes, startRow = endrow7 + 4, startCol = 2)
  }

  # Add styles

  # "A" tables - title row
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle, stack = TRUE)

  # "A" tables = subtitle row
  addStyle(wb, sheetname, rows = 3, cols = 2, style = subtitleStyle, stack = TRUE)

  # Add rel pov table title
  addStyle(wb, sheetname, rows = 5, cols = 2, style = subtitleStyle, stack = TRUE)

  # Header 1
  addStyle(wb, sheetname, rows = startrow1 - 1, cols = 2:endcol,
           style = headerStyle, stack = TRUE)

  # Data / body 1
  addStyle(wb, sheetname, rows = startrow1, cols = 2:endcol, style = pctStyle_underline,
           gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow1 + 1):(endrow1 - 1), cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow1, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = endrow1 + 1, cols = 2, style = sourceStyle)

  # Add sev pov table title
  addStyle(wb, sheetname, rows = endrow1 + 3, cols = 2, style = subtitleStyle)

  # Header 2
  addStyle(wb, sheetname, rows = startrow2 - 1, cols = 2:endcol,
           style = headerStyle)

  # Data / body 2
  addStyle(wb, sheetname, rows = startrow2, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow2 + 1):(endrow2 - 1), cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow2, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = endrow2 + 1, cols = 2, style = sourceStyle)

  # "B" tables - title row
  addStyle(wb, sheetname, rows = endrow2 + 4, cols = 2, style = titleStyle)

  # "B" tables - subtitle row
  addStyle(wb, sheetname, rows = endrow2 + 5, cols = 2, style = subtitleStyle)

  # Add rel pov table title
  addStyle(wb, sheetname, rows = endrow2 + 7, cols = 2, style = subtitleStyle)

  # Header 3
  addStyle(wb, sheetname, rows = startrow3 - 1, cols = 2:endcol,
           style = headerStyle)

  # Data / body 3
  addStyle(wb, sheetname, rows = startrow3, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow3 + 1):(endrow3 - 1), cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow3, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = endrow3 + 1, cols = 2, style = sourceStyle)

  # Add sev pov table title
  addStyle(wb, sheetname, rows = (endrow3 + 3), cols = 2, style = subtitleStyle)

  # Header 4
  addStyle(wb, sheetname, rows = (startrow4 - 1), cols = 2:endcol,
           style = headerStyle)

  # Data / body 4
  addStyle(wb, sheetname, rows = startrow4, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow4 + 1):(endrow4 - 1), cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow4, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = (endrow4 + 1), cols = 2, style = sourceStyle)

  # "C" tables - title row
  addStyle(wb, sheetname, rows = (endrow4 + 4), cols = 2, style = titleStyle)

  # "C" tables - subtitle row
  addStyle(wb, sheetname, rows = (endrow4 + 5), cols = 2, style = subtitleStyle)

  # Add rel pov table title
  addStyle(wb, sheetname, rows = (endrow4 + 7), cols = 2, style = subtitleStyle)

  # Header 5
  addStyle(wb, sheetname, rows = (startrow5 - 1), cols = 2:endcol,
           style = headerStyle)

  # Data / body 5
  addStyle(wb, sheetname, rows = startrow5, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow5 + 1):(endrow5 - 1), cols = 2:endcol,
           style = popStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow5, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = (endrow5 + 1), cols = 2, style = sourceStyle)

  # Add sev pov table title
  addStyle(wb, sheetname, rows = (endrow5 + 3), cols = 2, style = subtitleStyle)

  # Header 6
  addStyle(wb, sheetname, rows = (startrow6 - 1), cols = 2:endcol,
           style = headerStyle)

  # Data / body 6
  addStyle(wb, sheetname, rows = startrow6, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow6 + 1):(endrow6 - 1), cols = 2:endcol,
           style = popStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow6, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = (endrow6 + 1), cols = 2, style = sourceStyle)

  # "D" tables - title row
  addStyle(wb, sheetname, rows = (endrow6 + 4), cols = 2, style = titleStyle)

  # "D" tables - subtitle row
  addStyle(wb, sheetname, rows = (endrow6 + 5), cols = 2, style = subtitleStyle)

  # Header 7
  addStyle(wb, sheetname, rows = (startrow7 - 1), cols = 2:endcol,
           style = headerStyle)

  # Data / body 7
  addStyle(wb, sheetname, rows = startrow7, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = (startrow7 + 1):(endrow7 - 1), cols = 2:endcol,
           style = popStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow7, cols = 2:endcol,
           style = popStyle_underline, gridExpand = TRUE)

  # Data source
  addStyle(wb, sheetname, rows = (endrow7 + 1), cols = 2, style = sourceStyle)

  # Footnotes
  if (is.vector(footnotes)) {
    addStyle(wb, sheetname, rows = (endrow7 + 3), cols = 2,
             style = footnoteHeaderStyle)
    addStyle(wb, sheetname,
             rows = (endrow7 + 4):(endrow7 + 4 + length(footnotes)),
             cols = 2, style = footnoteStyle)
  }

  # Data / body 1 (repeat)
  addStyle(wb, sheetname, rows = 7, cols = 2:endcol, style = pctStyle_underline,
           gridExpand = TRUE)
  addStyle(wb, sheetname, rows = 8:(endrow1 - 1), cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow1, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  setColWidths(wb, sheetname, cols = 2, widths = 40)
  setColWidths(wb, sheetname, cols = 3:endcol, widths = 8.5)
  saveWorkbook(wb, filename, overwrite = TRUE)
}


createUKSpreadsheet <- function(data) {

  df1 <- data[["df1"]]
  df2 <- data[["df2"]]
  df3 <- data[["df3"]]
  df4 <- data[["df4"]]

  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  title_1 <- data[["title_1"]]
  title_2 <- data[["title_2"]]
  title_3 <- data[["title_3"]]
  title_4 <- data[["title_4"]]
  subtitle_1 <- data[["subtitle_1"]]
  subtitle_2 <- data[["subtitle_2"]]
  subtitle_3 <- data[["subtitle_3"]]
  subtitle_4 <- data[["subtitle_4"]]
  headers <- data[["headers"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]

  # Styles for Excel outputs

  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "black")

  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  subtitleStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                               wrapText = FALSE, halign = "left")
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 10,
                             halign = "right", border = "bottom",
                             wrapText = FALSE)
  pctStyle <- createStyle(numFmt = "0%", halign = "right")
  pctStyle_underline <- createStyle(numFmt = "0%", halign = "right",
                                    border = "bottom")
  sourceStyle <- createStyle(fontName = "Segoe UI", fontSize = 10,
                             wrapText = FALSE, halign = "left")
  footnoteHeaderStyle <- createStyle(fontName = "Segoe UI Semibold",
                                     fontSize = 11, textDecoration = "BOLD",
                                     wrapText = FALSE, halign = "left")
  footnoteStyle <- createStyle(fontName = "Segoe UI", fontSize = 11,
                               wrapText = FALSE, halign = "left")

  # Calculate body dimensions
  endcol <- length(df1) + 1
  endrow1 <- dim(df1)[1] + 5
  endrow2 <- ifelse(is.null(df2), endrow1, endrow1 + dim(df2)[1] + 7)
  endrow3 <- ifelse(is.null(df3), endrow2, endrow2 + dim(df3)[1] + 7)
  endrow4 <- ifelse(is.null(df4), endrow3, endrow3 + dim(df4)[1] + 7)

  # Transform headers into a data frame so they can be written as data
  headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))

  # If workbook already exists, open it and add sheet,
  # otherwise create new workbook
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)) {
      removeWorksheet(wb, sheetname)
      }
    } else {wb <- createWorkbook()}

  addWorksheet(wb, sheetname, gridLines = FALSE)

  # Table 1 - title row
  writeData(wb, sheetname, title_1, startRow = 2, startCol = 2)
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle)

  # Table 1 - subtitle row
  writeData(wb, sheetname, subtitle_1, startRow = 3, startCol = 2)
  addStyle(wb, sheetname, rows = 3, cols = 2, style = subtitleStyle)

  # Header
  writeData(wb, sheetname, headers, startRow = 5, startCol = 2,
            colNames = FALSE)
  addStyle(wb, sheetname, rows = 5, cols = 2:endcol, style = headerStyle)

  # Data / body 1
  writeData(wb, sheetname, df1, startRow = 6, startCol = 2, colNames = FALSE,
            keepNA = TRUE, na.string = "--")

  addStyle(wb, sheetname, rows = 6, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = 7:endrow1, cols = 2:endcol,
           style = pctStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow1, cols = 2:endcol,
           style = pctStyle_underline, gridExpand = TRUE)

  # Data source
  writeData(wb, sheetname, source, startRow = endrow1 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow1 + 1, cols = 2, style = sourceStyle)

  if (is.data.frame(df2)) {

    # Table 2 - title row
    writeData(wb, sheetname, title_2, startRow = endrow1 + 4, startCol = 2)
    addStyle(wb, sheetname, rows = endrow1 + 4, cols = 2, style = titleStyle)

    # Table 2 - subtitle row
    writeData(wb, sheetname, subtitle_2, startRow = endrow1 + 5, startCol = 2)
    addStyle(wb, sheetname, rows = endrow1 + 5, cols = 2, style = subtitleStyle)

    # Header
    writeData(wb, sheetname, headers, startRow = endrow1 + 7, startCol = 2,
              colNames = FALSE)
    addStyle(wb, sheetname, rows = endrow1 + 7, cols = 2:endcol,
             style = headerStyle)

    # Data / body 2
    writeData(wb, sheetname, df2, startRow = endrow1 + 8, startCol = 2,
              colNames = FALSE)
    addStyle(wb, sheetname, rows = endrow1 + 8, cols = 2:endcol,
             style = pctStyle_underline, gridExpand = TRUE)
    addStyle(wb, sheetname, rows = endrow1 + 9:endrow2, cols = 2:endcol,
             style = pctStyle, gridExpand = TRUE)
    addStyle(wb, sheetname, rows = endrow2, cols = 2:endcol,
             style = pctStyle_underline, gridExpand = TRUE)

    # Data source
    writeData(wb, sheetname, source, startRow = endrow2 + 1, startCol = 2)
    addStyle(wb, sheetname, rows = endrow2 + 1, cols = 2, style = sourceStyle)

    if (is.data.frame(df3)) {

      # Table 3 - title row
      writeData(wb, sheetname, title_3, startRow = endrow2 + 4, startCol = 2)
      addStyle(wb, sheetname, rows = endrow2 + 4, cols = 2, style = titleStyle)

      # Table 3 - subtitle row
      writeData(wb, sheetname, subtitle_3, startRow = endrow2 + 5, startCol = 2)
      addStyle(wb, sheetname, rows = endrow2 + 5, cols = 2, style = subtitleStyle)

      # Header
      writeData(wb, sheetname, headers, startRow = endrow2 + 7, startCol = 2,
                colNames = FALSE)
      addStyle(wb, sheetname, rows = endrow2 + 7, cols = 2:endcol,
               style = headerStyle)

      # Data / body 3
      writeData(wb, sheetname, df3, startRow = endrow2 + 8, startCol = 2,
                colNames = FALSE)
      addStyle(wb, sheetname, rows = endrow2 + 8, cols = 2:endcol,
               style = pctStyle_underline, gridExpand = TRUE)
      addStyle(wb, sheetname, rows = endrow2 + 9:endrow3, cols = 2:endcol,
               style = pctStyle, gridExpand = TRUE)
      addStyle(wb, sheetname, rows = endrow3, cols = 2:endcol,
               style = pctStyle_underline, gridExpand = TRUE)

      # Data source
      writeData(wb, sheetname, source, startRow = endrow3 + 1, startCol = 2)
      addStyle(wb, sheetname, rows = endrow3 + 1, cols = 2, style = sourceStyle)

      # Table 4 - title row
      writeData(wb, sheetname, title_4, startRow = endrow3 + 4, startCol = 2)
      addStyle(wb, sheetname, rows = endrow3 + 4, cols = 2, style = titleStyle)

      # Table 4 - subtitle row
      writeData(wb, sheetname, subtitle_4, startRow = endrow3 + 5, startCol = 2)
      addStyle(wb, sheetname, rows = endrow3 + 5, cols = 2,
               style = subtitleStyle)

      # Header
      writeData(wb, sheetname, headers, startRow = endrow3 + 7, startCol = 2,
                colNames = FALSE)
      addStyle(wb, sheetname, rows = endrow3 + 7, cols = 2:endcol,
               style = headerStyle)

      # Data / body 4
      writeData(wb, sheetname, df4, startRow = endrow3 + 8, startCol = 2,
                colNames = FALSE)
      addStyle(wb, sheetname, rows = endrow3 + 8, cols = 2:endcol,
               style = pctStyle_underline, gridExpand = TRUE)
      addStyle(wb, sheetname, rows = endrow3 + 9:endrow4, cols = 2:endcol,
               style = pctStyle, gridExpand = TRUE)
      addStyle(wb, sheetname, rows = endrow4, cols = 2:endcol,
               style = pctStyle_underline, gridExpand = TRUE)

      # Data source
      writeData(wb, sheetname, source, startRow = endrow4 + 1, startCol = 2)
      addStyle(wb, sheetname, rows = endrow4 + 1, cols = 2, style = sourceStyle)

    }
  }

  # Footnotes
  if (is.vector(footnotes)) {
    writeData(wb, sheetname, "Notes", startRow = endrow4 + 3,
              startCol = 2)
    addStyle(wb, sheetname, rows = endrow4 + 3, cols = 2,
             style = footnoteHeaderStyle)
    writeData(wb, sheetname, footnotes, startRow = endrow4 + 4, startCol = 2)
    addStyle(wb, sheetname,
             rows = (endrow4 + 4):(endrow4 + 4 + length(footnotes)),
             cols = 2, style = footnoteStyle)

    }

  setColWidths(wb, sheetname, cols = 2, widths = 15)
  setColWidths(wb, sheetname, cols = 3:endcol, widths = "auto")
  saveWorkbook(wb, filename, overwrite = TRUE)
}

getSheetTitles <- function(filename = filename, sheetname){

  wb <- loadWorkbook(filename)
  read.xlsx(wb, sheet = sheetname, startRow = 2, colNames = FALSE,
                   cols = 2, rows = 2,
                   skipEmptyRows = TRUE) %>% pull()
}

createSepsheet <- function(filename = filename, sheetname, text) {

  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            halign = "left")
  filename <- paste0("output/", filename)

  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)) {
      removeWorksheet(wb, sheetname)
    }
  } else {wb <- createWorkbook()}

  addWorksheet(wb, sheetname, gridLines = FALSE)
  writeData(wb, sheetname, text, startRow = 2,
            startCol = 2)
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle)
  saveWorkbook(wb, filename, overwrite = TRUE)
}

createContentSheet <- function(filename){

  wb <- loadWorkbook(filename)

  # get sheet names and titles
  sheets <- names(wb)
  sheets <- sheets[!sheets == "Contents"]
  titles <- sapply(sheets, getSheetTitles, filename = filename)
  titles <- titles[!titles == "Tables"]

  # remove "A." from titles
  titles <- sapply(titles, function(i) ifelse(startsWith(i, "A. Proportion"),
                                              str_c("...by",
                                                    str_split(i, "by")[[1]][2]),
                                              i))

  # create hyperlinks to sheets
  sheetlinks <- sapply(seq_along(sheets),
                       function(i) makeHyperlinkString(sheets[[i]],
                                                       text = titles[[i]],
                                                       col = 2))

  # create new worksheet
  if ("Contents" %in% getSheetNames(filename)) {removeWorksheet(wb, "Contents")}
  addWorksheet(wb, "Contents", gridLines = FALSE)

  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  noteStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                           wrapText = TRUE, halign = "left")
  noteStyle_nowrap <- createStyle(fontName = "Segoe UI", fontSize = 12,
                           wrapText = FALSE, halign = "left")
  tocStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                          fontColour = "blue", textDecoration = "underline",
                          halign = "left")
  backButtonStyle <- createStyle(fontName = "Segoe UI", fontSize = 9,
                                 fontColour = "blue",
                                 textDecoration = c("underline", "bold"),
                                 halign = "center", fgFill = "#D3D3D3",
                                 border = "TopBottomLeftRight")

  # add title
  writeData(wb, "Contents", "Tables", startRow = 2, startCol = 2)
  addStyle(wb, "Contents", rows = 2, cols = 2, style = titleStyle)

  # add note
  note1 <- "This workbook contains data used in the charts and tables in the Poverty and Income Inequality in Scotland report, and additional analysis."
  note2 <- "https://data.gov.scot/poverty"
  names(note2) <- "The report is available here: data.gov.scot/poverty"
  class(note2) <- "hyperlink"

  writeData(wb, "Contents", note1, startRow = 3, startCol = 2)
  writeData(wb, "Contents", note2, startRow = 4, startCol = 2)
  addStyle(wb, "Contents", rows = 3:4, cols = 2, style = noteStyle)

  # add list of sheets
  writeFormula(wb, "Contents", startRow = 6, startCol = 2, x = sheetlinks)
  addStyle(wb, "Contents", rows = 6:(length(sheets) + 6), cols = 2,
           style = tocStyle)

  # add contact information
  email <- "mailto:social-justice-analysis@gov.scot"
  names(email) <- "social-justice-analysis@gov.scot"
  class(email) <- "hyperlink"

  writeData(wb, "Contents", "Contact", startRow = 2,
            startCol = 3)
  addStyle(wb, "Contents", rows = 2, cols = 3, style = titleStyle)

  writeData(wb, "Contents", "Maike Waldmann", startRow = 3, startCol = 3)
  writeData(wb, "Contents", "Scottish Government", startRow = 4, startCol = 3)
  writeData(wb, "Contents", "Communities Analysis Division", startRow = 5,
            startCol = 3)
  writeData(wb, "Contents", email, startRow = 6, startCol = 3)
  addStyle(wb, "Contents", rows = 3:6, cols = 3, style = noteStyle_nowrap)

  # add note on sample sizes
  text <- c("Some estimates are unavailable due to small sample size and marked '..'.",
            "For population estimates, the sample of the underlying population has to",
            "be at least 100 cases, for example 100 interviewed people, or 100",
            "interviewed households (depending on the question).",
            "For estimated proportions, the sample of the base has to be at least 100.",
            "For example, if there were 100 German households in the Scottish sample,",
            "and 19 of them were in poverty, we could produce an estimate of the",
            "proportion of German households in Scotland who are in poverty.",
            "However, we could not produce a population estimate of German households",
            "in poverty, as this would be based on only 19 households.",
            "Estimates of amounts, such as median incomes, are based on at least 50 cases.")

  text <- data.frame(text)

  writeData(wb, "Contents", "Note on sample sizes", startRow = 8,
            startCol = 3)
  addStyle(wb, "Contents", rows = 8, cols = 3, style = titleStyle)

  writeData(wb, "Contents", text, startRow = 9, startCol = 3, colNames = FALSE)
  addStyle(wb, "Contents", rows = 9:19, cols = 3, style = noteStyle_nowrap)

  setColWidths(wb, "Contents", 2:3, widths = 80)

  # move contents sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])

  # add back button to each sheet
  for (sheet in sheets) {
    writeFormula(wb, sheet, startRow = 1, startCol = 2,
                 x = makeHyperlinkString("Contents", row = 2, col = 2,
                                         text = "BACK"))
    addStyle(wb, sheet, rows = 1, cols = 2, style = backButtonStyle)
    setRowHeights(wb, sheet, rows = 2, heights = 33)
  }

  saveWorkbook(wb, filename, overwrite = TRUE)

}

mark_missing <- function(data = data, ncols, nrows, xlscol, xlsrow) {
  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  wb <- loadWorkbook(filename)

  df <- data.frame(matrix("--", nrows, ncols))
  startcell <- c(xlscol, xlsrow)

  writeData(
    wb,
    sheetname,
    x = df,
    xy = startcell,
    colNames = FALSE
  )

  saveWorkbook(wb, filename, overwrite = TRUE)
}

wrap_text <- function(data = data, rows, cols) {
  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  wb <- loadWorkbook(filename)
  addStyle(wb, sheetname, rows = rows, cols = cols,
           style = createStyle(wrapText = TRUE), gridExpand = TRUE,
           stack = TRUE)
}


# Chart functions ----



linechart <- function(df, GBP = FALSE, ...){

  df$key <- factor(df$key)

  if (length(levels(df$key)) > 1) {
    if (GBP) {
      df$text <- df$tooltip
    } else {
      df$text <- str_c(df$key, ": ", percent2(df$value), " (", df$years, ")")
    }
  } else {
    if (GBP) {
      df$text <- str_c(df$text, " (", df$years, ")")
    } else {
      df$text <- str_c(percent2(df$value), " (", df$years, ")")
    }
  }

  ggplot(data = df,
         aes(x = years,
             y = value,
             group = key,
             colour = key,
             linetype = key,
             label = text)) +

    addxlabels() +
    addrecessionbar(...) +

    geom_point_interactive(aes(tooltip = text,
                               data_id = paste(text, value)),
                           show.legend = FALSE,
                           size = 6,
                           colour = "white",
                           alpha = 0.01) +

    geom_line(lineend = "round",
                          show.legend = FALSE) +

    geom_point(data = filter(df, years == min(years)),
               size = 2,
               show.legend = FALSE) +

    geom_point(data = filter(df, years == max(years)),
               size = 2,
               show.legend = FALSE)
}

linechart_small <- function(df, yrange = c(0.1, 0.35),
                            GBP = FALSE, col = SGblue){

  if (!GBP) {
    df$text <- percent2(df$value)
  }

  ggplot(data = df,
         aes(x = years,
             y = value,
             group = key,
             label = text)) +

    geom_line(data = df,
              aes(x = years,
                  y = value),
              size = 1.2,
              lineend = "round",
              show.legend = FALSE,
              colour = col) +

    geom_point(data = arrange(df, years) %>% tail(1L),
               size = 3,
               show.legend = FALSE,
               colour = col) +

    geom_point(data = arrange(df, years) %>% head(1L),
               size = 3,
               show.legend = FALSE,
               colour = col) +

    geom_text(data = head(df, 1L),
              show.legend = FALSE,
              hjust = 1,
              nudge_x = -1.5,
              size = 6,
              colour = col) +

    geom_text(data = tail(df, 1L),
              show.legend = FALSE,
              hjust = 0,
              nudge_x = 1.5,
              size = 6,
              colour = col) +

    scale_y_continuous(limits = yrange) +

    scale_x_discrete(drop = FALSE,
                     breaks = c("1994-97", "2016-19"),
                     expand = c(0.4, 0.45)) +

    theme(axis.text = element_text(size = 16))

}

barchart <- function(df) {

  ggplot(data = df,
         aes(x = key,
             y = value,
             group = key,
             fill = key,
             label = percent2(value))) +

    geom_bar_interactive(aes(tooltip = str_c(key, ": ", percent2(value)),
                             data_id = paste(key, value)),
                         show.legend = FALSE,
                         stat = "identity") +

    geom_text(colour = "white",
              fontface = "bold",
              nudge_y = -0.04,
              show.legend = FALSE) +

    scale_fill_manual(values = SGmix) +
    scale_colour_manual(values = SGmix) +

    scale_y_continuous(limits = c(0, 0.6)) +

    coord_flip() +

    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(hjust = 1)) +

    addsource()
}

persistentchart <- function(df) {

  ggplot(data = df,
         mapping = aes(x = key,
                       y = value,
                       fill = period,
                       group = period,
                       label = percent2(value))) +

    geom_bar_interactive(aes(tooltip = str_c(percent2(value), " (",
                                             period, ")"),
                             data_id = paste(key, period, value)),
                         stat = "identity",
                         position = "dodge",
                         colour = "white") +

    scale_fill_manual(values = rev(SGblues)) +
    scale_y_continuous(limits = c(0, 0.23)) +
    scale_alpha_manual(values = c("0" = 0, "1" = 1)) +

    geom_text(aes(alpha = ifelse(period == max(period), "1", "0")),
              vjust = -0.5,
              position = position_dodge(1),
              colour = SGblues[1],
              show.legend = FALSE) +

    labs(caption = "Source: Understanding Society Survey")
}

# Add annotations for recession and welfare reform
addrecessionbar <- function(recession = TRUE){

  # positions of bars
  rec_start <- 16.84
  rec_end <- 15.34
  rec_centre <- 15

  rec_rect <- annotate("rect",
                       fill = SGgreys[4],
                       xmin = rec_start,
                       xmax = rec_end,
                       ymin = -Inf,
                       ymax = Inf)

  rec_text <- annotate("text",
                       label = "Recession",
                       size = 3,
                       colour = SGgreys[2],
                       hjust = 1,
                       vjust = 2,
                       x = rec_centre,
                       y = Inf)

  if (recession) {list(rec_rect, rec_text)}

}

disabilitybreaks <- function() {

  line1 <- geom_vline(aes(xintercept = 6.5),
             colour = SGgreys[4],
             alpha = 0.9)

  line2 <- geom_vline(aes(xintercept = 8.5),
               colour = SGgreys[4],
               alpha = 0.9)

  line3 <- geom_vline(aes(xintercept = 16.5),
               colour = SGgreys[4],
               alpha = 0.9)

  text <- annotate("text",
             x = 16.3,
             y = 0.4,
             hjust = 1,
             vjust = 1,
             size = 3,
             colour = SGgreys[2],
             label = "Methodology changes")

  list(line1, line2, line3, text)
}

addsource <- function(){
  labs(caption = "Source: Family Resources Survey")
}

addlabels <- function(df = data, GBP = FALSE, size = 4){

if (!GBP) {
  df$labeltext <- percent2(df$value)
} else {
  df$labeltext <- df$text
}

  left <- geom_text_repel(data = filter(df,
                                      years == min(years)),
                           aes(x = years,
                               label = labeltext),
                           direction = "y",
                           nudge_x = -1,
                           hjust = 1,
                           size = size,
                           show.legend = FALSE,
                           min.segment.length = 10)

  right <- geom_text_repel(data = filter(df,
                                      years == max(years)),
                           aes(x = years,
                               label = labeltext),
                           direction = "y",
                           nudge_x = 1,
                           hjust = 0,
                           size = size,
                           show.legend = FALSE,
                           min.segment.length = 10)
  list(left, right)
}

addnames <- function(df = data, up = 0) {

  geom_text(data = filter(df, min(df$years) == as.character(df$years)),
                  mapping = aes(x = years, y = value + up, label = key),
                  show.legend = FALSE,
                  hjust = 0)
}

addxlabels <- function(){

  periods <- labels[["years"]]$periods

  scale_x_discrete(drop = FALSE,
                   limits = levels(periods),
                   breaks = c("1994-97", "", "", "",
                              "", "", "2000-03", "",
                              "", "", "", "",
                              "2006-09", "", "", "",
                              "", "", "2012-15", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1))
}

addscales <- function(){

  list(scale_color_manual(values = SGmix),
       scale_size_manual(values = c(1.2, 1)))
}


saveplot <- function(file){

  ggsave(file, width = 8.3, height = 5.5, units = "cm", dpi = 500)
}

addinterimtarget <- function(target){

  a <- geom_point(aes(x = "2324",
                      y = target),
                  shape = 21,
                  size = 5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = "white")

  b <- geom_point(aes(x = "2324",
                      y = target),
                  shape = 21,
                  size = 2.5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = SGoranges[2])

  c <- geom_text(data = tail(data, 1L),
                 aes(x = 30.2, y = target + 0.05,
                       label = percent2(target)),
                 colour = SGgreys[1],
                 size = 5)

  d <- geom_point_interactive(aes(x = "2324",
                                  y = target,
                                  tooltip = str_c("Interim target (2023/24): ",
                                                  percent2(target)),
                                  data_id = target),
                              size = 8,
                              colour = "white",
                              alpha = 0.01,
                              show.legend = FALSE)

  list(a, b, c, d)
}

addfinaltarget <- function(target){

  a <- geom_point(aes(x = "3031", y = target),
                  shape = 21,
                  size = 5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = "white")

  b <- geom_point(aes(x = "3031", y = target),
                  shape = 21,
                  size = 2.5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = SGoranges[2])

  c <- geom_text(data = tail(data, 1L),
                   aes(x = 37.2, y = target + 0.05,
                       label = percent2(target)),
                   colour = SGgreys[1],
                 size = 5)

  d <- geom_point_interactive(aes(x = "3031",
                                  y = target,
                                  tooltip = str_c("Final target (2030/31): ",
                                                  percent2(target)),
                                  data_id = target),
                              size = 8,
                              colour = "white",
                              alpha = 0.01,
                              show.legend = FALSE)
  list(a, b, c , d)
}

addtargetbars <- function(){

  a <- geom_vline(aes(xintercept = 24.36),
                  colour = SGgreys[4],
                  alpha = 0.9)

  b <-  annotate("rect",
                 xmin = 29, xmax = 31,
                 ymin = -Inf, ymax = Inf,
                 fill = SGgreys[4])

  c <- annotate("rect",
                xmin = 36, xmax = 38,
                ymin = -Inf, ymax = Inf,
                fill = SGgreys[4])

  list(a, b, c)
}

addyaxis <- function(){

  scale_y_continuous(limits = c(0, 0.53), labels = percent_format(1))

}

adddatalabels <- function(){

  a <- geom_text(data = head(data, 1L),
                 aes(x = year, y = single + 0.04,
                     label = percent2(single)),
                 size = 3,
                 nudge_x = -0.4,
                 colour = SGgreys[1])

  b <-  geom_text(data = tail(data, 1L),
                  aes(x = year, y = single + 0.04,
                      label = percent2(single)),
                  size = 3,
                  hjust = 0,
                  colour = SGgreys[1])

  list(a, b)
}
