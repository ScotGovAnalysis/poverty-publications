
library(tidyverse)
library(openxlsx)
library(haven)
library(stringr)
library(Hmisc)
library(scales)

# Data analysis ----------------------------------------------------------------

getpovby <- function(df,
                     pov = "low60ahc",
                     weight = "gs_newpp",
                     by = NULL) {

  df$povvar <- df[[pov]]
  df$weightvar <- df[[weight]]

  total <- df %>%
    group_by(yearn) %>%
    mutate(population = sum(weightvar),
           sample = sum(weightvar > 0, na.rm = TRUE)) %>%
    group_by(yearn, povvar) %>%
    summarise(number = sum(weightvar),
              population = max(population),
              sample = max(sample),
              povsample = sum(weightvar > 0, na.rm = TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(rate = number / population,
           composition = number / sum(number),
           groupingvar = "All") %>%
    ungroup() %>%
    select(yearn, groupingvar, number, rate, composition,
           sample, povsample)

  if (!is.null(by)) {
    df$groupingvar <- df[[by]]

    grouped <- df %>%
      group_by(yearn, groupingvar) %>%
      mutate(population = sum(weightvar),
             sample = sum(weightvar > 0, na.rm = TRUE)) %>%
      group_by(yearn, povvar, groupingvar) %>%
      summarise(number = sum(weightvar),
                population = max(population),
                sample = max(sample),
                povsample = sum(weightvar > 0, na.rm = TRUE)) %>%
      filter(povvar == 1) %>%
      mutate(rate = number / population,
             composition = number / sum(number)) %>%
      ungroup() %>%
      select(yearn, groupingvar, number, rate, composition,
             sample, povsample)

    rbind(total, grouped) %>%
      arrange(yearn, groupingvar) %>%
      mutate(weight = weight,
             type = pov,
             groups = by)
  } else {
    total  %>%
      arrange(yearn) %>%
      select(-groupingvar) %>%
      mutate(weight = weight,
             type = pov)
  }
}

getheadlines <- function(df, pov = "low60ahc", threeyr = FALSE) {

  pp <- getpovby(df = df, pov = pov)
  ch <- getpovby(df = df, pov = pov, weight = "gs_newch")
  wa <- getpovby(df = df, pov = pov, weight = "gs_newwa")
  pn <- getpovby(df = df, pov = pov, weight = "gs_newpn")

  if (threeyr) {
    pp <-  get3yrtable(pp) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    ch <-  get3yrtable(ch) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    wa <-  get3yrtable(wa) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    pn <-  get3yrtable(pn) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
  } else {
    pp <- mutate(pp, year = factor(yearn,
                                   levels = labels$years$numbered,
                                   labels = labels$years$formatted))
    ch <- mutate(ch, year = factor(yearn,
                                   levels = labels$years$numbered,
                                   labels = labels$years$formatted))
    wa <- mutate(wa, year = factor(yearn,
                                   levels = labels$years$numbered,
                                   labels = labels$years$formatted))
    pn <- mutate(pn, year = factor(yearn,
                                   levels = labels$years$numbered,
                                   labels = labels$years$formatted))
  }

  pp$composition <- 1
  ch$composition <- ch$number / pp$number
  wa$composition <- wa$number / pp$number
  pn$composition <- pn$number / pp$number

  pp <- roundall(pp)
  ch <- roundall(ch)
  wa <- roundall(wa)
  pn <- roundall(pn)

  rate_pp <- select(pp, year, rate) %>% spread(year, rate) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  rate_ch <- select(ch, year, rate) %>% spread(year, rate) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  rate_wa <- select(wa, year, rate) %>% spread(year, rate) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  rate_pn <- select(pn, year, rate) %>% spread(year, rate)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  rates <- rbind(rate_pp, rate_ch, rate_wa, rate_pn)

  comp_pp <- select(pp, year, composition) %>%
    spread(year, composition) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  comp_ch <- select(ch, year, composition) %>%
    spread(year, composition) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  comp_wa <- select(wa, year, composition) %>%
    spread(year, composition) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  comp_pn <- select(pn, year, composition) %>%
    spread(year, composition)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  comps <- rbind(comp_pp, comp_ch, comp_wa, comp_pn)

  number_pp <- select(pp, year, number) %>%
    spread(year, number) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  number_ch <- select(ch, year, number) %>%
    spread(year, number) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  number_wa <- select(wa, year, number) %>%
    spread(year, number) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  number_pn <- select(pn, year, number) %>%
    spread(year, number) %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  numbers <- rbind(number_pp, number_ch, number_wa, number_pn)

  sample_pp <- select(pp, year, sample) %>%
    spread(year, sample) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  sample_ch <- select(ch, year, sample) %>%
    spread(year, sample) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  sample_wa <- select(wa, year, sample) %>%
    spread(year, sample)  %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  sample_pn <- select(pn, year, sample) %>%
    spread(year, sample)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  sample <- rbind(sample_pp, sample_ch, sample_wa, sample_pn)
  list(rates = rates, comps = comps, numbers = numbers, sample = sample)
}

getpriorityrate <- function(df, pov, by, class) {
  getpovby(df, pov = pov, by = by, weight = "gs_newch") %>%
    group_by(groupingvar) %>%
    get3yrtable() %>%
    filter(yearn == max(yearn),
           groupingvar == class) %>%
    samplesizecheck() %>%
    roundall() %>%
    select(groupingvar, rate, type)
}

getfoodsec <- function(househol) {

  househol %>%
    filter(hhstat == 1) %>%
    mutate(foodsec_score = ifelse(is.na(foodq1), NA, 0),
           foodsec_score = case_when(foodq1 %in% c(1, 2) ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq2 %in% c(1, 2) ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq3 %in% c(1, 2) ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq4a == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq4b >= 3 | foodq4c == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq5 == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq6 == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq7 == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq8a == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec_score = case_when(foodq8b >= 3 | foodq8c == 1 ~ foodsec_score + 1,
                                     TRUE ~ foodsec_score),
           foodsec = case_when(foodsec_score == 0 ~ 1,
                               foodsec_score %in% c(1, 2) ~ 2,
                               foodsec_score %in% c(3, 4, 5) ~ 3,
                               foodsec_score >= 6 ~ 4)) %>%
    select(year, sernum, foodsec_score, foodsec)
}


getmediansbhc <- function(df){

  df %>%
    summarise(pp = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef, probs = 0.5,
                                weights = gs_newpp),
              ch = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef, probs = 0.5,
                                weights = gs_newch),
              wa = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef, probs = 0.5,
                                weights = gs_newwa),
              pn = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef, probs = 0.5,
                                weights = gs_newpn),
              pp_sample = n(),
              ch_sample = sum(gs_newch > 0, na.rm = TRUE),
              wa_sample = sum(gs_newwa > 0, na.rm = TRUE),
              pn_sample = sum(gs_newpn > 0, na.rm = TRUE ))

}

getmediansahc <- function(df){

  df %>%
    summarise(pp = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef, probs = 0.5,
                                weights = gs_newpp),
              ch = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef, probs = 0.5,
                                weights = gs_newch),
              wa = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef, probs = 0.5,
                                weights = gs_newwa),
              pn = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef, probs = 0.5,
                                weights = gs_newpn),
              pp_sample = n(),
              ch_sample = sum(gs_newch > 0, na.rm = TRUE),
              wa_sample = sum(gs_newwa > 0, na.rm = TRUE),
              pn_sample = sum(gs_newpn > 0, na.rm = TRUE ))

}

getdecptsbhc <- function(df){

  df %>%
    summarise("1" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.1, weights = gs_newpp),
              "2" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.2, weights = gs_newpp),
              "3" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.3, weights = gs_newpp),
              "4" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.4, weights = gs_newpp),
              "5" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.5, weights = gs_newpp),
              "6" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.6, weights = gs_newpp),
              "7" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.7, weights = gs_newpp),
              "8" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.8, weights = gs_newpp),
              "9" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                   probs = 0.9, weights = gs_newpp),
              "10" = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                 probs = 1, weights = gs_newpp))

}

getdecptsahc <- function(df){

  df %>%
    summarise("1" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.1, weights = gs_newpp),
              "2" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.2, weights = gs_newpp),
              "3" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.3, weights = gs_newpp),
              "4" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.4, weights = gs_newpp),
              "5" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.5, weights = gs_newpp),
              "6" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.6, weights = gs_newpp),
              "7" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.7, weights = gs_newpp),
              "8" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.8, weights = gs_newpp),
              "9" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                   probs = 0.9, weights = gs_newpp),
              "10" = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                 probs = 1, weights = gs_newpp))

}

getdecsharesbhc <- function(df){

  df %>%
    group_by(yearn) %>%
    mutate(s_oe_bhc = round2(s_oe_bhc, 2),
           dec1 = wtd.quantile(s_oe_bhc, probs = 0.1, weights = gs_newpp),
           dec2 = wtd.quantile(s_oe_bhc, probs = 0.2, weights = gs_newpp),
           dec3 = wtd.quantile(s_oe_bhc, probs = 0.3, weights = gs_newpp),
           dec4 = wtd.quantile(s_oe_bhc, probs = 0.4, weights = gs_newpp),
           dec5 = wtd.quantile(s_oe_bhc, probs = 0.5, weights = gs_newpp),
           dec6 = wtd.quantile(s_oe_bhc, probs = 0.6, weights = gs_newpp),
           dec7 = wtd.quantile(s_oe_bhc, probs = 0.7, weights = gs_newpp),
           dec8 = wtd.quantile(s_oe_bhc, probs = 0.8, weights = gs_newpp),
           dec9 = wtd.quantile(s_oe_bhc, probs = 0.9, weights = gs_newpp),
           Decile = case_when(s_oe_bhc <= dec1 ~ 1,
                              s_oe_bhc <= dec2 ~ 2,
                              s_oe_bhc <= dec3 ~ 3,
                              s_oe_bhc <= dec4 ~ 4,
                              s_oe_bhc <= dec5 ~ 5,
                              s_oe_bhc <= dec6 ~ 6,
                              s_oe_bhc <= dec7 ~ 7,
                              s_oe_bhc <= dec8 ~ 8,
                              s_oe_bhc <= dec9 ~ 9,
                              s_oe_bhc >  dec9 ~ 10)) %>%
    group_by(yearn, Decile) %>%
    summarise(share = sum(s_oe_bhc * gs_newpp) * bhcpubdef[1] / bhcyrdef[1] * 365/7)
}

getdecsharesahc <- function(df){

  df %>%
    group_by(yearn) %>%
    mutate(s_oe_ahc = round2(s_oe_ahc, 2),
         dec1 = wtd.quantile(s_oe_ahc, probs = 0.1, weights = gs_newpp),
         dec2 = wtd.quantile(s_oe_ahc, probs = 0.2, weights = gs_newpp),
         dec3 = wtd.quantile(s_oe_ahc, probs = 0.3, weights = gs_newpp),
         dec4 = wtd.quantile(s_oe_ahc, probs = 0.4, weights = gs_newpp),
         dec5 = wtd.quantile(s_oe_ahc, probs = 0.5, weights = gs_newpp),
         dec6 = wtd.quantile(s_oe_ahc, probs = 0.6, weights = gs_newpp),
         dec7 = wtd.quantile(s_oe_ahc, probs = 0.7, weights = gs_newpp),
         dec8 = wtd.quantile(s_oe_ahc, probs = 0.8, weights = gs_newpp),
         dec9 = wtd.quantile(s_oe_ahc, probs = 0.9, weights = gs_newpp),
         Decile = case_when(s_oe_ahc <= dec1 ~ 1,
                            s_oe_ahc <= dec2 ~ 2,
                            s_oe_ahc <= dec3 ~ 3,
                            s_oe_ahc <= dec4 ~ 4,
                            s_oe_ahc <= dec5 ~ 5,
                            s_oe_ahc <= dec6 ~ 6,
                            s_oe_ahc <= dec7 ~ 7,
                            s_oe_ahc <= dec8 ~ 8,
                            s_oe_ahc <= dec9 ~ 9,
                            s_oe_ahc >  dec9 ~ 10)) %>%
    group_by(yearn, Decile) %>%
    summarise(share = sum(s_oe_ahc * gs_newpp) * ahcpubdef[1] / ahcyrdef[1] * 365/7)
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

getpovertythresholdsbhc <- function(df){

  decs <- getdecptsbhc(df) %>%
    select(-"10")

  df1 <- df %>%
    summarise(UKmedian = max(mdoebhc  * bhcpubdef / bhcyrdef),
              Scotmedian = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.5, weights = gs_newpp),
              relpovbhc_threshold = 0.6 * UKmedian,
              abspovbhc_threshold = 0.6 * max(abs1011bhc)) %>%
    cbind(decs)

  df <- as.data.frame(t(df1)) %>%
    rownames_to_column(var = "Measure") %>%
    rename(weekly2 = V1) %>%
    mutate(annual2 = weekly2 * 365/7,
           weekly1 = weekly2 * 0.67,
           annual1 = weekly1 * 365/7,
           weekly3 = weekly2 * 1.2,
           annual3 = weekly3 * 365/7,
           weekly4 = weekly2 * 1.53,
           annual4 = weekly4 * 365/7) %>%
    select(Measure, weekly1, weekly2, weekly3, weekly4, annual1, annual2, annual3,
           annual4)

  df$Measure <- decode(df$Measure,
                       search = c("UKmedian", "Scotmedian",
                                  "relpovbhc_threshold", "abspovbhc_threshold",
                                  "1", "2", "3", "4", "5", "6", "7", "8", "9"),
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

  decs <- getdecptsahc(df) %>%
    select(-"10")

  df1 <- df %>%
    summarise(UKmedian = max(mdoeahc * ahcpubdef / ahcyrdef),
              Scotmedian = wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.5, weights = gs_newpp),
              relpovahc_threshold = 0.6 * UKmedian,
              abspovahc_threshold = 0.6 * max(abs1011ahc)) %>%
    cbind(decs)

  df <- as.data.frame(t(df1)) %>%
    rownames_to_column(var = "Measure") %>%
    rename(weekly2 = V1) %>%
    mutate(annual2 = weekly2 * 365/7,
           weekly1 = weekly2 * 0.58,
           annual1 = weekly1 * 365/7,
           weekly3 = weekly2 * 1.2,
           annual3 = weekly3 * 365/7,
           weekly4 = weekly2 * 1.62,
           annual4 = weekly4 * 365/7) %>%
    select(Measure, weekly1, weekly2, weekly3, weekly4, annual1, annual2, annual3,
           annual4)

  df$Measure <- decode(df$Measure,
                       search = c("UKmedian", "Scotmedian",
                                  "relpovahc_threshold", "abspovahc_threshold",
                                  "1", "2", "3", "4", "5", "6", "7", "8", "9"),
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
    mutate(earns = case_when(esgjobhh + esgrsehh < 0 ~ 0,
                             TRUE ~(esgjobhh + esgrsehh) * bhcdef / eqobhchh),
           bens = (esbenihh + chbenhh) * bhcdef / eqobhchh,
           pen = esgocchh * bhcdef / eqobhchh,
           inv = case_when(esginvhh < 0 ~ 0,
                           TRUE ~ esginvhh * bhcdef / eqobhchh),
           misc = (esmischh + espribhh + inchilhh) * bhcdef / eqobhchh,
           total = esginchh * bhcdef / eqobhchh) %>%
    select(gvtregn, s_oe_bhc, bhcpubdef, bhcyrdef, gs_newpp, earns, bens, pen,
           inv, misc, total)

  decs <- as.numeric(getdecptsbhc(df))

  data <- data %>%
    mutate(Decile = case_when(s_oe_bhc * bhcpubdef / bhcyrdef <= decs[1] ~ 1,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[2] ~ 2,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[3] ~ 3,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[4] ~ 4,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[5] ~ 5,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[6] ~ 6,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[7] ~ 7,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[8] ~ 8,
                              s_oe_bhc * bhcpubdef / bhcyrdef <= decs[9] ~ 9,
                              s_oe_bhc * bhcpubdef / bhcyrdef >  decs[9] ~ 10))

  bydec <- data  %>%
    group_by(Decile) %>%
    summarise(Earnings = sum(earns * gs_newpp) / sum(total * gs_newpp),
              "Social Security payments" = sum(bens * gs_newpp) / sum(total * gs_newpp),
              "Occupational pensions" = sum(pen * gs_newpp) / sum(total * gs_newpp),
              "Investments" = sum(inv * gs_newpp) / sum(total * gs_newpp),
              "Other sources" = sum(misc * gs_newpp) / sum(total * gs_newpp)) %>%
    ungroup() %>%
    mutate(Decile = factor(Decile))

  tot <- data %>%
    summarise(Earnings = sum(earns * gs_newpp) / sum(total * gs_newpp),
              "Social Security payments" = sum(bens * gs_newpp) / sum(total * gs_newpp),
              "Occupational pensions" = sum(pen * gs_newpp) / sum(total * gs_newpp),
              "Investments" = sum(inv * gs_newpp) / sum(total * gs_newpp),
              "Other sources" = sum(misc * gs_newpp) / sum(total * gs_newpp)) %>%
    mutate(Decile = "All")

  rbind(bydec, tot)
}

splitntranspose <- function(df, measure){

  df$measure <- df[["measure"]]

  df %>%
    select(year, groupingvar, measure) %>%
    rename(Group = groupingvar) %>%
    spread(year, measure)

}

getpersistentpoverty <- function(df) {

  names(df)[1] <- "nation"

  df %>%
    filter_at(1, all_vars(. %in% c("Total", "England", "Scotland", "Wales",
                                   "Northern Ireland"))) %>%
    gather(period, value, -nation)
}

get3yrtable <- function(df) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              get3yraverage) %>%
    mutate_at(vars(contains("sample")), get3yrtotal) %>%
    filter(!is.na(sample)) %>%
    ungroup()
}

get5yrtable <- function(df) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              get5yraverage) %>%
    mutate_at(vars(contains("sample")), get5yrtotal) %>%
    filter(!is.na(sample)) %>%
    ungroup()
}

samplesizecheck <- function(df) {
  df %>%
    mutate(number = ifelse(povsample >= 100, number, NA),
           rate = ifelse(sample >= 100, rate, NA))
}

# Utils ------------------------------------------------------------------------

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

round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

roundpop <- function(x) {
  ifelse(!is.na(x), round2(x, -4), NA)
}

roundpct <- function(x) {
  ifelse(!is.na(x), round2(x, 4), NA)
}

roundall <- function(df) {
  df %>%
    mutate(number = roundpop(number),
           rate = roundpct(rate),
           composition = roundpct(composition))
}

fmtpop <- function(x) {

  require(scales)

  ifelse(!is.na(x), comma(x), NA)
}

fmtpct <- function(x) {

  require(scales)

  ifelse(!is.na(x), percent2(x), NA)
}

# Change scales::percent() and scales::comma() functions
# to ensure correct rounding

percent2 <- function(x) {paste0(round2(x, 2) * 100, "%")}

comma2 <- function(x, accuracy = 1, scale = 1, prefix = "") {

  y <- round2(x, accuracy)
  scales::comma(x = y, accuracy = accuracy, scale = scale, prefix = prefix)
}

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

# Spreadsheet functions --------------------------------------------------------

getSheetTitles <- function(filename = filename, sheetname){

  wb <- loadWorkbook(filename)
  read.xlsx(wb, sheet = sheetname, startRow = 3, colNames = FALSE,
                   cols = 2, rows = 3) %>% pull()
}

insertValues <- function(vector, location) {
  for (i in 0:(length(location) - 1)) {
    vector <- append(vector, c(" ", " "), after = (location[i + 1] + i))
  }
  vector
}

getHeadings <- function(sheetlinks, headings) {
  b <- rep(FALSE, length(sheetlinks))
  for (i in seq_along(sheetlinks)) {
    if (sheetlinks[i] == " " & sheetlinks[i + 1] == " ") {
      b[i + 1] <- TRUE
    }
  }
  b[which(b)] <- headings
  b[which(b == "FALSE")] <- " "
  b
}

# firstup <- function(x) {
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }

shortenTitle <- function(title) {
  if (endsWith(title, ")")) {
    str_split(title, " \\(")[[1]][1]
  } else {title}
}

createWideSpreadsheet <- function(data) {

  file <- data[["file"]]
  mysheet <- data[["sheet"]]
  sheettitle <- data[["sheettitle"]]
  dfs <- data[["dfs"]]
  formats <- data[["formats"]]
  titles <- data[["titles"]]
  subtitles <- data[["subtitles"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]
  totalrow <- data[["totalrow"]]

  if (length(dfs) == length(formats) &
      length(dfs) == length(titles) &
      length(dfs) == length(subtitles)) {} else {
        warning("Number of datasets and titles don't match!")
      }

  # Open / create workbook
  if (file.exists(file)) {
    wb <- loadWorkbook(file)
    if (mysheet %in% getSheetNames(file)) {
      removeWorksheet(wb = wb, sheet = mysheet)
    }
  } else {
    wb <- createWorkbook()
  }

  addWorksheet(wb = wb, sheetName = mysheet, gridLines = FALSE)

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
  GBPStyle <- createStyle(numFmt = "#,##0", halign = "right")
  GBPStyle_underline <- createStyle(numFmt = "#,##0", halign = "right",
                                    border = "bottom")
  sourceStyle <- createStyle(fontName = "Segoe UI", fontSize = 10,
                             wrapText = FALSE, halign = "left")
  footnoteHeaderStyle <- createStyle(fontName = "Segoe UI Semibold",
                                     fontSize = 11, textDecoration = "BOLD",
                                     wrapText = FALSE, halign = "left")
  footnoteStyle <- createStyle(fontName = "Segoe UI", fontSize = 11,
                               wrapText = FALSE, halign = "left")
  firstRow <- 5
  firstCol <- 2
  titlerows <- 2
  sourcerows <- 1

  # Sheet title
  writeData(wb, mysheet, x = sheettitle, startCol = firstCol, startRow = 3)
  addStyle(wb, mysheet, style = titleStyle, rows = 3, cols = firstCol)

  # loop through all tables in this sheet to write tables and add styles
  for (i in seq_along(dfs)) {

    datarows <- dim(dfs[[i]])[1] + 1
    datacols <- dim(dfs[[i]])[2]
    tablerows <- titlerows + datarows + sourcerows

    # write table
    writeData(wb, mysheet, x = titles[i], startCol = firstCol,
              startRow = firstRow)
    writeData(wb, mysheet, x = subtitles[i], startCol = firstCol,
              startRow = firstRow + 1)
    writeData(wb, mysheet, x = dfs[[i]], startCol = firstCol,
              startRow = firstRow + 2, keepNA = TRUE, na.string = "..",
              headerStyle = headerStyle)
    writeData(wb, mysheet, x = source, startCol = firstCol,
              startRow = firstRow + tablerows - 1)

    # add styles
    if (formats[i] %in% c("pct", "percent", "rate", "comp", "composition")) {
      if (is.null(totalrow)) {
        format_top <- pctStyle
      } else {
        format_top <- pctStyle_underline
      }
      format_main <- pctStyle
      format_bottom <- pctStyle_underline
    } else if (formats[i] %in% c("pop", "number", "population", "num")) {
      if (is.null(totalrow)) {
        format_top <- popStyle
      } else {
        format_top <- popStyle_underline
      }
      format_main <- popStyle
      format_bottom <- popStyle_underline
    } else if (formats[i] %in% c("pounds", "pound", "GBP", "gbp")) {
      if (is.null(totalrow)) {
        format_top <- GBPStyle
      } else {
        format_top <- GBPStyle_underline
      }
      format_main <- GBPStyle
      format_bottom <- GBPStyle_underline
    }

    # add styles
    addStyle(wb, mysheet, style = titleStyle, rows = firstRow, cols = firstCol)
    addStyle(wb, mysheet, style = subtitleStyle, rows = firstRow + 1,
             cols = firstCol)
    addStyle(wb, mysheet, style = format_top, rows = firstRow + 3,
             cols = firstCol:(datacols + 1), stack = TRUE)
    addStyle(wb, mysheet, style = format_main, cols = firstCol:(datacols + 1),
             rows = (firstRow + 4):(firstRow + datarows + 1), gridExpand = TRUE,
             stack = TRUE)
    addStyle(wb, mysheet, style = format_bottom, rows = firstRow + datarows + 1,
             cols = firstCol:(datacols + 1))
    addStyle(wb, mysheet, style = sourceStyle, rows = firstRow + tablerows - 1,
             cols = firstCol)

    firstRow = firstRow + tablerows + 2
  }

  # add footnotes
  if (is.vector(footnotes)) {
    writeData(wb, mysheet, x = "Notes", startCol = firstCol, startRow = firstRow)
    writeData(wb, mysheet, x = footnotes, startCol = firstCol,
              startRow = firstRow + 1)
    addStyle(wb, mysheet, style = footnoteHeaderStyle,
             rows = firstRow, cols = firstCol)
    addStyle(wb, mysheet, style = footnoteStyle, cols = firstCol,
             rows = (firstRow + 1):(firstRow + 1 + length(footnotes)))
  }

  setColWidths(wb, mysheet, cols = firstCol, widths = 40)
  saveWorkbook(wb, file, overwrite = TRUE)
}

createContentSheet <- function(filename, headings){

  wb <- loadWorkbook(filename)

  # get sheet names and titles
  sheets <- names(wb)
  sheets <- sheets[!sheets == "Contents"]
  titles <- sapply(sheets, getSheetTitles, filename = filename)
  titles <- titles[!titles == "Tables"]

  # remove content in () from titles
  titles <- sapply(titles, shortenTitle)

  # create hyperlinks to sheets
  sheetlinks <- sapply(seq_along(sheets),
                       function(i) makeHyperlinkString(sheets[[i]],
                                                       text = titles[[i]],
                                                       col = 1))

  # insert spaces for headers
  sheetlinks <- insertValues(sheetlinks, headings$location)

  # create new worksheet
  if ("Contents" %in% getSheetNames(filename)) {removeWorksheet(wb, "Contents")}
  addWorksheet(wb, "Contents", gridLines = FALSE)

  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 12,
                            wrapText = FALSE, halign = "left")
  noteStyle <- createStyle(fontName = "Segoe UI", fontSize = 12, halign = "left")
  tocStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                          fontColour = "blue", textDecoration = "underline",
                          halign = "left")
  backButtonStyle <- createStyle(fontName = "Segoe UI", fontSize = 10,
                                 fontColour = "blue",
                                 textDecoration = c("underline", "bold"),
                                 halign = "left", valign = "center")

  # 1st column - title
  writeData(wb, "Contents", "Tables", startRow = 2, startCol = 2)
  addStyle(wb, "Contents", rows = 2, cols = 2, style = titleStyle)

  # 1st column - note
  note1 <- c("This workbook contains data used in the charts and tables in the Poverty and Income",
             "Inequality in Scotland report, published on 25 March 2021, and additional analysis.")
  note2 <- "https://data.gov.scot/poverty"
  names(note2) <- "The report is available here: data.gov.scot/poverty"
  class(note2) <- "hyperlink"

  writeData(wb, "Contents", note1, startRow = 3, startCol = 2)
  writeData(wb, "Contents", note2, startRow = 5, startCol = 2)
  addStyle(wb, "Contents", rows = 3:5, cols = 2, style = noteStyle)

  # 1st column - headers
  writeData(wb, "Contents", getHeadings(sheetlinks, headings$titles),
            startRow = 6, startCol = 2)
  addStyle(wb, "Contents", rows = 6:(length(sheetlinks) + 6), cols = 2,
            style = headerStyle)

  # 1st column - list of sheets
  writeFormula(wb, "Contents", startRow = 6, startCol = 3, x = sheetlinks)
  addStyle(wb, "Contents", rows = 6:(length(sheetlinks) + 6), cols = 3,
           style = tocStyle)

  # 2nd column - contact information
  email <- "mailto:social-justice-analysis@gov.scot"
  names(email) <- "social-justice-analysis@gov.scot"
  class(email) <- "hyperlink"

  writeData(wb, "Contents", "Contact", startRow = 2,
            startCol = 4)
  addStyle(wb, "Contents", rows = 2, cols = 4, style = titleStyle)

  writeData(wb, "Contents", "Maike Waldmann", startRow = 3, startCol = 4)
  writeData(wb, "Contents", "Scottish Government", startRow = 4, startCol = 4)
  writeData(wb, "Contents", "Communities Analysis Division", startRow = 5,
            startCol = 4)
  writeData(wb, "Contents", email, startRow = 6, startCol = 4)
  addStyle(wb, "Contents", rows = 3:6, cols = 4, style = noteStyle)

  # 2nd column - note on sample sizes
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
            startCol = 4)
  addStyle(wb, "Contents", rows = 8, cols = 4, style = titleStyle)

  writeData(wb, "Contents", text, startRow = 9, startCol = 4, colNames = FALSE)
  addStyle(wb, "Contents", rows = 9:19, cols = 4, style = noteStyle)

  # 2nd column - note on housing costs
  text_hc <- c("The most commonly used poverty indicator in Scotland is relative",
               "poverty after housing costs.",
               "After-housing-costs measures describe disposable household income",
               "after housing costs are paid for. They therefore better describe",
               "what is available for food, bills and leisure every week or month.",
               "The poverty characteristics tables are therefore only available",
               "for poverty after housing costs.")

  text_hc <- data.frame(text_hc)

  writeData(wb, "Contents", "Note on poverty and housing costs", startRow = 21,
            startCol = 4)
  addStyle(wb, "Contents", rows = 21, cols = 4, style = titleStyle)

  writeData(wb, "Contents", text_hc, startRow = 22, startCol = 4,
            colNames = FALSE)
  addStyle(wb, "Contents", rows = 22:28, cols = 4, style = noteStyle)

  # 2nd column - note on revision
  text_rev <- c("In 2021, previously published datasets underwent a minor",
                "methodological revision to capture all income from child maintenance.",
                "This led to small changes in household income and small adjustments",
                "to some poverty estimates. Therefore, some poverty and income",
                "estimates that were published in 2021 may not exactly match",
                "previously published estimates for 1994/95 to 2019/20. The revision",
                "did not affect any trends in poverty or household income.")

  text_rev <- data.frame(text_rev)

  writeData(wb, "Contents", "Data revision", startRow = 30,
            startCol = 4)
  addStyle(wb, "Contents", rows = 30, cols = 4, style = titleStyle)

  writeData(wb, "Contents", text_rev, startRow = 31, startCol = 4,
            colNames = FALSE)
  addStyle(wb, "Contents", rows = 31:39, cols = 4, style = noteStyle)


  setColWidths(wb, "Contents", 1, widths = 7)
  setColWidths(wb, "Contents", 2, widths = 3)
  setColWidths(wb, "Contents", 3:4, widths = 85)

  # move contents sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])

  # add back button to each sheet
  for (sheet in sheets) {
    writeFormula(wb, sheet, startRow = 1, startCol = 2,
                 x = makeHyperlinkString("Contents", row = 2, col = 1,
                                         text = "Back to Contents"))
    addStyle(wb, sheet, rows = 1, cols = 2, style = backButtonStyle)
    setRowHeights(wb, sheet, rows = 1, heights = 33)
  }

  saveWorkbook(wb, filename, overwrite = TRUE)

}

mark_missing <- function(data = data, ncols, nrows, xlscol, xlsrow) {
  sheetname <- data[["sheet"]]
  filename <- data[["file"]]
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

# wrap_text <- function(data = data, rows, cols) {
#   filename <- data[["file"]]
#   sheetname <- data[["sheet"]]
#   wb <- loadWorkbook(filename)
#   addStyle(wb, sheetname, rows = rows, cols = cols,
#            style = createStyle(wrapText = TRUE), gridExpand = TRUE,
#            stack = TRUE)
# }

# Chart functions --------------------------------------------------------------

linechart <- function(df, ...){

  ggplot(data = df,
         aes(x = x,
             y = y,
             group = key,
             colour = key,
             linetype = key,
             label = label)) +

    addxlabels() +
    addrecessionbar(...) +

    geom_point_interactive(aes(tooltip = tooltip,
                               data_id = tooltip),
                           show.legend = FALSE,
                           size = 6,
                           colour = "white",
                           alpha = 0.01) +

    geom_line(lineend = "round",
                          show.legend = FALSE) +

    geom_point(data = filter(df, x == min(x)),
               size = 2,
               show.legend = FALSE) +

    geom_point(data = filter(df, x == max(x)),
               size = 2,
               show.legend = FALSE)
}

linechart_small <- function(df, yrange = c(0.1, 0.35), col = SGblue){

  ggplot(data = df,
         aes(x = x,
             y = y,
             group = key,
             label = label)) +

    geom_line(data = df,
              size = 1.2,
              lineend = "round",
              show.legend = FALSE,
              colour = col) +

    geom_point(data = filter(df, x == min(x)),
               size = 3,
               show.legend = FALSE,
               colour = col) +

    geom_point(data = filter(df, x == max(x)),
               size = 3,
               show.legend = FALSE,
               colour = col) +

    geom_text(data = filter(df, x == min(x)),
              show.legend = FALSE,
              hjust = 1,
              nudge_x = -1.5,
              size = 6,
              colour = col) +

    geom_text(data = filter(df, x == max(x)),
              show.legend = FALSE,
              hjust = 0,
              nudge_x = 1.5,
              size = 6,
              colour = col) +

    scale_y_continuous(limits = yrange) +

    scale_x_discrete(drop = FALSE,
                     breaks = c("1994-97", "2017-20"),
                     expand = c(0.4, 0.45)) +

    theme(axis.text = element_text(size = 16))

}

barchart <- function(df, palette = SGmix) {

  ggplot(data = df,
         aes(x = x, y = y, group = x, fill = x, label = label)) +

    geom_bar_interactive(aes(tooltip = tooltip,
                             data_id = tooltip),
                         show.legend = FALSE,
                         stat = "identity") +

    geom_text(colour = "white",
              fontface = "bold",
              nudge_y = -0.04,
              show.legend = FALSE) +

    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +

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

  if (recession) {
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

    list(rec_rect, rec_text)
  }

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

addlabels <- function(df = data, size = 4){

left <-  geom_text_repel(data = filter(df, x == min(x)),
                         mapping = aes(x = x, y = y, label = label),
                         direction = "y",
                         nudge_x = -0.5,
                         hjust = 1,
                         size = size,
                         point.padding = 0.2,
                         box.padding = 0.2,
                         show.legend = FALSE,
                         min.segment.length = 10)

right <-  geom_text_repel(data = filter(df, x == max(x)),
                         mapping = aes(x = x, y = y, label = label),
                         direction = "y",
                         nudge_x = 0.5,
                         hjust = 0,
                         size = size,
                         point.padding = 0.2,
                         box.padding = 0.2,
                         show.legend = FALSE,
                         min.segment.length = 10)

list(left, right)
}

addnames <- function(df = data, up = 0) {

  geom_text(data = filter(df, x == min(x)),
                  mapping = aes(x = x, y = y + up, label = key),
                  show.legend = FALSE,
                  hjust = 0)
}

addxlabels <- function(){

  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "2017-20"),
                   expand = c(0.1, 0.1))
}

addscales <- function(palette = SGmix){

  list(scale_color_manual(values = palette),
       scale_size_manual(values = c(1.2, 1)))
}


saveplot <- function(file){

  ggsave(file, width = 8.3, height = 5.5, units = "cm", dpi = 500)
}

addinterimtarget <- function(target){

  a <- geom_point(aes(x = "2023/24",
                      y = target),
                  shape = 21,
                  size = 5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = "white")

  b <- geom_point(aes(x = "2023/24",
                      y = target),
                  shape = 21,
                  size = 2.5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = SGoranges[2])

  c <- geom_text(data = tail(data, 1L),
                 aes(x = 30.2, y = target - 0.05,
                       label = percent2(target)),
                 colour = SGgreys[1],
                 size = 5)

  d <- geom_point_interactive(aes(x = "2023/24",
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

  a <- geom_point(aes(x = "2030/31", y = target),
                  shape = 21,
                  size = 5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = "white")

  b <- geom_point(aes(x = "2030/31", y = target),
                  shape = 21,
                  size = 2.5,
                  colour = SGgreys[1],
                  stroke = 1,
                  fill = SGoranges[2])

  c <- geom_text(data = tail(data, 1L),
                   aes(x = 37.2, y = target - 0.04,
                       label = percent2(target)),
                   colour = SGgreys[1],
                 size = 5)

  d <- geom_point_interactive(aes(x = "2030/31",
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
