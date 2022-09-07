
library(tidyverse)
library(openxlsx)
library(haven)
library(stringr)
library(Hmisc)
library(scales)
library(htmltools)

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
      group_by(yearn, povvar, groupingvar, .drop = FALSE) %>%
      summarise(number = sum(weightvar),
                population = population[1],
                sample = sample[1],
                povsample = sum(weightvar > 0, na.rm = TRUE)) %>%
      filter(povvar == 1) %>%
      mutate(sample = ifelse(is.na(sample), 0, sample),
             number = ifelse(is.na(number), 0, number),
             population = ifelse(is.na(population), 0, population),
             rate = ifelse(population > 0, number / population, NA),
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

summarise_data <- function(df) {

  df %>%
    group_by(groupingvar) %>%
    get3yrtable() %>%
    samplesizecheck() %>%
    roundall() %>%
    mutate(year = get_periods(yearn))

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
    mutate(foodsec_score = 0,
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

  names(df) <- c("nation", periods)

  df %>%
    filter_at(1, all_vars(. %in% c("Total", "England", "Scotland", "Wales",
                                   "Northern Ireland"))) %>%
    gather(period, value, -nation)
}

get_entry_exits <- function(df) {

  names(df) <- c("nation", periods)

  df %>%
    filter_at(1, all_vars(. %in% c("Total", "England", "Scotland", "Wales",
                                   "Northern Ireland"))) %>%
    pivot_longer(2:(length(periods) + 1), names_to = "period")
}

get3yrtable <- function(df) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              analysistools::getrollingmean, 3) %>%
    mutate_at(vars(contains("sample")), analysistools::getrollingtotal, 3) %>%
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

samplesizecheck <- function(df, suppressed = NA) {
  df %>%
    mutate(number = ifelse(povsample >= 100, number, suppressed),
           rate = ifelse(sample >= 100, rate, suppressed))
}

getlongformat <- function(df, weight = "gs_newpp", pov = "low60ahc", by = NULL,
                          get5yr = FALSE) {

  df <- df %>%
    group_by(yearn) %>%
    getpovby(pov = pov, weight = weight, by = by)

  if (!"groupingvar" %in% colnames(df)) {
    df <- df %>%
      mutate(groupingvar = case_when(weight == "gs_newpp" ~ "People",
                                     weight == "gs_newch" ~ "Children",
                                     weight == "gs_newwa" ~ "Working Age Adults",
                                     weight == "gs_newpn" ~ "Pensioners",
                                     weight == "gs_newad" ~ "Adults"),
             groups = NA)
  }
  if (get5yr) {
    df <- df %>%
      group_by(groupingvar) %>%
      get5yrtable() %>%
      filter(yearn >= 5) %>%
      mutate(DateCode = factor(yearn, levels = labels$years$numbered,
                               labels = labels$years$long5yrperiods))
  } else {
    df <- df %>%
      group_by(groupingvar) %>%
      get3yrtable() %>%
      filter(yearn >= 3) %>%
      mutate(DateCode = factor(yearn, levels = labels$years$numbered,
                               labels = labels$years$longperiods))
  }

  df <- df %>%
    roundall() %>%
    mutate(rate = 100 * rate) %>%
    samplesizecheck("*") %>%
    mutate(GeographyCode = "S92000003",
           HousingCosts = case_when(grepl("ahc", type) ~ "After Housing Costs",
                               grepl("bhc", type) ~ "Before Housing Costs"),
           Ratio = rate,
           Count = number,
           group = case_when(weight == "gs_newpp" ~ "People",
                             weight == "gs_newch" ~ "Children",
                             weight == "childwgt" ~ "Children",
                             weight == "gs_newwa" ~ "Working Age Adults",
                             weight == "gs_newpn" ~ "Pensioners",
                             weight == "gs_newad" ~ "Adults",
                             weight == "adultwgt" ~ "Adults")) %>%
    select(GeographyCode, DateCode, Ratio, Count, HousingCosts, groupingvar, group) %>%
    gather(Measurement, Value, -GeographyCode, -DateCode, -HousingCosts, -groupingvar, -group) %>%
    mutate(Units = case_when(Measurement == "Count" ~ group,
                             Measurement == "Ratio" ~ paste0("Percentage Of ", group))) %>%
    select(GeographyCode, DateCode, Measurement, Units, Value, HousingCosts, groupingvar) %>%
    mutate(Ethnicity = "All",
           Religion  = "All",
           Age = "All",
           FamilyType = "All",
           FamilyEmploymentStatus = "All",
           WorkStatus = "All",
           TypeOfTenure = "All",
           UrbanRuralClassification = "All",
           MaritalStatus = "All",
           SingleParenthood = "All",
           NumberOfChildrenInHousehold = "All",
           HouseholdTypeGender = "All",
           HouseholdDisabilityStatus = "All",
           AgeOfYoungestChild = "All",
           AgeOfMother = "All",
           IndicatorPoverty = pov) %>%
    filter(groupingvar != "(Missing)")

if (weight == "gs_newpn") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar)
} else if (weight == "gs_newwa") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar,
                      FamilyEmploymentStatus, WorkStatus)
} else if ((weight == "gs_newad" & by != "singlehh") | weight == "adultwgt") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar, FamilyType,
                      Age, Religion, MaritalStatus)
} else if (weight == "gs_newad" & by == "singlehh") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar,
                      HouseholdTypeGender)
} else if (weight == "gs_newpp") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar, Ethnicity,
                      TypeOfTenure, UrbanRuralClassification,
                      NumberOfChildrenInHousehold, HouseholdDisabilityStatus)
} else if (weight == "gs_newch" | weight == "childwgt") {
  df <- df %>% select(GeographyCode, DateCode, Measurement, Units, Value,
                      HousingCosts, IndicatorPoverty, groupingvar, Ethnicity,
                      TypeOfTenure, UrbanRuralClassification,
                      NumberOfChildrenInHousehold, HouseholdDisabilityStatus,
                      Age, WorkStatus, SingleParenthood, AgeOfYoungestChild,
                      AgeOfMother, FamilyEmploymentStatus)
}

  df
}

getbreakdowns <- function(measure) {

  all <- list()

  if (!measure %in% c("cmdahc", "cmdbhc")) {

    # Pensioners
    pn <- getlongformat(hbai, weight = "gs_newpn", pov = measure)

    # Adults
    newfambu <- getlongformat(hbai, weight = "gs_newad", pov = measure, by = "newfambu") %>%
      mutate(FamilyType = groupingvar)
    ageband <- getlongformat(adult, weight = "adultwgt", pov = measure, by = "ageband") %>%
      mutate(Age = groupingvar)
    marital <- getlongformat(adult, pov = measure, weight = "adultwgt", by = "marital") %>%
      mutate(MaritalStatus = groupingvar)
    religsc <- getlongformat(adult, pov = measure, weight = "adultwgt",
                             by = "religsc", get5yr = TRUE) %>%
      filter(DateCode == levels(labels$years$long5yrperiods)[22],
             groupingvar != "All") %>%
      mutate(Religion = groupingvar)

    # Working-age adults
    ecobu <- getlongformat(hbai, weight = "gs_newwa", pov = measure, by = "ecobu") %>%
      mutate(FamilyEmploymentStatus = groupingvar)
    workinghh <- getlongformat(hbai, weight = "gs_newwa", pov = measure, by = "workinghh") %>%
      mutate(WorkStatus = groupingvar)

    # Single adults
    singlehh <- filter(hbai, singlehh != "(Missing)") %>%
      getlongformat(weight = "gs_newad", pov = measure, by = "singlehh") %>%
      mutate(HouseholdTypeGender = groupingvar)

    # People
    tenhbai <- getlongformat(hbai, weight = "gs_newpp", pov = measure, by = "tenhbai") %>%
      mutate(TypeOfTenure = groupingvar)
    urinds <- getlongformat(hbai, weight = "gs_newpp", pov = measure, by = "urinds") %>%
      mutate(UrbanRuralClassification = groupingvar)
    depchldh <- hbai %>%
      mutate(depchldh = case_when(depchldh == "1 child in the household" ~ "One",
                                  depchldh == "2 children in the household" ~ "Two",
                                  depchldh == "3 or more children in the household" ~ "Three or more",
                                  depchldh == "No children in the household" ~ "None")) %>%
      getlongformat(weight = "gs_newpp", pov = measure, by = "depchldh") %>%
      mutate(NumberOfChildrenInHousehold = groupingvar)
    dispp_hh <- getlongformat(hbai, pov = measure, weight = "gs_newpp", by = "dispp_hh") %>%
      mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                              grepl("disabled", groupingvar, fixed = TRUE) &
                              Measurement == "Count", "-", Value)) %>%
      mutate(HouseholdDisabilityStatus = groupingvar)

    disch_hh <- getlongformat(hbai, pov = measure, weight = "gs_newpp", by = "disch_hh") %>%
      mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                              grepl("disabled", groupingvar, fixed = TRUE) &
                              Measurement == "Count", "-", Value)) %>%
      mutate(HouseholdDisabilityStatus = groupingvar)

    disad_hh <- getlongformat(hbai, pov = measure, weight = "gs_newpp", by = "disad_hh")  %>%
      mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                              grepl("disabled", groupingvar, fixed = TRUE) &
                              Measurement == "Count", "-", Value)) %>%
      mutate(HouseholdDisabilityStatus = groupingvar)

    ethgrphh <- getlongformat(hbai, pov = measure, weight = "gs_newpp",
                              by = "ethgrphh", get5yr = TRUE) %>%
      filter(DateCode == levels(labels$years$long5yrperiods)[22],
             groupingvar != "All") %>%
      mutate(Ethnicity = groupingvar)

    all$pn <- pn
    all$sa <- singlehh
    all$ad <- rbind(newfambu, ageband, marital, religsc)
    all$wa <- rbind(ecobu, workinghh)
    all$pp <- rbind(depchldh, tenhbai, urinds, dispp_hh, disch_hh, disad_hh,
                    ethgrphh)
  }


  # Children
  loneparenthh <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "loneparenthh") %>%
    mutate(SingleParenthood = groupingvar)
  ecobu_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "ecobu") %>%
    mutate(FamilyEmploymentStatus = groupingvar)
  workinghh_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "workinghh") %>%
    mutate(WorkStatus = groupingvar)
  tenhbai_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "tenhbai") %>%
    mutate(TypeOfTenure = groupingvar)
  urinds_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "urinds") %>%
    mutate(UrbanRuralClassification = groupingvar)
  depchldh_ch <- hbai %>%
    mutate(depchldh = case_when(depchldh == "1 child in the household" ~ "One",
                                depchldh == "2 children in the household" ~ "Two",
                                depchldh == "3 or more children in the household" ~ "Three or more",
                                depchldh == "No children in the household" ~ "None")) %>%
    getlongformat(pov = measure, weight = "gs_newch", by = "depchldh") %>%
    mutate(NumberOfChildrenInHousehold = groupingvar) %>%
    filter(NumberOfChildrenInHousehold != "None")
  dispp_hh_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "dispp_hh") %>%
    mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                            grepl("disabled", groupingvar, fixed = TRUE) &
                            Measurement == "Count", "-", Value)) %>%
    mutate(HouseholdDisabilityStatus = groupingvar)
  disch_hh_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "disch_hh") %>%
    mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                            grepl("disabled", groupingvar, fixed = TRUE) &
                            Measurement == "Count", "-", Value)) %>%
    mutate(HouseholdDisabilityStatus = groupingvar)
  disad_hh_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch", by = "disad_hh")  %>%
    mutate(Value = ifelse(DateCode %in% labels$years$longperiods[19:20] &
                            grepl("disabled", groupingvar, fixed = TRUE) &
                            Measurement == "Count", "-", Value)) %>%
    mutate(HouseholdDisabilityStatus = groupingvar)
  # skipped characteristics in time series, therefore starting in 2006/07
  babyhh <- filter(hbai, yearn >= 12) %>%
    getlongformat(pov = measure, weight = "gs_newch", by = "babyhh") %>%
    mutate(AgeOfYoungestChild = groupingvar,
           AgeOfYoungestChild = case_when(groupingvar == "Youngest child in household is 1 or older" ~ "Youngest child in household is one or older",
                                          groupingvar == "Youngest child in household is younger than 1" ~ "Youngest child in household is younger than one",
                                          TRUE ~ groupingvar))
  # skipped characteristics in time series, therefore starting in 2006/07
  youngmumhh <- filter(hbai, yearn >= 12) %>%
    getlongformat(pov = measure, weight = "gs_newch", by = "youngmumhh") %>%
    mutate(AgeOfMother = groupingvar)

  # skipped characteristics in time series, therefore starting in 2006/07
  ageband_ch <- filter(child, yearn >= 12) %>%
    getlongformat(weight = "childwgt", pov = measure, by = "ageband") %>%
    mutate(Age = groupingvar)

  ethgrphh_ch <- getlongformat(hbai, pov = measure, weight = "gs_newch",
                               by = "ethgrphh", get5yr = TRUE) %>%
    filter(DateCode == levels(labels$years$long5yrperiods)[22],
           groupingvar != "All") %>%
    mutate(Ethnicity = groupingvar)

  all$ch <- rbind(loneparenthh, depchldh_ch, ecobu_ch, workinghh_ch, tenhbai_ch,
                  urinds_ch, babyhh, youngmumhh, dispp_hh_ch, disch_hh_ch,
                  disad_hh_ch, ethgrphh_ch, ageband_ch)

  all
}

get_disability_type_composition <- function(df, variable, weight) {

  df$var <- df[[variable]]
  df$wgt <- df[[weight]]

  df %>%
    filter(discor == "Disabled") %>%
    group_by(yearn, var) %>%
    summarise(number = sum(wgt, na.rm = TRUE),
              sample = n()) %>%
    group_by(yearn) %>%
    summarise(composition = number[1] / sum(number),
              number = number[1],
              sample_type = sample[1],
              sample = sum(sample),
              type = case_when(variable == "disd01" ~ "Vision",
                               variable == "disd02" ~ "Hearing",
                               variable == "disd03" ~ "Mobility",
                               variable == "disd04" ~ "Dexterity",
                               variable == "disd05" ~ "Learning",
                               variable == "disd06" ~ "Memory",
                               variable == "disd07" ~ "Mental health",
                               variable == "disd08" ~ "Stamina/breathing/fatigue",
                               variable == "disd09" ~ "Social/behavioural",
                               variable == "disd10" ~ "Other",
                               variable == "discor" ~ "All")) %>%
    ungroup()
}

get_disability_types <- function(df, weight) {

  rbind(get_disability_type_composition(df = df, weight = weight, variable = "disd01"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd02"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd03"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd04"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd05"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd06"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd07"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd08"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd09"),
        get_disability_type_composition(df = df, weight = weight, variable = "disd10"),
        get_disability_type_composition(df = df, weight = weight, variable = "discor"))

}


# Utils ------------------------------------------------------------------------

get_periods <- function(yearn) {

  factor(yearn, levels = labels$years$numbered, labels = labels$years$periods)

}

get_years <- function(yearn) {

  factor(yearn, levels = labels$years$numbered, labels = labels$years$formatted)

}

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
  ifelse(!is.na(x), round2(x, 7), NA)
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

create_worksheet <- function(sheet_no, file, tables, headers, titles) {

  mysheet <- as.character(sheet_no)
  headerrows = length(headers)

  # Open / create workbook and worksheet
  if (file.exists(file)) {
    wb <- loadWorkbook(file)
    if (mysheet %in% getSheetNames(file)) {
      removeWorksheet(wb = wb, sheet = mysheet)
    }
  } else {
    wb <- createWorkbook()
  }

  # New worksheet
  addWorksheet(wb = wb, sheetName = mysheet, gridLines = FALSE)

  # write worksheet title, add number
  writeData(wb, mysheet, paste0(sheet_no, " ", headers[1]))

  # write remaining worksheet header (notes, source)
  if (headerrows > 1) {
    writeData(wb, mysheet, startRow = 2, headers[2:headerrows])
    }

  # format worksheet header
  addStyle(wb, mysheet, createStyle(fontSize = 13, textDecoration = "bold"),
           rows = 1, cols = 1)
  addStyle(wb, mysheet, createStyle(wrapText = TRUE),
           rows = 2:headerrows, cols = 1)

  # get first row of first table (first table title)
  first_row <- 1 + headerrows

  # loop through tables for this worksheet
  for (i in 1:length(tables)) {

    table <- tables[[i]]
    tablerows <- dim(table)[1] + 1

    # write title (with table number added)
    writeData(wb, mysheet, paste0(sheet_no, letters[i], " ", titles[i]), startRow = first_row)

    # write table (marked up properly as table)
    writeDataTable(wb, mysheet, table,
                   startRow = first_row + 1,
                   withFilter = FALSE, firstColumn = TRUE, bandedRows = FALSE,
                   tableStyle = "TableStyleLight1",
                   tableName = paste0("Table", sheet_no, letters[i]),
                   keepNA = TRUE, na.string = "[u]")

    # format title
    addStyle(wb, mysheet, rows = first_row, cols = 1,
             style = createStyle(textDecoration = "bold", halign = "left"))
    setRowHeights(wb, mysheet, rows = first_row, 30)

    # format table header
    addStyle(wb, mysheet, rows = first_row + 1, cols = 2:length(table),
             gridExpand = TRUE, stack = TRUE,
             style = createStyle(wrapText = TRUE, halign = "right"))

    # identify number format
    if (any(table[, 2:length(table)] <= 2, na.rm = TRUE)) {numformat <- "0%"} else {numformat <- "#,##0"}

    # format table body
    addStyle(wb, mysheet,
             rows = (first_row + 2):(first_row + 2 + tablerows),
             cols = 2:length(table),
             gridExpand = TRUE, stack = TRUE,
             style = createStyle(numFmt = numformat, halign = "right"))

    setColWidths(wb, mysheet, cols = 1, widths = 70)

    # move to next sub-table on this worksheet
    first_row <- first_row + tablerows + 1

  }

  # right align all columns except for 1st
  addStyle(wb, mysheet, rows = 3:100, cols = 2:length(table),
           gridExpand = TRUE, stack = TRUE,
           style = createStyle(halign = "right"))
  addStyle(wb, mysheet, rows = 1:200, cols = 1,
           gridExpand = TRUE, stack = TRUE,
           style = createStyle(halign = "left"))

  # Save spreadsheet
  saveWorkbook(wb, file, overwrite = TRUE)
}


createContentSheet <- function(filename, title, toptext, headings = NULL) {

  wb <- loadWorkbook(filename)

  # get sheet names
  sheets <- names(wb)
  sheets <- sheets[!sheets == "Contents"]
  sheets <- sheets[!sheets == "Readme"]

  # get worksheet titles
  titles <- vector(length = length(sheets))
  for (i in 1:length(sheets)) {
    titles[i] <-  read.xlsx(wb, sheet = sheets[i], startRow = 1, colNames = FALSE,
                            cols = 1, rows = 1) %>% pull()
  }

  # remove heading markers from titles
  titles <- sapply(titles, function(x) str_remove(x, "H1-"))

  # create hyperlinks to sheets
  sheetlinks <- sapply(seq_along(sheets),
                       function(x) makeHyperlinkString(sheets[[x]],
                                                       text = titles[[x]]))

  sheetlinks <- sapply(seq_along(sheets),
                       function(x) paste0('=HYPERLINK("#',
                                          sheets[[x]],
                                          '!A1", "',
                                          titles[[x]],
                                          '")'))

  # insert spaces for the headers
  sheetlinks <- insertSpaces(sheetlinks, headings$location)

  # create new worksheet
  if ("Contents" %in% getSheetNames(filename)) {removeWorksheet(wb, "Contents")}
  addWorksheet(wb, "Contents", gridLines = FALSE)

  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  noteStyle <- createStyle(fontName = "Segoe UI", fontSize = 12, halign = "left",
                           wrapText = TRUE)
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 12, halign = "left")
  tocStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                          fontColour = "blue", textDecoration = "underline",
                          halign = "left")

  # write title
  writeData(wb, "Contents", title, startRow = 1, startCol = 1)
  addStyle(wb, "Contents", rows = 1, cols = 1, style = titleStyle)

  # add note
  writeData(wb, "Contents", toptext, startRow = 2, startCol = 1)
  addStyle(wb, "Contents", rows = 2:(length(toptext) + 1), cols = 1, style = noteStyle)

  # add list of sheets
  writeFormula(wb, "Contents", startRow = length(toptext) + 2, startCol = 1, x = sheetlinks)
  addStyle(wb, "Contents", rows = (length(toptext) + 2):(length(sheetlinks) + length(toptext) + 2),
           cols = 1,
           style = tocStyle)

  # add headers
  pos <- grep("blank", sheetlinks)

  for (i in 1:length(pos)) {
    writeData(wb, "Contents", x = headings$titles[i],
              startRow = length(toptext) + 1 + pos[i],
              startCol = 1)
    addStyle(wb, "Contents", rows = length(toptext) + 1 + pos[i], cols = 1,
             style = headerStyle)
    setRowHeights(wb, "Contents", rows = length(toptext) + 1 + pos[i], heights = 30)
  }

  setColWidths(wb, "Contents", 1, widths = 105)

  # move contents sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])

  saveWorkbook(wb, filename, overwrite = TRUE)
}

createReadmeSheet <- function(filename, notes, title = "Readme") {

  wb <- loadWorkbook(filename)

  # create new worksheet
  if (title %in% getSheetNames(filename)) {removeWorksheet(wb, title)}
  addWorksheet(wb, title, gridLines = FALSE)

  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI ", fontSize = 14,
                            textDecoration = "bold")

  headerStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                             textDecoration = "bold")

  noteStyle <- createStyle(fontName = "Calibri", fontSize = 12, halign = "left",
                           wrapText = TRUE)

  # write text
  startrow <- 1

  for (i in 1:length(notes)) {

    # write header
    writeData(wb, title, x = names(notes)[i], startRow = startrow, startCol = 1)
    addStyle(wb, title, rows = startrow, cols = 1, style = headerStyle)
    setRowHeights(wb, title, rows = startrow, heights = 30)

    # write note
    writeData(wb, title, notes[[i]], startRow = startrow + 1, startCol = 1)
    addStyle(wb, title, cols = 1, style = noteStyle,
             rows = (startrow + 1):(startrow + 1 + length(notes[[i]])))

    # move to next note
    startrow = startrow + length(notes[[i]]) + 1

  }

  addStyle(wb, title, rows = 1, cols = 1, style = titleStyle)
  setColWidths(wb, title, 1, widths = 85)

  # move Readme sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])

  saveWorkbook(wb, filename, overwrite = TRUE)
}

insertSpaces <- function(vector, location) {
  for (i in 1:(length(location))) {
    vector <- append(vector, "blank", after = (location[i] + i - 1))
  }
  vector
}



mark_missing <- function(filename, mark_list) {

  wb <- loadWorkbook(filename)
  sheets <- names(mark_list)

  for (i in 1:length(sheets)) {

    markers <- mark_list[[i]]$markers
    ranges <- mark_list[[i]]$ranges

    for (j in 1:length(ranges)) {
      mark_range(wb = wb, sheetname = sheets[i], range = ranges[j],
                 marker = markers[j])
    }
  }
  saveWorkbook(wb, filename, overwrite = TRUE)
}

col_to_int <- function(cellref) {

  col <- sub('[[:digit:]]+','', cellref)
  x1 <- strsplit(col, "")[[1]][1]
  x2 <- strsplit(col, "")[[1]][2]

  out <- which(LETTERS == toupper(x1))

  if (!is.na(x2)) {out <- out * 26 + which(LETTERS == toupper(x2))}

  return(out)
}
cell_to_coord <- function(cellref) {

  x1 <- col_to_int(cellref)
  x2 <- sub('[[:alpha:]]+','', cellref)
  x2 <- as.numeric(x2)
  return(c(x1, x2))

}
range_to_coord <- function(range) {
  cell1 <- str_split(range, ":")[[1]][1]
  cell2 <- str_split(range, ":")[[1]][2]

  coord1 <- cell_to_coord(cell1)
  coord2 <- cell_to_coord(cell2)
  return(list(coord1, coord2))
}
mark_range <- function(wb, sheetname, range, marker) {

  coords <- range_to_coord(range)
  ncols <- abs(coords[[2]][1] - coords[[1]][1]) + 1
  nrows <- abs(coords[[2]][2] - coords[[1]][2]) + 1

  df <- data.frame(matrix(marker, nrows, ncols), stringsAsFactors = FALSE)

  writeData(wb, sheetname, x = df, xy = coords[[1]], colNames = FALSE)
}

# Chart functions --------------------------------------------------------------

linechart <- function(df, ...) {

  ggplot(data = df,
         aes(x = x, y = y, group = key, colour = key, fill = key,
             tooltip = tooltip, data_id = data_id)) +
    scale_x_discrete() +
    addrecessionbar(...) +
    geom_point_interactive(show.legend = FALSE,
                           size = 6,
                           colour = "white",
                           alpha = 0.01) +

    # line ends at second to last data point
    geom_line(data = arrange(df, desc(x)) %>% group_by(key) %>% slice(2:n()),
              aes(linetype = key), lineend = "round", show.legend = FALSE) +

    # points at first and second-to-last data point
    geom_point(data = arrange(df, desc(x)) %>%
                 group_by(key) %>%
                 slice(2:n()) %>%
                 filter(x == min(x) | x == max(x)),
               size = 2, show.legend = FALSE) +

    # points for final data point
    geom_point(data = df %>% filter(x == max(x)), size = 2, shape = 15,
               show.legend = FALSE) +

    theme(axis.text.y = element_blank())
}

linechart_small <- function(df, yrange = c(0.1, 0.35), col = SGblue){

  ggplot(data = df,
         aes(x = x,
             y = y,
             group = key,
             label = label)) +

    # line ends at second to last data point
    geom_line(data = arrange(df, desc(x)) %>% group_by(key) %>% slice(2:n()),
              lineend = "round", show.legend = FALSE, size = 1.2, colour = col) +

    # points at first and second-to-last data point
    geom_point(data = arrange(df, desc(x)) %>%
                 group_by(key) %>%
                 slice(2:n()) %>%
                 filter(x == min(x) | x == max(x)),
               size = 3, show.legend = FALSE, colour = col) +

    # points for final data point
    geom_point(data = df %>% filter(x == max(x)), size = 3, shape = 15,
               show.legend = FALSE, colour = col) +

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
                     breaks = c("1994-97", "2018-21"),
                     expand = c(0.4, 0.45)) +

    theme(axis.text = element_text(size = 16))

}

barchart <- function(df, palette = SGmix6) {

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

  df %>%
    ggplot(aes(x = x, y = y, group = key, colour = key, label = labels,
               tooltip = tooltip, data_id = data_id)) +
    scale_x_discrete(limits = periods,
                     breaks = c("2010-2014", "", "2012-2016", "", "2014-2018",
                                "", "2016-2020"),
                     expand = expansion(add = c(3, 1))) +
    scale_y_continuous(limits = c(0, .2)) +
    geom_point_interactive(show.legend = FALSE,
                           size = 6,
                           colour = "white",
                           alpha = 0.01) +
    geom_line(lineend = "round", show.legend = FALSE) +
    geom_point(data = filter(df, x == min(x) | x == max(x)), size = 2,
               show.legend = FALSE) +
    labs(caption = "Source: Understanding Society Survey") +
    addscales() +
    geom_text_repel(data = filter(df, x == min(x)),
                    aes(x = x, y = y, label = labels),
                    direction = "y",
                    nudge_x = -0.5,
                    hjust = 1,
                    point.padding = 0.2,
                    box.padding = 0.2,
                    show.legend = FALSE,
                    min.segment.length = 10) +
    geom_text_repel(data = filter(df, x == max(x)),
                    aes(x = x, y = y, label = labels),
                    direction = "y",
                    nudge_x = +0.5,
                    hjust = 0,
                    point.padding = 0.2,
                    box.padding = 0.2,
                    show.legend = FALSE,
                    min.segment.length = 10)
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

addscales <- function(palette = SGmix6){

  list(scale_color_manual(values = palette),
       scale_fill_manual(values = palette),
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

  c <- geom_text(aes(x = 33, y = target,
                       label = fmtpct(target)),
                 colour = SGgreys[1],
                 size = 5)

  d <- geom_point_interactive(aes(x = "2023/24",
                                  y = target,
                                  tooltip = str_c("Interim target (2023/24): ",
                                                  fmtpct(target)),
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

  c <- geom_text(aes(x = 40, y = target,
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

# html -------------------------------------------------------------------------

infobox <- function(md) {
  div(class = "infobox-text",
      create_html(md))
}

message <- function(md) {
  tags <- htmltools::tags
  div(class = "message-text",
      tags$strong(create_html(md)))
}

create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}

abbr <- function(short, long) {
  tags <- htmltools::tags
  tags$abbr(title = long,
            short)
}

interactive <- function(chart,
                        title,
                        subtitle,
                        description = NULL,
                        height = 3.5) {

  id <- word(title, 1L, sep = ":")
  id <- word(id, 2L)
  id <- paste0("figure-", id)

  subtitle <- str_wrap(subtitle, 85)
  chart <- chart + labs(title = subtitle)
  chart <- girafe(ggobj = chart, width = 1, height_svg = height, width_svg = 7)
  chart <- girafe_options(chart, opts_toolbar(pngname = id))

  # tidy up svg html
  chart$elementId <- paste0(id, "_htmlwidget")
  chart$x$html <- str_replace_all(chart$x$html, chart$x$uid, paste0(id, "_svg"))
  chart$x$uid <- paste0(id, "_svg")

  tags <- htmltools::tags

  if (is.null(description)) {

    tags$div(tags$figure(id = id,
                         "aria-labelledby" = paste0(id, "_caption"),
                         tags$figcaption(id = paste0(id, "_caption"),
                                         title),
                         chart))
  } else {
    tags$div(tags$div(id = paste0(id, "_description"),
                      create_html(description)),
             tags$figure(id = id,
                         "aria-describedby" = paste0(id, "_description"),
                         "aria-labelledby" = paste0(id, "_caption"),
                         tags$figcaption(id = paste0(id, "_caption"),
                                         title),
                         chart))
  }
}

tables_panel <- function(tables) {
  tags$div(class = "accordion",
           tags$button(id = paste0("accordion-button-", fign),
                       class = "btn collapsed accordion-header noprint",
                       "data-toggle" = "collapse",
                       "data-target" = paste0("#accordion-panel-", fign),
                       "aria-controls" = paste0("accordion-panel-", fign),
                       "Show / hide data tables"),
           tags$div(id = paste0("accordion-panel-", fign),
                    class = "collapse accordion-panel",
                    tables)
  )
}

make4panels <- function(title_tl, subtitle_tl, text_tl, chart_tl, desc_tl,
                        title_tr, subtitle_tr, text_tr, chart_tr, desc_tr,
                        title_bl, subtitle_bl, text_bl, chart_bl, desc_bl,
                        title_br, subtitle_br, text_br, chart_br, desc_br) {

  tags <- htmltools::tags

  chart_tl <- girafe(ggobj = chart_tl)
  chart_tr <- girafe(ggobj = chart_tr)
  chart_bl <- girafe(ggobj = chart_bl)
  chart_br <- girafe(ggobj = chart_br)

  div(class = "row fluid-row",

      div(class = "col-md-6",

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_tl),
                  p(subtitle_tl)),
              tags$figure(role = "figure",
                          class = "panel-body",
                          style = "max-width: 438px;",
                          "aria-label" = desc_tl,
                          tags$figcaption(text_tl),
                          chart_tl)),

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_bl),
                  p(subtitle_bl)),
              tags$figure(role = "figure",
                          class = "panel-body",
                          style = "max-width: 438px;",
                          "aria-label" = desc_bl,
                          tags$figcaption(text_bl),
                          chart_bl))),

      div(class = "col-md-6",

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_tr),
                  p(subtitle_tr)),
              tags$figure(role = "figure",
                          class = "panel-body",
                          style = "max-width: 438px;",
                          "aria-label" = desc_tr,
                          tags$figcaption(text_tr),
                          chart_tr)),

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_br),
                  p(subtitle_br)),
              tags$figure(role = "figure",
                          class = "panel-body",
                          style = "max-width: 438px;",
                          "aria-label" = desc_br,
                          tags$figcaption(text_br),
                          chart_br))) )
}

# Create accessible (= marked up correctly), responsive html table

#'//////////////////////////////////////////////////////////////////////////////
#' FILE: datatable.R
#' AUTHOR: David Ruvolo
#' CREATED: 2019-12-05
#' MODIFIED: 2021-04-18
#' PURPOSE: build datatable function and helpers
#' STATUS: working
#' PACKAGES: htmltools
#' COMMENTS:
#'      The datatable function generates an html table from a dataset.
#'      This func returns a shiny tagList object which can be used in shiny
#'      applications, markdown documents, or written to an html file. The
#'      datatable function takes the following arguments.
#'
#'      ARGUMENTS:
#'      - data: the input dataset
#'      - id: an identifier for the table ideal for styling specific tables
#'            or for use in js
#'      - caption: a title for the table (recommended for accessible tables)
#'      - options:
#'          - responsive: a logical arg for turning on/off the rendering of
#'                      additional elements for responsive tables (i.e., span).
#'                      (Default = FALSE)
#'          - rowHeaders: a bool that renders the first cell of every row
#'              as a row header. This is useful for datasets where all data
#'              in a row is related, e.g., patient data. If set to TRUE,
#'              the data must be organized so that the row header is the
#'              first column.
#'          - `asHTML`: a logical argument used to render cell text as html
#'               elements (default = FALSE)
#'
#'      ABOUT:
#'      The datatable function requires two helper functions: 1) to generate the
#'      table header and another used 2) to generate the table body. The func
#'      build_header() renders the <thead> element according to the input data.
#'      The build_body functions renders the table's <tbody> based on the input
#'      and the options. This function uses a nested lapplys to iterate each row
#'      and cell. If the responsive opt is TRUE, then the function will return
#'      a <span> element with the current cell's column name. <span> has
#'      the class `hidden-colname` that hides/shows the element based on screen
#'      size (see datatable.css). Role attributes are added in the event
#'      the display properties are altered in css.
#'//////////////////////////////////////////////////////////////////////////////

#' @name datatable_helpers
#' @description object containing the datatable helper functions
datatable_helpers <- list()

#' @name build_header
#' @description generate the table header markup
#'
#' @param data input database (from `datatable`)
#' @param options internal configuration object (from `datatable`)
datatable_helpers$build_header <- function(data, options) {
  columns <- colnames(data)
  cells <- lapply(seq_len(length(columns)), function(n) {

    # define cell content: as html or text
    if (isTRUE(options$asHTML)) {
      cell_value <- htmltools::HTML(columns[n])
    } else {
      cell_value <- columns[n]
    }

    # build header
    cell <- htmltools::tags$th(scope = "col", cell_value)
    cell
  })

  # return header
  htmltools::tags$thead(
    htmltools::tags$tr(role = "row", cells)
  )
}

#' @name build_body
#' @description generate the markup for the table body

#' @param data input dataset (from `datatable`)
#' @param options internal configuration object (from `datatable`)
#'
#' @return shiny.tag object
datatable_helpers$build_body <- function(data, options) {
  body <- lapply(seq_len(NROW(data)), function(row) {
    cells <- lapply(seq_len(NCOL(data)), function(col) {

      # process options: render as html or escape?
      if (isTRUE(options$asHTML)) {
        cell_value <- htmltools::HTML(data[row, col])
      } else {
        cell_value <- data[row, col]
      }

      # process options$rowHeaders (this generates the cell)
      if (isTRUE(options$rowHeaders) && col == 1) {
        cell <- htmltools::tags$th(role = "rowheader")
      } else {
        cell <- htmltools::tags$td(role = "cell")
      }

      # process options: responsive and rowHeaders
      if (isTRUE(options$responsive)) {
        cell$children <- list(
          htmltools::tags$span(
            class = "hidden-colname",
            `aria-hidden` = "true",
            colnames(data)[col]
          ),
          cell_value
        )
      } else {
        cell$children <- list(
          cell_value
        )
      }
      cell
    })
    htmltools::tags$tr(role = "row", cells)
  })
  htmltools::tags$tbody(body)
}


#' @name datatable
#'
#' @description generate a responsive datatable
#'
#' @param data input dataset
#' @param id a unique identifier for the table
#' @param caption an optional caption to render
#' @param options a list containing additional parameters for configuring
#'          the table output
#'
#' @section Options
#'
#' `responsive`: If TRUE (default), the table markup will be generated for
#'      responsiveness
#' `rowHeaders`: If TRUE (default), the first value in each row is considered
#'      as a row header (required for responsive tables)
#' `asHTML`: if TRUE, all values will be treated as HTML and rendered
#'      accordingly
#'
#' @return a shiny.tag object
datatable <- function(
  data,
  id = NULL,
  caption = NULL,
  source = NULL,
  options = list(responsive = TRUE, rowHeaders = TRUE, asHTML = FALSE)
) {

  # render table and table elements
  tbl <- htmltools::tags$table(
    class = "table",
    datatable_helpers$build_header(data, options),
    datatable_helpers$build_body(data, options)
  )

  # add id, caption
  if (!is.null(id)) tbl$attribs$id <- id
  if (!is.null(caption)) {
    tbl$children <- list(
      htmltools::tags$caption(caption,
                              htmltools::tags$br(),
                              htmltools::tags$span(source)),
      tbl$children
    )
  }

  return(tbl)
}
