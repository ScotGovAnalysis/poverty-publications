# Data analysis ----------------------------------------------------------------

getpovby <- function(df,
                     pov = "low60ahc",
                     weight = "gs_newpp",
                     by = NULL) {

  df$povvar <- df[[pov]]
  df$weightvar <- df[[weight]]

  if (is.null(by)) {

    df$groupingvar <- "All"
    multiplier <- 1

  } else {

    df$groupingvar <- df[[by]]
    df <- rbind(df, df %>% mutate(groupingvar = "All"))

    # this fixes double-counting in composition (i.e. "All" is not another
    # category, but all combined)
    multiplier <- 2
  }

  df %>%
    group_by(yearn, povvar, groupingvar, .drop = FALSE) %>%
    summarise(number = sum(weightvar),
              sample = sum(weightvar > 0, na.rm = TRUE)) %>%
    group_by(yearn, groupingvar, .drop = FALSE) %>%
    mutate(povsample = sample,
           sample = sum(sample),
           population = sum(number)) %>%
    filter(povvar == 1) %>%
    group_by(yearn) %>%
    mutate(sample = ifelse(is.na(sample), 0, sample),
           number = ifelse(is.na(number), 0, number),
           number = ifelse(sample == 0, NA, number),
           population = ifelse(is.na(population), 0, population),
           rate = ifelse(population > 0, number / population, NA),
           composition = number / sum(number, na.rm = TRUE) * multiplier) %>%
    ungroup() %>%
    select(yearn, groupingvar, number, rate, composition,
           sample, povsample) %>%

    # move total to top
    mutate(groupingvar = factor(groupingvar),
           groupingvar = fct_relevel(groupingvar, "All", after = 0L)) %>%
    arrange(yearn, groupingvar) %>%
    mutate(weight = weight,
           type = pov,
           groups = by)
}

getheadlines <- function(df, pov = "low60ahc", threeyr = FALSE, ...) {

  pp <- getpovby(df = df, pov = pov)
  ch <- getpovby(df = df, pov = pov, weight = "gs_newch")
  wa <- getpovby(df = df, pov = pov, weight = "gs_newwa")
  pn <- getpovby(df = df, pov = pov, weight = "gs_newpn")

  if (threeyr) {
    pp <-  getnyrtable(pp, ...) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    ch <-  getnyrtable(ch, ...) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    wa <-  getnyrtable(wa, ...) %>%
      mutate(year = factor(yearn,
                           levels = labels$years$numbered,
                           labels = labels$years$periods))
    pn <-  getnyrtable(pn, ...) %>%
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

  rate_pp <- select(pp, year, rate) %>% pivot_wider(names_from = year, values_from = rate) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  rate_ch <- select(ch, year, rate) %>% pivot_wider(names_from = year, values_from = rate) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  rate_wa <- select(wa, year, rate) %>% pivot_wider(names_from = year, values_from = rate) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  rate_pn <- select(pn, year, rate) %>% pivot_wider(names_from = year, values_from = rate)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  rates <- rbind(rate_pp, rate_ch, rate_wa, rate_pn)

  comp_pp <- select(pp, year, composition) %>%
    pivot_wider(names_from = year, values_from = composition) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  comp_ch <- select(ch, year, composition) %>%
    pivot_wider(names_from = year, values_from = composition) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  comp_wa <- select(wa, year, composition) %>%
    pivot_wider(names_from = year, values_from = composition) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  comp_pn <- select(pn, year, composition) %>%
    pivot_wider(names_from = year, values_from = composition)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  comps <- rbind(comp_pp, comp_ch, comp_wa, comp_pn)

  number_pp <- select(pp, year, number) %>%
    pivot_wider(names_from = year, values_from = number) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  number_ch <- select(ch, year, number) %>%
    pivot_wider(names_from = year, values_from = number) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  number_wa <- select(wa, year, number) %>%
    pivot_wider(names_from = year, values_from = number) %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  number_pn <- select(pn, year, number) %>%
    pivot_wider(names_from = year, values_from = number) %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  numbers <- rbind(number_pp, number_ch, number_wa, number_pn)

  sample_pp <- select(pp, year, sample) %>%
    pivot_wider(names_from = year, values_from = sample) %>%
    mutate(Group = "All people") %>%
    select(Group, everything())
  sample_ch <- select(ch, year, sample) %>%
    pivot_wider(names_from = year, values_from = sample) %>%
    mutate(Group = "Children") %>%
    select(Group, everything())
  sample_wa <- select(wa, year, sample) %>%
    pivot_wider(names_from = year, values_from = sample)  %>%
    mutate(Group = "Working-age adults") %>%
    select(Group, everything())
  sample_pn <- select(pn, year, sample) %>%
    pivot_wider(names_from = year, values_from = sample)  %>%
    mutate(Group = "Pensioners") %>%
    select(Group, everything())

  sample <- rbind(sample_pp, sample_ch, sample_wa, sample_pn)
  list(rates = rates, comps = comps, numbers = numbers, sample = sample)
}

summarise_data <- function(df, periodlength = 3, ...) {

  df %>%
    group_by(groupingvar) %>%
    getnyrtable(n = periodlength, ...) %>%
    samplesizecheck() %>%
    roundall() %>%
    mutate(year = get_periods(yearn, n = periodlength))

}

getpriorityrate <- function(df, pov, by, class) {
  getpovby(df, pov = pov, by = by, weight = "gs_newch") %>%
    group_by(groupingvar) %>%
    getnyrtable() %>%
    filter(yearn == max(yearn),
           groupingvar == class) %>%
    samplesizecheck() %>%
    roundall() %>%
    select(groupingvar, rate, type)
}

getfoodsec4 <- function(househol) {

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
           foodsec4 = case_when(foodsec_score == 0 ~ 1,
                                foodsec_score %in% c(1, 2) ~ 2,
                                foodsec_score %in% c(3, 4, 5) ~ 3,
                                foodsec_score >= 6 ~ 4)) %>%
    select(year, sernum, foodsec_score, foodsec4)
}

getfoodsecbyGroup <- function(df, weight = "gs_newpp") {

  df$weight <- df[[weight]]

  df %>%

    # add another instance of the same dataset to create the 'All' category
    rbind(df %>% mutate(Group = "All")) %>%

    filter(foodsec != "(Missing)",
           # keep only latest 3 years, no time series required
           yearn >= max(yearn) - 2,
           yearn != 27) %>%
    group_by(yearn, Group) %>%
    mutate(sample = sum(weight > 0, na.rm = TRUE)) %>%
    group_by(yearn, foodsec, Group) %>%
    summarise(number = sum(weight),
              Sample = max(sample),
              povsample = sum(weight > 0, na.rm = TRUE)) %>%
    group_by(foodsec, Group) %>%

    summarise(number = mean(number),
              Sample = sum(Sample),
              povsample = sum(povsample)) %>%

    group_by(Group) %>%
    mutate(composition = number/sum(number),
           # samplesize checks
           number = ifelse(povsample >= 100, number, NA),
           number = roundpop(number),
           composition = ifelse(Sample >= 100, composition, NA),
           composition = ifelse(composition <= 0.005, 99993, composition),
           composition = roundpct(composition)) %>%
    ungroup() %>%
    mutate(Group = fct_relevel(Group, "All", after = 0L)) %>%
    arrange(Group)
}

getdecptsbhc <- function(df){

  df %>%
    summarise("1" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.1, weights = gs_newpp),
              "2" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.2, weights = gs_newpp),
              "3" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.3, weights = gs_newpp),
              "4" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.4, weights = gs_newpp),
              "5" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.5, weights = gs_newpp),
              "6" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.6, weights = gs_newpp),
              "7" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.7, weights = gs_newpp),
              "8" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.8, weights = gs_newpp),
              "9" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                        probs = 0.9, weights = gs_newpp),
              "10" = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                         probs = 1, weights = gs_newpp))

}

getdecptsahc <- function(df){

  df %>%
    summarise("1" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.1, weights = gs_newpp),
              "2" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.2, weights = gs_newpp),
              "3" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.3, weights = gs_newpp),
              "4" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.4, weights = gs_newpp),
              "5" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.5, weights = gs_newpp),
              "6" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.6, weights = gs_newpp),
              "7" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.7, weights = gs_newpp),
              "8" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.8, weights = gs_newpp),
              "9" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                        probs = 0.9, weights = gs_newpp),
              "10" = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
                                         probs = 1, weights = gs_newpp))

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
              Scotmedian = Hmisc::wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
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
              Scotmedian = Hmisc::wtd.quantile(s_oe_ahc * ahcpubdef / ahcyrdef,
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
    pivot_wider(names_from = year, values_from = measure)

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

getnyrtable <- function(df, n = 3, ...) {

  df %>%
    mutate_at(vars(c(contains("rate")), contains("num"), contains("comp")),
              analysistools::getrollingmean, n, ...) %>%
    mutate_at(vars(contains("sample")), analysistools::getrollingtotal, n, ...) %>%
    filter(!is.na(sample)) %>%
    ungroup()
}


samplesizecheck <- function(df, suppressed = NA, low = NA) {
  df %>%
    mutate(number = ifelse(povsample >= 100, number, suppressed),
           rate = ifelse(sample >= 100, rate, suppressed),
           rate = ifelse(rate > 0.005,  rate, low))
}



get_disability_type_composition <- function(df, variable, weight) {

  df$var <- df[[variable]]
  df$wgt <- df[[weight]]

  df %>%
    filter(discor == "Disabled") %>%
    group_by(yearn, var) %>%
    summarise(number = sum(wgt, na.rm = TRUE),
              sample = sum(wgt > 0, na.rm = TRUE)) %>%
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

get_SE_by <- function(var, by, wgt, df, poollist) {

  out <- lapply(seq_along(poollist), function(i) {

    design <- svydesign(ids = ~psu,
                        strata = ~psu_pair_number,
                        nest = TRUE,
                        weights = wgt,
                        data = df %>% filter(yearn %in% poollist[[i]]))

    stats <- svyby(formula = var,
                   by = by,
                   design = design,
                   FUN = svyciprop,
                   vartype = c("ci", "se"),
                   method = "likelihood")

    stats %>%
      rename(key = 1,
             estimate = 2,
             se = 3) %>%
      mutate(period = names(poollist)[i]) %>%
      select(period, key, estimate, se, ci_l, ci_u)

  })
  bind_rows(out)
}

small_plot <- function(df) {

  mytheme <- ggplot2::theme_grey() +
    ggplot2::theme(text = element_text(colour = SGdarkgrey, size = 14),
                   line = element_line(colour = SGdarkgrey,
                                       linetype = 1,
                                       lineend = 2,
                                       size = 0.5),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line.x = element_line(),
                   axis.ticks.length = unit(2, "pt"),
                   axis.ticks.y = element_blank(),
                   axis.title = element_blank(),
                   axis.text = element_text(size = 16),
                   axis.text.y = element_blank())

  ggplot2::theme_set(mytheme)

  ggplot2::ggplot(data = df,
                  aes(x = x,
                      y = y,
                      group = Group,
                      label = label)) +

    geom_line(data = df,
              size = 1.2,
              lineend = "round",
              show.legend = FALSE,
              colour = SGmix6_cat[1]) +

    geom_point(data = filter(df, x == min(x)),
               size = 3,
               show.legend = FALSE,
               colour = SGmix6_cat[1]) +

    geom_point(data = filter(df, x == max(x)),
               size = 3,
               show.legend = FALSE,
               colour = SGmix6_cat[1]) +

    geom_text(data = filter(df, x == min(x)),
              show.legend = FALSE,
              hjust = 1,
              nudge_x = -1.5,
              size = 6,
              colour = SGmix6_cat[1]) +

    geom_text(data = filter(df, x == max(x)),
              show.legend = FALSE,
              hjust = 0,
              nudge_x = 1.5,
              size = 6,
              colour = SGmix6_cat[1]) +

    scale_y_continuous(limits = c(0, 0.4)) +

    scale_x_discrete(drop = FALSE,
                     breaks = c("1994-97", "2019-22"),
                     expand = c(0.4, 0.45))

}

# Target measures by priority group --------------------------------------------

get_target_measures <- function(df, by) {

  rel <- df %>% getpovby(pov = "low60ahc", by = by, weight = "gs_newch")
  abs <- df %>% getpovby(pov = "low60ahcabs", by = by, weight = "gs_newch")
  cmd <- df %>% getpovby(pov = "cmdahc", by = by, weight = "gs_newch")

  rbind(rel, abs, cmd)

}

# Utils ------------------------------------------------------------------------

weightstozero <- function(df, exclude){

  df_exclude <- df %>% filter(yearn == exclude)
  df_other <- df %>% filter(yearn != exclude)

  change <- names(df) %in% c("gs_newpp", "gs_newch", "gs_newwa", "gs_newpn",
                             "gs_newad", "gs_newsa", "adultwgt", "childwgt",
                             "wgt0_4", "wgt5_12", "wgt13plus", "wgt0_17",
                             "wgt65", "wgt_kidplus")
  df_exclude[change] <- 0
  rbind(df_other, df_exclude) %>% arrange(yearn)
}

'%notin%' <- function(x,y){
  !('%in%'(x,y))
}

get_periods <- function(yearn, n = 3) {

  if (n == 1) {
    factor(yearn, levels = labels$years$numbered, labels = labels$years$formatted)
  }  else if (n == 2) {
    factor(yearn, levels = labels$years$numbered, labels = labels$years$period2yr)
  } else if (n == 3) {
    factor(yearn, levels = labels$years$numbered, labels = labels$years$periods)
  } else if (n == 5) {
    factor(yearn, levels = labels$years$numbered, labels = labels$years$period5yr)
  }

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

# Spreadsheet functions --------------------------------------------------------

create_worksheet <- function(sheet_no, wb, tables, headers, titles) {

  mysheet <- as.character(sheet_no)
  headerrows = length(headers)

  # delete existing worksheet
  if (mysheet %in% names(wb)) {removeWorksheet(wb = wb, sheet = mysheet)}

  # New worksheet
  addWorksheet(wb = wb, sheetName = mysheet, gridLines = FALSE)

  # write worksheet title, add number
  writeData(wb, mysheet, paste0(sheet_no, " ", headers[1]))

  # write remaining worksheet header (notes, source)
  if (headerrows > 1) {
    writeData(wb, mysheet, startRow = 2, headers[2:headerrows])
  }

  # format worksheet header
  addStyle(wb, mysheet, createStyle(fontSize = 13,
                                    textDecoration = "bold"),
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

    # identify appropriate number format from table value range
    ispercent <- table[, 2:length(table)] < 5 & table[, 2:length(table)] > 0.0001

    # identify sample table (also for number format)
    issampletable <- grepl("sample", titles[i])

    if (any(ispercent, na.rm = TRUE) & !issampletable) {numformat <- "0%"} else {numformat <- "#,##0"}

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

  cat(paste0('Sheet ', mysheet, ' ("', headers[1], '") written.'), fill = TRUE)

}


createContentSheet <- function(wb, title, toptext, headings = NULL) {

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
                       function(x) paste0('=HYPERLINK("#',
                                          sheets[[x]],
                                          '!A1", "',
                                          titles[[x]],
                                          '")'))

  # insert spaces for the headers
  if (!is.null(headings)) {
    sheetlinks <- insertSpaces(sheetlinks, headings$location)
  }

  # create new worksheet
  if ("Contents" %in% names(wb)) {removeWorksheet(wb, "Contents")}
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

  if (!is.null(headings)) {
    pos <- grep("blank", sheetlinks)
  } else {
    pos <- length(sheetlinks) + 1
  }

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

}

createReadmeSheet <- function(wb, notes, title = "Readme") {

  # create new worksheet
  if (title %in% names(wb)) {removeWorksheet(wb, title)}
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

}

insertSpaces <- function(vector, location) {
  for (i in 1:(length(location))) {
    vector <- append(vector, "blank", after = (location[i] + i - 1))
  }
  vector
}

# highcharts functions ---------------------------------------------------------

complete_ts <- function(df, x = periods) {

  df$x <- as.character(df$x)

  combs <- merge(data.frame(x = as.character(x), stringsAsFactors = FALSE),
                 data.frame(key = as.character(unique(df$key)), stringsAsFactors = FALSE))
  df %>%
    right_join(combs, by = c("x", "key")) %>%
    mutate(x = factor(x, ordered = TRUE))
}

add_recessionbar <- function(hc) {

  hc %>%
    hc_xAxis(
      plotBands = list(
        list(
          # UK Great Recession from Q2 2008 to Q2 2009 (2008/09 - 2009/10)
          # COVID recession Q1-Q2 2020 (2019/20 - 2020/21)
          label = list(
            text = "Recession<br>2008/09",
            x = -8,
            align = "left",
            textAlign = "right"),
          from = 11.9,
          to = 15.1,
          color = SGlightgrey),
        list(
          label = list(
            text = "Recession<br>2020",
            x = -8,
            align = "left",
            textAlign = "right"),
          from = 22.9,
          to = 27.1,
          color = SGlightgrey)
      )
    )
}

add_cmdbar <- function(hc) {

  cmd <- list(from = 14.9,
              to = 15.1,
              color = SGmidgrey,
              label = list(text = "Methodology change<br>2010/11",
                           x = -8,
                           align = "left",
                           textAlign = "right"))

  covid <- list(from = 23.8,
                to = 25.2,
                color = SGlightgrey,
                borderColor = SGmidgrey,
                borderWidth = 2,
                label = list(
                  text = "Responses<br>affected by<br>the pandemic",
                  rotation = 0,
                  x = -10,
                  align = "left",
                  textAlign = "right"))

  hc %>%
    hc_xAxis(plotBands = list(cmd, covid))
}

add_disbar <- function(hc) {
  hc %>%
    hc_xAxis(
      plotBands = list(
        list(
          zIndex = 4,
          label = list(
            text = "Methodology changes<br>in 2002/03, 2004/05<br>and 2012/13",
            x = -8,
            align = "left",
            textAlign = "right"),
          from = 15.4,
          to = 15.6,
          color = SGlightgrey),
        list(zIndex = 4,
             from = 5.4,
             to = 5.6,
             color = SGlightgrey),
        list(zIndex = 4,
             from = 7.4,
             to = 7.6,
             color = SGlightgrey)
      )
    )
}

add_act_bars <- function(hc, annotations = TRUE, cmdbreak = FALSE) {

  cmd <- list(value = 16,
              color = SGmidgrey,
              width = 2,
              label = list(
                text = "Methodology change<br>2010/11",
                rotation = 0,
                x = -10,
                y = 16,
                align = "left",
                textAlign = "right",
                style = list(fontSize = "smaller")))

  act <- list(value = 23,
              color = SGpaleorange,
              width = 3,
              label = list(
                text = ifelse(annotations, "Child Poverty<br>(Scotland) Act<br>2017", ""),
                rotation = 0,
                x = -8,
                y = 16,
                align = "left",
                textAlign = "right",
                style = list(fontSize = "smaller")))

  interim <- list(value = 29,
                  width = 18,
                  label = list(
                    text = ifelse(annotations, "Interim<br>target<br>2023/24", ""),
                    rotation = 0,
                    x = -10,
                    y = 16,
                    align = "left",
                    textAlign = "right",
                    style = list(fontSize = "smaller")),
                  color = SGpaleorange)

  final <- list(value = 36,
                color = SGpaleorange,
                width = 18,
                label = list(
                  text = ifelse(annotations, "Final<br>target<br>2030/31", ""),
                  rotation = 0,
                  x = -10,
                  y = 16,
                  align = "left",
                  textAlign = "right",
                  style = list(fontSize = "smaller")))

  if (cmdbreak) {
    bands <- list(cmd, interim, final)
  } else {
    bands <- list(act, interim, final)
  }

  hc %>% hc_xAxis(plotLines = bands)

}

add_CIs <- function(hc, data, colors) {

  hc %>% hc_add_series(type = "arearange",
                       data = data,
                       hcaes(x = x, low = low, high = high, group = key),
                       color = colors,
                       fillOpacity = 0.3,
                       enableMouseTracking = FALSE,
                       lineColor = "transparent",
                       marker = list(enabled = FALSE),
                       legend = list(enabled = FALSE),
                       visible = FALSE,
                       zIndex = -1) %>%

    # extend tooltip to include range
    hc_tooltip(useHTML = TRUE,
               formatter = JS('function () {
                if (this.y < 100) {
                  return "<strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          Highcharts.numberFormat(this.y*100, 0) + "% (" +
                          Highcharts.numberFormat(this.point.low*100, 0) + "-" +
                          Highcharts.numberFormat(this.point.high*100, 0) + "%)";
                } else {
                  return "<strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          "£" + Highcharts.numberFormat(this.y, 0);
                }

                              }')
    )

}


hc_exporting_options <- function(hc) {

  hc %>%
    hc_exporting(enabled = TRUE,
                 filename = NULL,
                 buttons = list(
                   contextButton = list(
                     menuItems = c("fullscreen", "printChart", "downloadPNG",
                                   "downloadSVG")
                   )),

                 menuItemDefinitions = list(

                   fullscreen = list(
                     text = "View in fullscreen",
                     onclick = JS('function () {

                   var container = this.renderTo;

                   if (container.requestFullscreen) {
                   container.requestFullscreen();
                   } else if (container.mozRequestFullScreen) {
                   container.mozRequestFullScreen();
                   } else if (container.webkitRequestFullscreen) {
                   container.webkitRequestFullscreen();
                   } else if (container.msRequestFullscreen) {
                   container.msRequestFullscreen();
                   }
                   }')
                   )
                 )
    )

}

hc_col <- function(df) {

  hchart(df, type = "column", hcaes(x = x, y = y)) %>%
    hc_add_theme(my_theme) %>%
    hc_colors(colors = SGmix6_cat[order(c(1, 5, 4, 3, 2, 6))])  %>%
    hc_yAxis(title = list(text = ""),
             labels = list(
               formatter = JS('function () { return Math.round(this.value*100, 0) + "%"}')
             ))  %>%

    hc_xAxis(title = list(text = ""),
             tickLength = 0)  %>%

    hc_plotOptions(series = list(pointPadding = 0,
                                 dataLabels = list(enabled = TRUE,
                                                   formatter = JS('function () {
                              return Math.round(this.y*100, 0) + "%";} '),
                                                   style = list(#fontSize = 'medium',
                                                     color = SGdarkgrey),
                                                   shadow = FALSE,
                                                   inside = FALSE)) ) %>%


    hc_tooltip(# css below is required so that tooltips are still readable when
      # overlapping with titles
      formatter = JS('function () {
                              return "<p style = background-color:white;margin:0;padding:2px 7px><strong>" +
                              this.point.name +  "</strong>: " +
                              Highcharts.numberFormat(this.y*100, 0) + "%</p>" ;
                              }')
    ) %>%
    hc_exporting_options() %>%
    hc_size(width = 680, height = 400)
}

hc_bar <- function(df) {

  hchart(df, type = "bar", hcaes(x = x, y = y),
         colorByPoint = TRUE) %>%
    hc_add_theme(my_theme) %>%
    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS('function () {
                              return Math.round(this.value*100, 0) + "%";} ')),
             ceiling = 1)  %>%

    hc_xAxis(title = list(text = ""),
             tickLength = 0,
             labels = list(
               align = "left",
               reserveSpace = TRUE,
               style = list(textOverflow = "none")
             )) %>%

    hc_plotOptions(series = list(stacking = "normal",
                                 pointPadding = 0,
                                 dataLabels = list(enabled = TRUE,
                                                   formatter = JS('function () {
                              return Math.round(this.y*100, 0) + "%";} '),
                                                   style = list(#fontSize = 'medium',
                                                     color = SGdarkgrey),
                                                   shadow = FALSE,
                                                   inside = FALSE,
                                                   position = "right")) ) %>%

    hc_tooltip(formatter = JS('function () {
                return "<p style = background-color:white;margin:0;padding:2px 7px><strong> " +
                this.point.name + "</strong>: " +
                              Highcharts.numberFormat(this.y*100, 0) + "%"; }')) %>%

    hc_exporting_options() %>%
    hc_size(width = 680, height = 400)

}

hc_groupedcol <- function(df) {


  hchart(df, type = "column", hcaes(x = x, y = y, group = key)) %>%
    hc_add_theme(my_theme) %>%
    hc_colors(colors = c(SGmix4_cont[3:2], SGmidgrey, SGmix4_cont[1]))  %>%

    hc_yAxis(title = list(text = ""),
             labels = list(format = "£{value:,f}"))  %>%

    hc_xAxis(title = list(text = "Income decile"),
             tickLength = 0)  %>%

    hc_plotOptions(series = list(pointPadding = 0)) %>%

    hc_legend(verticalAlign = "top") %>%
    hc_tooltip(formatter = JS('function () {
                return "<strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": £" +
                          Highcharts.numberFormat(this.y, 0); }')) %>%
    hc_exporting_options() %>%
    hc_size(width = 680, height = 400)

}

hc_stackedbar <- function(df) {

  hchart(df, type = "bar", hcaes(x = x, y = y, group = key)) %>%
    hc_add_theme(my_theme) %>%
    hc_colors(colors = c(SGmidgrey, SGlightgreen, SGbrown, SGorange))  %>%

    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS('function () {
                              return Math.round(this.value*100, 0) + "%";} ')),
             ceiling = 1
    )  %>%
    hc_xAxis(title = list(text = ""),
             tickLength = 0)  %>%

    hc_plotOptions(series = list(stacking = "normal",
                                 pointPadding = 0)) %>%
    hc_legend(verticalAlign = "top",
              reversed = TRUE) %>%

    # background-color: white is required to tooltip is readable against axes
    hc_tooltip(formatter = JS('function () {
                return "<p style = background-color:white;margin:0;padding:2px 7px><strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          Highcharts.numberFormat(this.y*100, 0) + "%"; }')) %>%

    hc_exporting_options() %>%
    hc_size(width = 680, height = 400)

}


hc_stackedcol <- function(df) {

  hchart(df, type = "column", hcaes(x = x, y = y, group = key)) %>%
    hc_add_theme(my_theme) %>%
    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS('function () {
                              return Math.round(this.value*100, 0) + "%";} ')),
             ceiling = 1
    )  %>%
    hc_xAxis(title = list(text = "Income decile"),
             tickLength = 0)  %>%

    hc_plotOptions(series = list(stacking = "normal",
                                 pointPadding = 0)) %>%
    hc_legend(verticalAlign = "middle",
              layout = "vertical",
              align = "right") %>%

    hc_tooltip(# css below is required so that tooltips are still readable when
      # overlapping with titles
      formatter = JS('function () {
                return "<p style = background-color:white;margin:0;padding:2px 7px><strong> " +
                          this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          Highcharts.numberFormat(this.y*100, 0) + "%"; }')) %>%

    hc_exporting_options() %>%
    hc_size(width = 680, height = 400)

}

hc_line <- function(df, minYRange = 0.3, persistent = FALSE, covid = FALSE) {

  remove_markers <- function(hc) {

    # a bit hacky - removes all markers that are not at the end of the line
    for (i in 1:length(hc$x$hc_opts$series)) {

      series_length <- length(hc$x$hc_opts$series[[i]]$data)

      for (j in 2:(series_length - 1)) {

        # keep first & last markers
        if (!is.na(hc$x$hc_opts$series[[i]]$data[[j - 1]]$y) &
            !is.na(hc$x$hc_opts$series[[i]]$data[[j + 1]]$y)) {

          hc$x$hc_opts$series[[i]]$data[[j]]$marker <- list(enabled = FALSE)
        }
      }

      # keep markers in covid break of cmd chart
      if (covid) {
        hc$x$hc_opts$series[[i]]$data[[24]]$marker <- list(enabled = TRUE)
        hc$x$hc_opts$series[[i]]$data[[25]]$marker <- list(enabled = TRUE)
        hc$x$hc_opts$series[[i]]$data[[26]]$marker <- list(enabled = TRUE)
        }
    }
    hc
  }

  add_click_labels <- function(hc) {

    hc %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS('function () {

              var e = this.series.options.dataLabels.enabled

              this.series.update({

                dataLabels: {
                    enabled: !e,
                    /*style: {
                      fontSize: "medium"
                      },*/
                    formatter: function() {

                    var data = this.series.data,
                        index = data.indexOf(this.point);
                      /* add labels for first and last point, and after a break */
                      if([0, (data.length - 1)].includes(this.x) || data[index-1].y === null ) {

                          if (this.y < 100) {
                            return Math.round(this.y*100, 2) + "%";
                          } else {
                            return "£" + Math.round(this.y, 0);
                          }

                        }
                    }
                  }

              });

          }')
            )
          )
        )
      )

  }

  if (persistent) {

  a <- df %>%
    arrange(x) %>%
    hchart('line',
           hcaes(x = 'x', y = 'y', group = "key"))
  } else {
    a <- df %>%
      arrange(x) %>%
      hchart('spline',
             hcaes(x = 'x', y = 'y', group = "key"))

  }

  a %>%
    hc_add_theme(my_theme) %>%
    hc_yAxis(title = list(text = ""),
             labels = list(
               formatter = JS('function () {
                              return Math.round(this.value*100, 0) + "%";} ')),
             minRange = minYRange,
             showLastLabel = FALSE,
             accessibility = list(description = "Rate")) %>%

    hc_xAxis(title = list(text = ""),
             labels = list(
               autoRotation = rep(-32, length(periods) %/% 5)),
             tickLength = 5,
             tickInterval = 5,
             accessibility = list(description = "Time")) %>%

    hc_legend(align = "right",
              layout = "proximate") %>%

    hc_responsive(
      rules = list(

        # for very small screens, remove legend and annotations
        list(
          condition = list(maxWidth = 500),
          chartOptions = list(
            legend = list(enabled = FALSE),
            xAxis = list(plotBands = NULL))
        )
      )
    ) %>%

    hc_tooltip(formatter = JS('function () {
                if (this.y < 100) {
                  return "<strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          Highcharts.numberFormat(this.y*100, 0) + "%";
                } else {
                  return "<strong> " + this.point.name + "</strong><br>" +
                          this.series.name + ": " +
                          "£" + Highcharts.numberFormat(this.y, 0);
                }

                              }')
    ) %>%

    hc_exporting_options() %>%
    remove_markers() %>%
    add_click_labels() %>%
    hc_size(width = 680, height = 400)


}

hc_target <- function(df, targets = c(0.18, 0.10)) {

  df %>%
    arrange(x) %>%
    hchart('scatter',
           hcaes(x = 'x', y = 'y'),
           color = SGdarkblue,

           # data labels for single-year estimates
           dataLabels = list(
             enabled = TRUE,
             formatter = JS('function() {
                        var data = this.series.data,
                        index = data.indexOf(this.point);
                         if([0, 27].includes(this.x) || (data[index-1].y === null && data[index-2].y === null)) {
                         return Highcharts.numberFormat(this.y*100, 0) + "%";
                         }
          }')
           )) %>%
    hc_add_theme(my_theme) %>%
    hc_add_series(type = "spline", data = df, mapping = hcaes(x = 'x',
                                                              y = 'three'),
                  marker = list(enabled = FALSE),
                  enableMouseTracking = FALSE,
                  lineWidth = 8,
                  color = SGmix4_cont[4],
                  zIndex = -1) %>%
    hc_add_series(type = 'scatter',
                  data = data.frame(x = c("2023/24", "2030/31"),
                                    y = targets),
                  mapping = hcaes(x = 'x', y = 'y'),
                  marker = list(radius = 6,
                                fillColor = SGlightorange,
                                lineColor = SGdarkgrey,
                                lineWidth = 3),
                  # data labels for final target (not interim, too crowded)
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS('function() {
                        var data = this.series.data,
                        index = data.indexOf(this.point);

                         if(this.x === 36) {
                         return Highcharts.numberFormat(this.y*100, 0) + "%";
                         }
          }')
                  )) %>%
    hc_size(height = 350) %>%
    hc_yAxis(visible = FALSE,
             min = 0,
             max = 0.55,
             accessibility = list(description = "Rate")) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(
               autoRotation = rep(-32, length(periods) %/% 5)),
             tickInterval = 12,
             tickLength = 5,
             accessibility = list(description = "Time")) %>%

    hc_tooltip(formatter = JS('function () {
               if (this.point.high > 0) {
               return "<strong>" + this.point.name + "</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "% (" +
                       Highcharts.numberFormat(this.point.low*100, 0) + "-" +
                       Highcharts.numberFormat(this.point.high*100, 0) + "%)";
               } else {
              return "<strong>" + this.point.name + "</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "%";
              }}')) %>%

    hc_credits(enabled = TRUE,
               text = "Source: Family Resources Survey",
               href = "download.html") %>%
    hc_exporting_options()
}


# html -------------------------------------------------------------------------

infobox <- function(md) {
  htmltools::div(class = "infobox-text",
                 create_html(md))
}

message <- function(md) {
  tags <- htmltools::tags
  htmltools::div(class = "message-text",
                 tags$strong(create_html(md)))
}

create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md,
      fragment.only = TRUE
    )
  )
}

abbr <- function(short, long) {
  tags <- htmltools::tags
  tags$abbr(title = long,
            short)
}


tables_panel <- function(tables) {

  tags <- htmltools::tags

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
