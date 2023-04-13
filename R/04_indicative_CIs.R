# load ----------------------------------------------------------------------

library(tidyverse)
library(survey)
source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")
sample_design_identifiers <- readRDS("data/sample_design_identifiers.rds")

periods <- levels(labels$years$periods)
years <- levels(labels$years$formatted)

# Sampling method: stratified clustered sample

if (file.exists("data/indicative_CIs.rds")) {
  ind_CIs <- readRDS("data/indicative_CIs.rds")
} else {ind_CIs <- list()}

# prepare datasets -------------------------------------------------------------

hbai <- hbai  %>%
  filter(yearn >= 4) %>%
  left_join(sample_design_identifiers, by = c("yearn", "sernum")) %>%
  filter(gvtregn == "Scotland")

adult <- adult %>%
  filter(yearn >= 4) %>%
  left_join(sample_design_identifiers,
            by = c("yearn", "sernum")) %>%
  filter(gvtregn == "Scotland")

# pool lists -------------------------------------------------------------------
# add period variable for 3-year / 5-year data pooling

x1 <- list("1997/98" = 4,
           "1998/99" = 5,
           "1999/00" = 6,
           "2000/01" = 7,
           "2001/02" = 8,

           "2002/03" = 9,
           "2003/04" = 10,
           "2004/05" = 11,
           "2005/06" = 12,
           "2006/07" = 13,

           "2007/08" = 14,
           "2008/09" = 15,
           "2009/10" = 16,
           "2010/11" = 17,
           "2011/12" = 18,

           "2012/13" = 19,
           "2013/14" = 20,
           "2014/15" = 21,
           "2015/16" = 22,
           "2016/17" = 23,

           "2017/18" = 24,
           "2018/19" = 25,
           "2019/20" = 26,
           # do not include year 27
           "2021/22" = 28
           )

x3 <- list("1997-00" = c(4,5,6),
           "1998-01" = c(5,6,7),
           "1999-02" = c(6,7,8),
           "2000-03" = c(7,8,9),
           "2001-04" = c(8,9,10),

           "2002-05" = c(9,10,11),
           "2003-06" = c(10,11,12),
           "2004-07" = c(11,12,13),
           "2005-08" = c(12,13,14),
           "2006-09" = c(13,14,15),

           "2007-10" = c(14,15,16),
           "2008-11" = c(15,16,17),
           "2009-12" = c(16,17,18),
           "2010-13" = c(17,18,19),
           "2011-14" = c(18,19,20),

           "2012-15" = c(19,20,21),
           "2013-16" = c(20,21,22),
           "2014-17" = c(21,22,23),
           "2015-18" = c(22,23,24),
           "2016-19" = c(23,24,25),
           "2017-20" = c(24,25,26),
           "2018-21" = c(25,26,27),
           "2019-22" = c(26,27,28))

x5 <- list("1997-02" = c(4,5,6,7,8),
          "1998-03" = c(5,6,7,8,9),
          "1999-04" = c(6,7,8,9,10),
          "2000-05" = c(7,8,9,10,11),
          "2001-06" = c(8,9,10,11,12),

          "2002-07" = c(9,10,11,12,13),
          "2003-08" = c(10,11,12,13,14),
          "2004-09" = c(11,12,13,14,15),
          "2005-10" = c(12,13,14,15,16),
          "2006-11" = c(13,14,15,16,17),

          "2007-12" = c(14,15,16,17,18),
          "2008-13" = c(15,16,17,18,19),
          "2009-14" = c(16,17,18,19,20),
          "2010-15" = c(17,18,19,20,21),
          "2011-16" = c(18,19,20,21,22),

          "2012-17" = c(19,20,21,22,23),
          "2013-18" = c(20,21,22,23,24),
          "2014-19" = c(21,22,23,24,25),
          "2015-20" = c(22,23,24,25,26),
          "2016-21" = c(23,24,25,26,27),

          "2017-22" = c(24,25,26,27,28))

# get CIs ----------------------------------------------------------------------
# * -- POVERTY -- --------------------------------------------------------------

#options(survey.lonely.psu = "remove")
options(survey.lonely.psu = "adjust")

# * pp rel ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahc, wgt = ~gs_newpp, by = ~gvtregn,
                        df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhc, wgt = ~gs_newpp, by = ~gvtregn,
                        df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$pp_rel <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * pp abs ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahcabs, wgt = ~gs_newpp, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhcabs, wgt = ~gs_newpp, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$pp_abs <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * wa rel ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahc, wgt = ~gs_newwa, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhc, wgt = ~gs_newwa, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$wa_rel <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * wa abs ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahcabs, wgt = ~gs_newwa, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhcabs, wgt = ~gs_newwa, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$wa_abs <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * pn rel ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahc, wgt = ~gs_newpn, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhc, wgt = ~gs_newpn, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$pn_rel <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * pn abs ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahcabs, wgt = ~gs_newpn, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhcabs, wgt = ~gs_newpn, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$pn_abs <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * pn dep ---------------------------------------------------------------------

ind_CIs$pn_dep <- get_SE_by(var = ~mdpn, wgt = ~wgt65,
                  by = ~gvtregn,
                  df = hbai %>% filter(wgt65 > 0),
                  poollist = x3[13:length(x3)]) %>%
  mutate(key = "Pensioners",
         halfCI = estimate - ci_l)


# * -- CHILD POVERTY -- --------------------------------------------------------

# * ch rel ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahc, wgt = ~gs_newch, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhc, wgt = ~gs_newch, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$ch_rel <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * ch abs ---------------------------------------------------------------------

ahc <- get_SE_by(var = ~low60ahcabs, wgt = ~gs_newch, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "After housing costs")

bhc <- get_SE_by(var = ~low60bhcabs, wgt = ~gs_newch, by = ~gvtregn,
                 df = hbai, poollist = x3) %>%
  mutate(key = "Before housing costs")

data <- rbind(ahc, bhc) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$ch_abs <- rbind(data,

                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * ch dep ---------------------------------------------------------------------

ahc_new <- get_SE_by(var = ~cmdahc, wgt = ~gs_newch, by = ~gvtregn,
                     df = hbai %>%
                       filter(gs_newch > 0) %>%
                       mutate(cmdahc = ifelse(is.na(cmdahc_new), cmdahc, cmdahc_new)),
                     poollist = x3[14:length(x3)]) %>%
  mutate(key = "After housing costs",
         Measure = "New measure, after housing costs")

ahc_old <- get_SE_by(var = ~cmdahc, wgt = ~gs_newch, by = ~gvtregn,
                     df = filter(hbai, gs_newch > 0), poollist = x3[8:12]) %>%
  mutate(key = "After housing costs",
         Measure = "Old measure, after housing costs")

bhc_new <- get_SE_by(var = ~cmdbhc, wgt = ~gs_newch, by = ~gvtregn,
                     df = hbai %>%
                       filter(gs_newch > 0) %>%
                       mutate(cmdbhc = ifelse(is.na(cmdbhc_new), cmdbhc, cmdbhc_new)),
                     poollist = x3[14:length(x3)]) %>%
  mutate(key = "Before housing costs",
         Measure = "New measure, before housing costs")

bhc_old <- get_SE_by(var = ~cmdbhc, wgt = ~gs_newch, by = ~gvtregn,
                     df = filter(hbai, gs_newch > 0), poollist = x3[8:12]) %>%
  mutate(key = "Before housing costs",
         Measure = "Old measure, before housing costs")

ind_CIs$ch_dep <- rbind(ahc_old, ahc_new, bhc_old, bhc_new) %>%
  mutate(halfCI = estimate - ci_l)

# * -- CP UPDATES -- -----------------------------------------------------------

# * rel ------------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc, wgt = ~gs_newch, by = ~gvtregn,
                 df = hbai, poollist = x1) %>%
  mutate(halfCI = estimate - ci_l) %>%
  select(-key)

ind_CIs$rel_ch_1yr <- rbind(data,
                            # missing psu and region info for first 3 years of data
                            # assuming same CI as in 4th year
                            data.frame(period = periods[1:3],
                                       estimate = NA,
                                       se = NA,
                                       ci_l = NA,
                                       ci_u = NA,
                                       halfCI = rep(
                                         data %>%
                                           filter(period == years[4]) %>%
                                           pull(halfCI), 3))) %>%
  arrange(period)

# * abs ------------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahcabs, wgt = ~gs_newch, by = ~gvtregn,
                  df = hbai, poollist = x1) %>%
  mutate(halfCI = estimate - ci_l) %>%
  select(-key)

ind_CIs$abs_ch_1yr <- rbind(data,
                            # missing psu and region info for first 3 years of data
                            # assuming same CI as in 4th year
                            data.frame(period = periods[1:3],
                                       estimate = NA,
                                       se = NA,
                                       ci_l = NA,
                                       ci_u = NA,
                                       halfCI = rep(
                                         data %>%
                                           filter(period == years[4]) %>%
                                           pull(halfCI), 3))) %>%
  arrange(period)

# * dep ------------------------------------------------------------------------

data1 <- get_SE_by(var = ~low60ahc,
                   wgt = ~gs_newch,
                   by = ~gvtregn,
                   df = hbai %>%
                     filter(gs_newch > 0),
                   poollist = x1[8:14])

data2 <- get_SE_by(var = ~low60ahc,
                   wgt = ~gs_newch,
                   by = ~gvtregn,
                   df = hbai %>%
                     filter(gs_newch > 0),
                   # calculating SE for 2010/11 based on old methodology only,
                   # because SEs are the same for both methods
                   poollist = x1[15:length(x1)])


ind_CIs$dep_ch_1yr <- rbind(data1, data2) %>%
  mutate(halfCI = estimate - ci_l) %>%
  select(-key) %>%
  arrange(period)

# * -- EQUALITY -- -------------------------------------------------------------

# * pp age ---------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc, wgt = ~gs_newpp, by = ~agehdband8,
                  df = hbai, poollist = x3) %>%
  mutate(halfCI = estimate - ci_l)

ind_CIs$age <- rbind(data,
                     # missing psu and region info for first 3 years of data
                     # assuming same CI as in 4th year
                     data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                key = c(rep(unique(data$key), 3)),
                                estimate = NA,
                                se = NA,
                                ci_l = NA,
                                ci_u = NA,
                                halfCI = rep(
                                  data %>%
                                    filter(period == periods[4]) %>%
                                    pull(halfCI), 3))) %>%
  arrange(key, period)

# * ad gender ------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc, wgt = ~gs_newad, by = ~singlehh,
                  df = hbai %>%
                    filter(singlehh != "(Missing)",
                           singlehh != "Male working-age adult with dependent children") %>%
                    mutate(singlehh = fct_drop(singlehh),
                           singlehh = as.character(singlehh)),
                  poollist = x3) %>%
  mutate(halfCI = estimate - ci_l)


ind_CIs$gender <- rbind(data,
                        # missing psu and region info for first 3 years of data
                        # assuming same CI as in 4th year
                        data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                   key = c(rep(unique(data$key), 3)),
                                   estimate = NA,
                                   se = NA,
                                   ci_l = NA,
                                   ci_u = NA,
                                   halfCI = rep(
                                     data %>%
                                       filter(period == periods[4]) %>%
                                       pull(halfCI), 3))) %>%
  arrange(key, period)

# * ad sexid -------------------------------------------------------------------

ind_CIs$sexid <- get_SE_by(var = ~low60ahc, wgt = ~adultwgt, by = ~sidqn,
                           df = adult %>% mutate(sidqn = fct_lump_n(sidqn, 2)),
                           poollist = x3[15:length(x3)]) %>%
  mutate(halfCI = estimate - ci_l,
         key = as.character(key),
         key = case_when(key == "Other" ~ "LGB+",
                         TRUE ~ key),
         key = factor(key,
                      levels = c("LGB+",
                                 "Heterosexual / straight",
                                 "(Missing)"))) %>%
  arrange(key, period)

# * ad marital -----------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc, wgt = ~adultwgt, by = ~marital,
                           df = adult %>% mutate(marital = as.character(marital)),
                           poollist = x3) %>%
  mutate(halfCI = estimate - ci_l) %>%
  arrange(key, period)

ind_CIs$marital <- rbind(data,
                         # missing psu and region info for first 3 years of data
                         # assuming same CI as in 4th year
                         data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                    key = c(rep(unique(data$key), 3)),
                                    estimate = NA,
                                    se = NA,
                                    ci_l = NA,
                                    ci_u = NA,
                                    halfCI = rep(
                                      data %>%
                                        filter(period == periods[4]) %>%
                                        pull(halfCI), 3))) %>%
  arrange(key, period)

# * pp ethnic ------------------------------------------------------------------

data <- hbai %>%
  filter(yearn >= 8,
         ethgrphh != "(Missing)") %>%
  mutate(ethgrphh = fct_drop(ethgrphh),
         ethgrphh = as.character(ethgrphh))

ind_CIs$ethnic <- get_SE_by(var = ~low60ahc, wgt = ~gs_newpp, by = ~ethgrphh,
                                df = data, poollist = x5[5:length(x5)]) %>%
  mutate(halfCI = estimate - ci_l)

# * ad religion ----------------------------------------------------------------

data <- adult %>% filter(yearn >= 18,
                         religsc != "(Missing)") %>%
  mutate(religsc = fct_drop(religsc),
         religsc = as.character(religsc))

ind_CIs$religion <- get_SE_by(var = ~low60ahc, wgt = ~adultwgt, by = ~religsc,
                              df = data, poollist = x5[15:length(x5)]) %>%
  mutate(halfCI = estimate - ci_l)

# * pp dis1 --------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc, wgt = ~gs_newpp, by = ~dispp_hh,
                  df = hbai %>%
                    filter(dispp_hh != "(Missing)") %>%
                    mutate(dispp_hh = fct_drop(dispp_hh),
                           dispp_hh = as.character(dispp_hh)),
                  poollist = x3) %>%
  mutate(halfCI = estimate - ci_l,
         key = case_when(key == "In household with disabled person(s)"
                         ~ "Someone disabled",
                         key == "In household with no disabled person(s)"
                         ~ "No-one disabled"))

ind_CIs$dis1 <- rbind(data,
                      # missing psu and region info for first 3 years of data
                      # assuming same CI as in 4th year
                      data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                 key = c(rep(unique(data$key), 3)),
                                 estimate = NA,
                                 se = NA,
                                 ci_l = NA,
                                 ci_u = NA,
                                 halfCI = rep(
                                   data %>%
                                     filter(period == periods[4]) %>%
                                     pull(halfCI), 3))) %>%
  arrange(key, period)

# * pp dis2 --------------------------------------------------------------------

data <- get_SE_by(var = ~low60ahc_dis, wgt = ~gs_newpp, by = ~dispp_hh,
                  df = hbai %>%
                    filter(dispp_hh != "(Missing)") %>%
                    mutate(dispp_hh = fct_drop(dispp_hh),
                           dispp_hh = as.character(dispp_hh)),
                  poollist = x3) %>%
  mutate(halfCI = estimate - ci_l,
         key = case_when(key == "In household with disabled person(s)"
                         ~ "Someone disabled",
                         key == "In household with no disabled person(s)"
                         ~ "No-one disabled"))

ind_CIs$dis2 <- rbind(data,
                      # missing psu and region info for first 3 years of data
                      # assuming same CI as in 4th year
                      data.frame(period = sort(c(rep(periods[1:3], length(unique(data$key))))),
                                 key = c(rep(unique(data$key), 3)),
                                 estimate = NA,
                                 se = NA,
                                 ci_l = NA,
                                 ci_u = NA,
                                 halfCI = rep(
                                   data %>%
                                     filter(period == periods[4]) %>%
                                     pull(halfCI), 3))) %>%
  arrange(key, period)

# apply correction -------------------------------------------------------------

# this factor widens the confidence bands to mitigate that the method above
# understates uncertainty
# chose a factor of 1.5 for 2-, 3- and 5-year averages, and 1.7 for single year
# estimates

for (i in c("pp_rel", "pp_abs", "wa_rel", "wa_abs", "pn_rel", "pn_abs", "pn_dep",
            "ch_rel", "ch_abs", "ch_dep", "age", "gender", "marital", "religion",
            "dis1", "dis2", "sexid", "ethnic")) {
  ind_CIs[[i]]$halfCI_widened <- ind_CIs[[i]]$halfCI * 1.5
}

for (i in c("rel_ch_1yr", "abs_ch_1yr", "dep_ch_1yr")) {
  ind_CIs[[i]]$halfCI_widened <- round2(ind_CIs[[i]]$halfCI * 1.7, 7)
}

# drop extra variables
ind_CIs <- lapply(ind_CIs, function(x) x %>% select(-estimate, -se, -halfCI,
                                                    -ci_l, -ci_u))


# save all ---------------------------------------------------------------------
saveRDS(ind_CIs, "data/indicative_CIs.rds")
rm(list = ls())

cat("Indicative confidence intervals created", fill = TRUE)

