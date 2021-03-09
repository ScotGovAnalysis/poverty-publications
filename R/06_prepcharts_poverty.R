
# Prep datasets for poverty charts

source("R/00_functions.R")
source("R/00_strings.R")

# Load data --------------------------------------------------------------------
hbai <- readRDS("data/tidyhbai.rds") %>%
  filter(gvtregn == "Scotland")

adult <- readRDS("data/tidyadult.rds") %>%
  filter(gvtregn == "Scotland") %>%
  mutate(yearn = factor(year, levels = labels$years$years,
                       labels = labels$years$numbered),
         yearn = as.numeric(yearn))

povertychartdata <- list()

# Relative poverty -------------------------------------------------------------

pp <- getpovby(hbai, pov = "low60bhc", weight = "gs_newpp")
ch <- getpovby(hbai, pov = "low60bhc", weight = "gs_newch")
wa <- getpovby(hbai, pov = "low60bhc", weight = "gs_newwa")
pn <- getpovby(hbai, pov = "low60bhc", weight = "gs_newpn")

bhc <- rbind(pp, ch, wa, pn) %>%
  group_by(weight) %>%
  get3yrtable() %>%
  mutate(key = "Before housing costs")

pp <- getpovby(hbai, pov = "low60ahc", weight = "gs_newpp")
ch <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch")
wa <- getpovby(hbai, pov = "low60ahc", weight = "gs_newwa")
pn <- getpovby(hbai, pov = "low60ahc", weight = "gs_newpn")

ahc <- rbind(pp, ch, wa, pn) %>%
  group_by(weight) %>%
  get3yrtable() %>%
  mutate(key = "After housing costs")

povertychartdata$relpov <- rbind(bhc, ahc) %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, number, rate, weight, key, type) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    exclude = NA,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")"))

# Absolute poverty -------------------------------------------------------------
pp <- getpovby(hbai, pov = "low60bhcabs", weight = "gs_newpp")
ch <- getpovby(hbai, pov = "low60bhcabs", weight = "gs_newch")
wa <- getpovby(hbai, pov = "low60bhcabs", weight = "gs_newwa")
pn <- getpovby(hbai, pov = "low60bhcabs", weight = "gs_newpn")

bhc <- rbind(pp, ch, wa, pn) %>%
  mutate(key = "Before housing costs")

pp <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newpp")
ch <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newch")
wa <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newwa")
pn <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newpn")

ahc <- rbind(pp, ch, wa, pn) %>%
  mutate(key = "After housing costs")

povertychartdata$abspov <- rbind(bhc, ahc) %>%
  group_by(weight, key) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, number, rate, weight, key, type) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")"))

# In-work poverty --------------------------------------------------------------
# ch <- getpovby(hbai, pov = "low60bhc", weight = "gs_newch", by = "workinghh")
# wa <- getpovby(hbai, pov = "low60bhc", weight = "gs_newwa", by = "workinghh")
#
# bhc <- rbind(ch, wa) %>%
#   mutate(key = "Before housing costs")
#
# ch <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch", by = "workinghh")
# wa <- getpovby(hbai, pov = "low60ahc", weight = "gs_newwa", by = "workinghh")
#
# ahc <- rbind(ch, wa) %>%
#   mutate(key = "After housing costs")
#
# povertychartdata$workpov <- rbind(bhc, ahc) %>%
#   group_by(weight, key, groupingvar) %>%
#   get3yrtable() %>%
#   samplesizecheck() %>%
#   filter(groupingvar == "Someone in paid work") %>%
#   mutate_at(vars(contains("composition")), list(~roundpct(.))) %>%
#   select(yearn, number, composition, weight, key) %>%
#   mutate(x = factor(yearn,
#                     levels = labels$years$numbered,
#                     labels = labels$years$periods,
#                     ordered = TRUE),
#          x = fct_drop(x),
#          y = composition,
#          label = fmtpct(y),
#          tooltip = paste0(key, ": ",  fmtpct(y),
#                           " (", x, ")"))

# In-work poverty --------------------------------------------------------------

ch_all <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch") %>%
  rename(povrate = rate) %>% select(yearn, povrate, weight)
wa_all <- getpovby(hbai, pov = "low60ahc", weight = "gs_newwa") %>%
  rename(povrate = rate) %>% select(yearn, povrate, weight)
ch <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch", by = "workinghh")
wa <- getpovby(hbai, pov = "low60ahc", weight = "gs_newwa", by = "workinghh")

pov <- rbind(ch_all, wa_all)

povertychartdata$workpov <- rbind(ch, wa) %>%
  mutate(key = "After housing costs") %>%
  left_join(pov, by = c("yearn", "weight")) %>%
  group_by(weight, groupingvar) %>%
  arrange(groupingvar, weight) %>%
  get3yrtable() %>%
  filter(groupingvar == "Someone in paid work" | yearn <= 4,
         groupingvar == "All" | yearn >= 5) %>%
  mutate(rate_working = roundpct(composition * povrate),
         composition = roundpct(composition)) %>%
  select(yearn, number, composition, rate_working, povrate, weight, key) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = povrate,
         ymax = ifelse(composition == 1, NA, rate_working),
         label = fmtpct(round2(y, 2)),
         tooltip = paste0(x, "\n In workless households: ", fmtpct(round2(1 - composition, 2)),
                          "\nIn working households: ",  fmtpct(round2(composition, 2))),
         tooltip = ifelse(composition == 1, paste0(x, "\n Work status not available"), tooltip)) %>%
  arrange(weight, yearn)

#View(povertychartdata$workpov)

## food security ---------------------------------------------------------------
relpp <- getpovby(hbai, pov = "low60ahc", weight = "gs_newpp", by = "foodsec") %>%
  filter(yearn >= 26,
         groupingvar != "(Missing)",
         groupingvar != "All") %>%
  mutate(x = "People in relative poverty",
         key = groupingvar,
         y = composition) %>%
  select(number, x, y, key, weight)

abspp <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newpp", by = "foodsec") %>%
  filter(yearn >= 26,
         groupingvar != "(Missing)",
         groupingvar != "All") %>%
  mutate(x = "People in absolute poverty",
         key = groupingvar,
         y = composition) %>%
    select(number, x, y, key, weight)

relch <- getpovby(hbai, pov = "low60ahc", weight = "gs_newch", by = "foodsec") %>%
  filter(yearn >= 26,
         groupingvar != "(Missing)",
         groupingvar != "All") %>%
  mutate(x = "Children in relative poverty",
         key = groupingvar,
         y = composition) %>%
  select(number, x, y, key, weight)

absch <- getpovby(hbai, pov = "low60ahcabs", weight = "gs_newch", by = "foodsec") %>%
  filter(yearn >= 26,
         groupingvar != "(Missing)",
         groupingvar != "All") %>%
  mutate(x = "Children in absolute poverty",
         key = groupingvar,
         y = composition) %>%
  select(number, x, y, key, weight)

totpp <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newpp),
            y = number / max(population),
            x = "All people",
            weight = "gs_newpp") %>%
  ungroup() %>%
  rename(key = foodsec)

totch <- hbai %>%
  filter(foodsec != "(Missing)",
         yearn >= 26,
         gs_newch > 0) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newch),
            y = number / max(population),
            x = "All children",
            weight = "gs_newch") %>%
  ungroup() %>%
  rename(key = foodsec)

povertychartdata$foodsec <- rbind(totpp, totch, relpp, abspp, relch, absch) %>%
  mutate(tooltip = paste0(key, " food security: ",  fmtpct(y)),
         x = factor(x, levels = c("All people",
                                  "People in relative poverty",
                                  "People in absolute poverty",
                                  "All children",
                                  "Children in relative poverty",
                                  "Children in absolute poverty")),
         x = fct_rev(x),
         y = roundpct(y))

# Equality groups --------------------------------------------------------------

## age -------------------------------------------------------------------------
povertychartdata$age <- getpovby(adult, weight = "adultwgt", by = "ageband") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = groupingvar,
         label = case_when(yearn == min(yearn) ~ paste0(key, ": ", fmtpct(y)),
                           yearn == max(yearn) ~ paste0(fmtpct(y), " (", key, ")")),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(key != "All")

## age2 (single adults) --------------------------------------------------------
povertychartdata$age2 <- getpovby(filter(adult, adulth == 1), weight = "adultwgt", by = "ageband") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = groupingvar,
         label = case_when(yearn == min(yearn) ~ paste0(key, ": ", fmtpct(y)),
                           yearn == max(yearn) ~ paste0(fmtpct(y), " (", key, ")")),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(key != "All")

## age3 (bar chart with children) ----------------------------------------------

rel0 <- povertychartdata$age %>% filter(yearn == max(yearn)) %>%
  select(y, number, key, label) %>% rename(x = key, tooltip = label)

rel1 <- getpovby(hbai, weight = "wgt0_4") %>% get3yrtable()
rel2 <- getpovby(hbai, weight = "wgt5_12") %>% get3yrtable()
rel3 <- getpovby(hbai, weight = "wgt13plus") %>% get3yrtable()

povertychartdata$age3 <- rbind(rel1, rel2, rel3) %>%
  filter(yearn == max(yearn)) %>%
  samplesizecheck() %>%
  roundall() %>%
  select(rate, number, weight) %>%
  rename(y = rate, x = weight) %>%
  mutate(x = case_when(x == "wgt0_4" ~ "0-4",
                       x == "wgt5_12" ~ "5-12",
                       x == "wgt13plus" ~ "13-19"),
         tooltip = paste0(fmtpct(round2(y, 2)), " (", x, ")")) %>%
  rbind(rel0) %>%
  mutate(label = fmtpct(round2(y, 2)),
         x = factor(x, levels = as.character(x)),
         number = fmtpop(round2(number, -4)))

## gender ----------------------------------------------------------------------
povertychartdata$gender <- getpovby(filter(hbai, singlehh != "(Missing)"),
                                    weight = "gs_newad", by = "singlehh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = groupingvar,
         key = case_when(key == "Female working-age adult, no dependent children" ~ "Single woman, no children",
                         key == "Male working-age adult, no dependent children" ~ "Single man, no children",
                         key == "Female working-age adult with dependent children" ~ "Single mother",
                         key == "Male pensioner" ~ "Single male pensioner",
                         key == "Female pensioner" ~ "Single female pensioner"),
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(!is.na(y))

povertychartdata$gender_ages <- hbai %>%
  filter(singlehh %in% c("Male pensioner", "Female pensioner"),
         yearn >= max(yearn) - 4) %>%
  mutate(older = ifelse(agehd >= 80, 1, 0)) %>%
  group_by(yearn, sexhd) %>%
  summarise(older = sum(older*gs_newbu),
            all = sum(gs_newbu),
            ratio = older / all,
            sample = n()) %>%
  group_by(sexhd) %>%
  summarise(older = fmtpct(round2(mean(ratio), 2)))

## marital status --------------------------------------------------------------
povertychartdata$marital <- getpovby(adult, weight = "adultwgt",
                                     by = "marital") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = case_when(groupingvar == "Divorced / Civil Partnership dissolved / separated" ~ "Divorced",
                         groupingvar == "Married / Civil Partnership" ~ "Married",
                         TRUE ~ as.character(groupingvar)),
         label = case_when(yearn == max(yearn) ~ paste0(fmtpct(y), " (", key, ")"),
                           TRUE ~ paste0(key, ": ", fmtpct(y))),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(key != "All")

## ethnic group ----------------------------------------------------------------
povertychartdata$ethnic <- getpovby(hbai, by = "ethgrphh") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  filter(yearn == max(yearn)) %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(groupingvar, levels = c("All",
                                            "White - British",
                                            "White - Other",
                                            "Asian or Asian British",
                                            "Mixed, Black or Black British, and Other"),
                    ordered = TRUE),
         x = fct_rev(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(x, ": ",  fmtpct(y))) %>%
  arrange(x)

povertychartdata$ethnic_ages <- hbai %>%
  filter(yearn >= max(yearn) - 4,
         gvtregn == "Scotland") %>%
  group_by(yearn, ethgrphh) %>%
  summarise(agehd_median = wtd.quantile(agehd, probs = 0.5, weights = gs_newbu),
            sample = n()) %>%
  arrange(ethgrphh) %>%
  ungroup() %>%
  group_by(ethgrphh) %>%
  summarise(age = round2(mean(agehd_median), 0),
            sample = sum(sample))

## religion --------------------------------------------------------------------
povertychartdata$religion <- getpovby(adult, weight = "adultwgt",
                                      by = "religsc") %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  filter(yearn == max(yearn)) %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(groupingvar, levels = c("All",
                                            "Church of Scotland",
                                            "Roman Catholic",
                                            "Other Christian",
                                            "Muslim",
                                            "Other religion",
                                            "No religion"),
                    ordered = TRUE),
         x = fct_rev(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(x, ": ",  fmtpct(y))) %>%
  arrange(x)

povertychartdata$religion_ages <- adult %>%
  filter(yearn >= max(yearn) - 4,
         gvtregn == "Scotland") %>%
  group_by(yearn, religsc) %>%
  summarise(age_median = wtd.quantile(age, probs = 0.5, weights = adultwgt),
            sample = n()) %>%
  arrange(religsc) %>%
  ungroup() %>%
  group_by(religsc) %>%
  summarise(age = round2(mean(age_median), 0),
            sample = sum(sample))

## disability ------------------------------------------------------------------
povertychartdata$disability <- getpovby(hbai, by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = groupingvar,
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(key != "All")

povertychartdata$disability2 <- getpovby(hbai, pov = "low60ahc_dis",
                                         by = "dispp_hh") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  mutate_at(vars(contains("rate")), list(~roundpct(.))) %>%
  select(yearn, groupingvar, number, rate, composition) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         key = groupingvar,
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")) %>%
  filter(key != "All")

# Material deprivation ---------------------------------------------------------

## children --------------------------------------------------------------------
bhc_new <- getpovby(hbai, pov = "cmdbhc_new", weight = "gs_newch")
bhc <- getpovby(hbai, pov = "cmdbhc", weight = "gs_newch") %>%
  rbind(bhc_new) %>%
  mutate(key = "Before housing costs")

ahc_new <- getpovby(hbai, pov = "cmdahc_new", weight = "gs_newch")
ahc <- getpovby(hbai, pov = "cmdahc", weight = "gs_newch") %>%
  rbind(ahc_new) %>%
  mutate(key = "After housing costs")

povertychartdata$cmd <- rbind(ahc, bhc) %>%
  group_by(key) %>%
  arrange(key, yearn) %>%
  get3yrtable() %>%
  mutate(number = ifelse(yearn == 18, NA, number),
         rate = ifelse(yearn == 18, NA, rate)) %>%
  filter(type != "cmdahc_new",
         type != "cmdbhc_new") %>%
  samplesizecheck() %>%
  roundall() %>%
  select(yearn, number, rate, weight, key, type) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         label = fmtpct(y),
         tooltip = ifelse(is.na(y), "Missing",
                          paste0(key, ": ",  fmtpct(y),
                          " (", x, ")")))

## pensioners ------------------------------------------------------------------
povertychartdata$pndep <- getpovby(hbai, pov = "mdpn", weight = "wgt65") %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  select(yearn, number, rate, weight) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(fmtpct(y),
                          " (", x, ")"))

# Priority groups --------------------------------------------------------------
rel <- povertychartdata$relpov %>%
  filter(weight == "gs_newch",
         key == "After housing costs",
         yearn == max(yearn)) %>%
  mutate(groupingvar = "All children") %>%
  select(groupingvar, rate, type)

abs <- povertychartdata$abspov %>%
  filter(weight == "gs_newch",
         key == "After housing costs",
         yearn == max(yearn)) %>%
  mutate(groupingvar = "All children") %>%
  select(groupingvar, rate, type)

cmd <- povertychartdata$cmd %>%
  filter(weight == "gs_newch",
         key == "After housing costs",
         yearn == max(yearn)) %>%
  mutate(groupingvar = "All children") %>%
  select(groupingvar, rate, type)

rel1 <- getpriorityrate(hbai, pov = "low60ahc", by = "depchldh_ch",
                        class = "3 or more children")
abs1 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "depchldh_ch",
                        class = "3 or more children")
cmd1 <- getpriorityrate(hbai, pov = "cmdahc", by = "depchldh_ch",
                        class = "3 or more children")
rel2 <- getpriorityrate(hbai, pov = "low60ahc", by = "babyhh",
                        class = "Youngest child is younger than 1")
abs2 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "babyhh",
                        class = "Youngest child is younger than 1")
cmd2 <- getpriorityrate(hbai, pov = "cmdahc", by = "babyhh",
                        class = "Youngest child is younger than 1")
rel3 <- getpriorityrate(hbai, pov = "low60ahc", by = "youngmumhh",
                        class = "Mother under 25 in household")
abs3 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "youngmumhh",
                        class = "Mother under 25 in household")
cmd3 <- getpriorityrate(hbai, pov = "cmdahc", by = "youngmumhh",
                        class = "Mother under 25 in household")
rel4 <- getpriorityrate(hbai, pov = "low60ahc", by = "loneparenthh",
                        class = "Single parent in household")
abs4 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "loneparenthh",
                        class = "Single parent in household")
cmd4 <- getpriorityrate(hbai, pov = "cmdahc", by = "loneparenthh",
                        class = "Single parent in household")
rel5 <- getpriorityrate(hbai, pov = "low60ahc", by = "dispp_hh",
                        class = "In household with disabled person(s)")
abs5 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "dispp_hh",
                        class = "In household with disabled person(s)")
cmd5 <- getpriorityrate(hbai, pov = "cmdahc", by = "dispp_hh",
                        class = "In household with disabled person(s)")
rel6 <- getpriorityrate(hbai, pov = "low60ahc", by = "ethgrphh_2f",
                        class = "Minority ethnic")
abs6 <- getpriorityrate(hbai, pov = "low60ahcabs", by = "ethgrphh_2f",
                        class = "Minority ethnic")
cmd6 <- getpriorityrate(hbai, pov = "cmdahc", by = "ethgrphh_2f",
                        class = "Minority ethnic")

povertychartdata$priority <- rbind(rel, rel1, rel2, rel3, rel4, rel5, rel6,
                                        abs, abs1, abs2, abs3, abs4, abs5, abs6,
                                        cmd, cmd1, cmd2, cmd3, cmd4, cmd5, cmd6) %>%
  mutate(x = factor(groupingvar,
                    levels = c("All children",
                               "In household with disabled person(s)",
                               "3 or more children",
                               "Youngest child is younger than 1",
                               "Minority ethnic",
                               "Single parent in household"),
                    labels = c("All children",
                               "Disabled household member(s)",
                               "3 or more children in the household",
                               "Youngest child in the household is under 1",
                               "Minority ethnic household",
                               "Single parent in the household"),
                    ordered = TRUE),
         x = fct_rev(x),
         y = rate,
         label = fmtpct(y),
         tooltip = paste0(x, ": ",  fmtpct(y))) %>%
  filter(!is.na(y)) %>%
  arrange(type, x)

# Income -----------------------------------------------------------------------

## medians ---------------------------------------------------------------------
bhc <- hbai %>%
  group_by(yearn) %>%
  getmediansbhc() %>%
  mutate_at(vars(c(pp, ch, wa, pn)), get3yraverage) %>%
  mutate(key = "Before housing costs")

ahc <- hbai %>%
  group_by(yearn) %>%
  getmediansahc() %>%
  mutate_at(vars(c(pp, ch, wa, pn)), get3yraverage) %>%
  mutate(key = "After housing costs")

povertychartdata$medians <- rbind(bhc, ahc) %>%
  filter(yearn >= 3) %>%
  select(yearn, pp, key) %>%
  mutate(pp = round2(pp, 2)) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = pp,
         label = stringi::stri_enc_toutf8(paste0("£", round2(y, 0))),
         tooltip = stringi::stri_enc_toutf8(paste0(key, ": £", round2(y, 0),
                          " (", x, ")")))

## decile points ---------------------------------------------------------------
povertychartdata$deciles <- hbai %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  gather(x, value, -yearn) %>%
  filter(x != "10") %>%
  group_by(x) %>%
  mutate(value = round2(value, 2)) %>%
  filter(yearn >= 22) %>%
  mutate(key = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$formatted,
                    ordered = TRUE),
         key = fct_drop(key),
         y = round2(value, 2),
         tooltip = stringi::stri_enc_toutf8(paste0("Decile ", x, ": ",  paste0("£", round2(y, 0)),
                          " (", key, ")")))

## distribution ----------------------------------------------------------------
povertychartdata$distribution <- hbai %>%
  group_by(yearn) %>%
  mutate(income = s_oe_bhc * bhcpubdef / bhcyrdef) %>%
  select(yearn, gs_newpp, income) %>%
  ungroup() %>%
  filter(yearn >= max(yearn) - 2)

povertychartdata$distdecs <- hbai %>%
  group_by(yearn) %>%
  getdecptsbhc() %>%
  gather(x, value, -yearn) %>%
  group_by(x) %>%
  mutate(value = get3yraverage(value)) %>%
  ungroup() %>%
  filter(yearn >= 26) %>%
  mutate(xpos = lag(value) + 1/2*(value - lag(value)),
         xpos = ifelse(x == "1", value/2 + 50, xpos),
         xpos = ifelse(x == "10", (lag(value) + 50), xpos))

povertychartdata$distthresh <- hbai %>%
  filter(yearn >= max(yearn) - 2) %>%
  group_by(yearn) %>%
  summarise(UKmedian = max(mdoebhc * bhcpubdef / bhcyrdef),
            Scotmedian = wtd.quantile(s_oe_bhc * bhcpubdef / bhcyrdef,
                                      probs = 0.5, weights = gs_newpp),
            povthresh = 0.6 * UKmedian) %>%
  ungroup() %>%
  summarise(UKmedian = mean(UKmedian),
            Scotmedian = mean(Scotmedian),
            povthresh = mean(povthresh))

## sources ---------------------------------------------------------------------
df1 <- getsources(filter(hbai, yearn == max(yearn)))
df2 <- getsources(filter(hbai, yearn == max(yearn) - 1))
df3 <- getsources(filter(hbai, yearn == max(yearn) - 2))

df <- data.frame(df1[1])
df[2] <- (df1[2] + df2[2] + df3[2])/3
df[3] <- (df1[3] + df2[3] + df3[3])/3
df[4] <- (df1[4] + df2[4] + df3[4])/3
df[5] <- (df1[5] + df2[5] + df3[5])/3
df[6] <- (df1[6] + df2[6] + df3[6])/3

povertychartdata$sources <- df %>%
  mutate_if(is.numeric, list(~roundpct(.))) %>%
  gather(key, value, -Decile) %>%
  filter(Decile != "All") %>%
  mutate(x = Decile,
         y = value,
         tooltip = paste0(key, ": ", fmtpct(y)))

## palma -----------------------------------------------------------------------
palma_bhc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesbhc() %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = get3yraverage(Palma),
         Palma = roundpct(Palma),
         key = "Before housing costs")

palma_ahc <- hbai %>%
  group_by(yearn) %>%
  getdecsharesahc() %>%
  mutate(Palma = share[10] / sum(share[1:4])) %>%
  group_by(Decile) %>%
  mutate(Palma = get3yraverage(Palma),
         Palma = roundpct(Palma),
         key = "After housing costs")

povertychartdata$palma <- rbind(palma_bhc, palma_ahc) %>%
  ungroup() %>%
  filter(Decile == 10,
         yearn >= 3) %>%
  mutate(Palma = roundpct(Palma)) %>%
  select(yearn, Palma, key) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = Palma,
         label = fmtpct(y),
         tooltip = paste0(fmtpct(y), " (", x, ")"))

## gini ------------------------------------------------------------------------
gini_bhc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         key = "Before housing costs")

gini_ahc <- hbai %>%
  group_by(yearn) %>%
  summarise(Gini = gini(s_oe_ahc, weights = gs_newpp)) %>%
  mutate(Gini = get3yraverage(Gini),
         Gini = roundpct(Gini),
         key = "After housing costs")

povertychartdata$gini <- rbind(gini_bhc, gini_ahc) %>%
  mutate(Gini = roundpct(Gini)) %>%
  filter(yearn >= 3) %>%
  select(yearn, Gini, key) %>%
  mutate(x = factor(yearn,
                    levels = labels$years$numbered,
                    labels = labels$years$periods,
                    ordered = TRUE),
         x = fct_drop(x),
         y = Gini,
         label = fmtpct(y),
         tooltip = paste0(key, ": ",  fmtpct(y),
                          " (", x, ")"))

saveRDS(povertychartdata, "data/povertychartdata.rds")

rm(list = ls())
