
# Prep datasets for poverty charts

# TO DO: food poverty

source("R/00_functions.R")
source("R/00_strings.R")

# Load data ----
hbai <- readRDS("data/tidyhbai.rds")
adult <- readRDS("data/tidyadult.rds")

periods <- labels[["years"]]$periods
yearsno <- length(periods)

povertychartdata <- list()

# Relative poverty ----
relpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60bhc")) %>%
  addyearvar() %>%
  get3yrtable() %>%
  mutate(key = "Before housing costs")

relpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60ahc")) %>%
  addyearvar() %>%
  get3yrtable() %>%
  mutate(key = "After housing costs")

povertychartdata[["relpov"]] <- rbind(relpovbhc, relpovahc)

remove(relpovahc, relpovbhc)

# Absolute poverty ----
abspovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovbhc")) %>%
  addyearvar() %>%
  get3yrtable() %>%
  mutate(key = "Before housing costs")

abspovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovahc")) %>%
  addyearvar() %>%
  get3yrtable() %>%
  mutate(key = "After housing costs")

povertychartdata[["abspov"]] <- rbind(abspovbhc, abspovahc)
remove(abspovbhc, abspovahc)

# In-work poverty ----
workbhc <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60bhc",
                      groupingvar = "workinghh")) %>%
  addyearvar() %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-8L) %>%
  mutate(key = "Before housing costs")

workahc <- do.call(rbind.data.frame,
                   lapply(hbai, getpovby, povvar = "low60ahc",
                          groupingvar = "workinghh")) %>%
  addyearvar() %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-8L) %>%
  mutate(key = "After housing costs")

povertychartdata[["workpov"]] <- rbind(workbhc, workahc)

remove(workbhc, workahc)

# Equality groups ----

## age ----
povertychartdata[["age"]] <- do.call(rbind.data.frame,
                                     lapply(adult,
                                            getpovby_adult,
                                            povvar = "low60ahc",
                                            groupingvar = "ageband")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-12L) %>%
  samplesizecheck_ad_num()

## gender ----
povertychartdata[["gender"]] <- do.call(rbind.data.frame,
               lapply(hbai, getpovby, povvar = "low60ahc",
                      groupingvar = "newfambu")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-16L) %>%
  samplesizecheck_num()

## marital status ----
povertychartdata[["marital"]] <- do.call(rbind.data.frame,
                   lapply(adult, getpovby_adult, povvar = "low60ahc",
                          groupingvar = "marital")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-10) %>%
  samplesizecheck_ad_num()

## ethnic group ----
povertychartdata[["ethnic"]] <- do.call(rbind.data.frame,
                     lapply(hbai, getpovby, povvar = "low60ahc",
                            groupingvar = "ethgrphh")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  tail(5L)  %>%
  samplesizecheck_num()

## religion ----
povertychartdata[["religion"]] <- do.call(rbind.data.frame,
                    lapply(adult, getpovby_adult, povvar = "low60ahc",
                           groupingvar = "religsc")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get5yrtable() %>%
  tail(7L)  %>%
  samplesizecheck_ad_num()

## disability ----
povertychartdata[["disability"]] <- do.call(rbind.data.frame,
                      lapply(hbai, getpovby, povvar = "low60ahc",
                             groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  tail(-4L) %>%
  samplesizecheck_num

povertychartdata[["disability2"]] <- do.call(rbind.data.frame,
                  lapply(hbai, getpovby, povvar = "low60ahc_dis",
                         groupingvar = "dispp_hh")) %>%
  addyearvar %>%
  group_by(groupingvar) %>%
  get3yrtable %>%
  tail(-4L) %>%
  samplesizecheck_num

# Material deprivation ----

## children ----
cmdbhc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdbhc_new")) %>%
  addyearvar() %>%
  rename(chnum_new = chnum,
         chrate_new = chrate)

cmdbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdbhc")) %>% addyearvar() %>%
  left_join(cmdbhc_new, by = "years") %>%
  select(years, chnum, chrate, chnum_new, chrate_new) %>%
  mutate(chnum = ifelse(years %in% years[11:17], get3yraverage(chnum),
                        ifelse(years == 1112, NA,
                               ifelse(years == 1213, (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                                      get3yraverage(chnum)))),
         chrate = ifelse(years %in% years[11:17], get3yraverage(chrate),
                         ifelse(years == 1112, NA,
                                ifelse(years == 1213, (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                                       get3yraverage(chrate))))) %>%
  tail(-2L) %>%
  select(1:3) %>%
  mutate(years = factor(years, labels = labels[["years"]]$periods[13:length(labels[["years"]]$periods)]),
         key = "Before housing costs")

cmdahc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc_new")) %>%
  addyearvar() %>%
  rename(chnum_new = chnum,
         chrate_new = chrate)

cmdahc <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc")) %>% addyearvar() %>%
  left_join(cmdahc_new, by = "years") %>%
  select(years, chnum, chrate, chnum_new, chrate_new) %>%
  mutate(chnum = ifelse(years %in% years[11:17], get3yraverage(chnum),
                        ifelse(years == 1112, NA,
                               ifelse(years == 1213, (chnum + lag(chnum, 1L) + lag(chnum_new, 2L))/3,
                                      get3yraverage(chnum)))),
         chrate = ifelse(years %in% years[11:17], get3yraverage(chrate),
                         ifelse(years == 1112, NA,
                                ifelse(years == 1213, (chrate + lag(chrate, 1L) + lag(chrate_new, 2L))/3,
                                       get3yraverage(chrate))))) %>%
  tail(-2L) %>%
  select(1:3) %>%
  mutate(years = factor(years,
                        labels = labels[["years"]]$periods[13:length(labels[["years"]]$periods)]),
         key = "After housing costs")

povertychartdata[["cmd"]] <- rbind(cmdahc, cmdbhc)

remove(cmdahc_new, cmdbhc_new, cmdahc, cmdbhc)

## pensioners ----
povertychartdata[["pndep"]] <- do.call(rbind.data.frame,
                                       lapply(hbai, getpov, povvar = "mdpn")) %>%
  addyearvar() %>%
  get3yrtable()

# Priority groups ----
rel <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "low60ahc")) %>%
  addyearvar %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(years == max(levels(periods))) %>%
  mutate(povvar = "low60ahc",
         groupingvar = "All children") %>%
  select(years, groupingvar, chrate, groupsample_ch, povvar)

abs <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "abspovahc")) %>%
  addyearvar %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(years == max(levels(periods))) %>%
  mutate(povvar = "abspovahc",
         groupingvar = "All children") %>%
  select(years, groupingvar, chrate, groupsample_ch, povvar)

cmd <- do.call(rbind.data.frame, lapply(hbai, getpov, povvar = "cmdahc")) %>%
  addyearvar %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  filter(years == max(levels(periods))) %>%
  mutate(povvar = "cmdahc",
         groupingvar = "All children") %>%
  select(years, groupingvar, chrate, groupsample_ch, povvar)


rel1 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "depchldh_ch", "3 or more children")
abs1 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "depchldh_ch", "3 or more children")
cmd1 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "depchldh_ch", "3 or more children")
rel2 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "babyhh", "Youngest child is younger than 1")
abs2 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "babyhh", "Youngest child is younger than 1")
cmd2 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "babyhh", "Youngest child is younger than 1")
rel3 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "youngmumhh", "Mother under 25 in household")
abs3 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "youngmumhh", "Mother under 25 in household")
cmd3 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "youngmumhh", "Mother under 25 in household")
rel4 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "loneparenthh", "Single parent in household")
abs4 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "loneparenthh", "Single parent in household")
cmd4 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "loneparenthh", "Single parent in household")
rel5 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "dispp_hh", "In household with disabled person(s)")
abs5 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "dispp_hh", "In household with disabled person(s)")
cmd5 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "dispp_hh", "In household with disabled person(s)")
rel6 <- getpriority(hbai, povvar = "low60ahc",
                    groupingvar = "ethgrphh_2f", "(Non-white) minority ethnic")
abs6 <- getpriority(hbai, povvar = "abspovahc",
                    groupingvar = "ethgrphh_2f", "(Non-white) minority ethnic")
cmd6 <- getpriority(hbai, povvar = "cmdahc",
                    groupingvar = "ethgrphh_2f", "(Non-white) minority ethnic")

povertychartdata[["priority"]] <- rbind(rel, rel1, rel2, rel3, rel4, rel5, rel6,
                                        abs, abs1, abs2, abs3, abs4, abs5, abs6,
                                        cmd, cmd1, cmd2, cmd3, cmd4, cmd5, cmd6)

remove(rel, rel1, rel2, rel3, rel4, rel5, rel6,
       abs, abs1, abs2, abs3, abs4, abs5, abs6,
       cmd, cmd1, cmd2, cmd3, cmd4, cmd5, cmd6)

# Income ----

## medians ----
medianbhc <- do.call(rbind.data.frame, lapply(hbai, getmediansbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$periods),
         key = "Before housing costs")

medianahc <- do.call(rbind.data.frame, lapply(hbai, getmediansahc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$periods),
         key = "After housing costs")

povertychartdata[["medians"]] <- rbind(medianbhc, medianahc)

remove(medianbhc, medianahc)


## decile points ----
povertychartdata[["deciles"]] <- do.call(rbind.data.frame, lapply(hbai, getdecptsbhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$periods))

## distribution ----
povertychartdata[["distribution"]] <- getdistribution(hbai[[length(labels$years[[1]])]]) %>%
  rbind(getdistribution(hbai[[length(labels$years[[1]]) - 1]])) %>%
  rbind(getdistribution(hbai[[length(labels$years[[1]]) - 2]]))

povertychartdata[["UKdeciles"]] <- getUKdeciles(hbai[[length(labels$years[[1]])]]) %>%
  right_join(getUKdeciles(hbai[[length(labels$years[[1]]) - 1]]), by = "name") %>%
  right_join(getUKdeciles(hbai[[length(labels$years[[1]]) - 2]]), by = "name") %>%
  mutate(value = (value.x + value.y + value)/3) %>%
  select(name, value)

## sources ----
sources1 <- getsources(hbai[[length(labels$years[[1]])]])
sources2 <- getsources(hbai[[length(labels$years[[1]]) - 1]])
sources3 <- getsources(hbai[[length(labels$years[[1]]) - 2]])

sources <- data.frame(sources1[1])
sources[2] <- (sources1[2] + sources2[2] + sources3[2])/3
sources[3] <- (sources1[3] + sources2[3] + sources3[3])/3
sources[4] <- (sources1[4] + sources2[4] + sources3[4])/3
sources[5] <- (sources1[5] + sources2[5] + sources3[5])/3
sources[6] <- (sources1[6] + sources2[6] + sources3[6])/3

povertychartdata[["sources"]] <- sources %>%
  mutate(decbhc = factor(decbhc))

remove(sources1, sources2, sources3, sources)


## palma ----
povertychartdata[["palma"]] <- do.call(rbind.data.frame, lapply(hbai, getpalmabhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$periods))

## gini ----
povertychartdata[["gini"]] <- do.call(rbind.data.frame, lapply(hbai, getginibhc)) %>%
  addyearvar() %>%
  mutate_if(is.numeric, get3yraverage) %>%
  tail(-2L) %>%
  mutate(years = factor(years,
                        levels = labels[["years"]]$years,
                        labels = labels[["years"]]$periods))

remove(hbai, adult, labels)


