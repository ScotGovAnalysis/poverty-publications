# TODO ----
# update labels$years with latest year

# This script contains all factor levels and labels for consistent
#   categorisation of characteristics etc. It's used to format / label FRS and
#   HBAI variables that have numeric codes, or to recode existing categories.

# load -------------------------------------------------------------------------
library(tidyverse)

# Create list for factor levels and labels
labels <- list()

# Years -------------------------------------------------------------------------------------------
labels$years <- data.frame(numbered = seq(1, 28, 1),
                           years = c("9495", "9596", "9697", "9798", "9899",
                                     "9900", "0001", "0102", "0203", "0304",
                                     "0405", "0506", "0607", "0708", "0809",
                                     "0910", "1011", "1112", "1213", "1314",
                                     "1415", "1516", "1617", "1718", "1819",
                                     "1920", "2021", "2122"),
                           formatted = c("1994/95", "1995/96", "1996/97", "1997/98", "1998/99",
                                         "1999/00", "2000/01", "2001/02", "2002/03", "2003/04",
                                         "2004/05", "2005/06", "2006/07", "2007/08", "2008/09",
                                         "2009/10", "2010/11", "2011/12", "2012/13", "2013/14",
                                         "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
                                         "2019/20", "2020/21", "2021/22"),
                           periods = c(NA, NA, "1994-97", "1995-98", "1996-99",
                                       "1997-00", "1998-01", "1999-02", "2000-03",
                                       "2001-04", "2002-05", "2003-06", "2004-07",
                                       "2005-08", "2006-09", "2007-10", "2008-11",
                                       "2009-12", "2010-13", "2011-14", "2012-15",
                                       "2013-16", "2014-17", "2015-18", "2016-19",
                                       "2017-20", "2018-21", "2019-22"),
                           longperiods = c(NA, NA, "1994/95-1996/97", "1995/96-1997/98", "1996/97-1998/99",
                                           "1997/98-1999/00", "1998/99-2000/01", "1999/00-2001/02", "2000/01-2002/03",
                                           "2001/02-2003/04", "2002/03-2004/05", "2003/04-2005/06", "2004/05-2006/07",
                                           "2005/06-2007/08", "2006/07-2008/09", "2007/08-2009/10", "2008/09-2010/11",
                                           "2009/10-2011/12", "2010/11-2012/13", "2011/12-2013/14", "2012/13-2014/15",
                                           "2013/14-2015/16", "2014/15-2016/17", "2015/16-2017/18", "2016/17-2018/19",
                                           "2017/18-2019/20", "2018/19-2020/21", "2019/20-2021/22"),
                           period5yr = c(NA, NA, NA, NA, "1994-99",
                                         "1995-00", "1996-01", "1997-02", "1998-03", "1999-04",
                                         "2000-05", "2001-06", "2002-07", "2003-08", "2004-09",
                                         "2005-10", "2006-11", "2007-12", "2008-13", "2009-14",
                                         "2010-15", "2011-16", "2012-17", "2013-18", "2014-19",
                                         "2015-20", "2016-21", "2017-22"),
                           long5yrperiods = c(NA, NA, NA, NA, "1994/95-1998/99",
                                              "1995/96-1999/00", "1996/97-2000/01", "1997/98-2001/02", "1998/99-2002/03", "1999/00-2003/04",
                                              "2000/01-2004/05", "2001/02-2005/06", "2002/03-2006/07", "2003/04-2007/08", "2004/05-2008/09",
                                              "2005/06-2009/10", "2006/07-2010/11", "2007/08-2011/12", "2008/09-2012/13", "2009/10-2013/14",
                                              "2010/11-2014/15", "2011/12-2015/16", "2012/13-2016/17", "2013/14-2017/18", "2014/15-2018/19",
                                              "2015/16-2019/20", "2016/17-2020/21", "2017/18-2021/22"),

                           period2yr = c(NA, "1994-96", "1995-97", "1996-98", "1997-99",
                                         "1998-00", "1999-01", "2000-02", "2001-03",
                                         "2002-04", "2003-05", "2004-06", "2005-07",
                                         "2006-08", "2007-09", "2008-10", "2009-11",
                                         "2010-12", "2011-13", "2012-14", "2013-15",
                                         "2014-16", "2015-17", "2016-18", "2017-19",
                                         "2018-20", "2019-21", "2020-22"))

# Years expanded (for cp update)
labels$years_exp <- data.frame(years_exp = c("9495", "9596", "9697", "9798",
                                             "9899", "9900", "0001", "0102",
                                             "0203", "0304", "0405", "0506",
                                             "0607", "0708", "0809", "0910",
                                             "1011", "1112", "1213", "1314",
                                             "1415", "1516", "1617", "1718",
                                             "1819", "1920", "2021", "2122",
                                             "2223", "2324", "2425", "2526",
                                             "2627", "2728", "2829", "2930",
                                             "3031"),
                               formatted = c("1994/95", "1995/96", "1996/97",
                                             "1997/98", "1998/99", "1999/00",
                                             "2000/01", "2001/02", "2002/03",
                                             "2003/04", "2004/05", "2005/06",
                                             "2006/07", "2007/08", "2008/09",
                                             "2009/10", "2010/11", "2011/12",
                                             "2012/13", "2013/14", "2014/15",
                                             "2015/16", "2016/17", "2017/18",
                                             "2018/19", "2019/20", "2020/21",
                                             "2021/22", "2022/23", "2023/24",
                                             "2024/25", "2025/26", "2026/27",
                                             "2027/28", "2028/29", "2029/30",
                                             "2030/31"),
                               numbered = seq(1, 37, 1))

# Regions ----------------------------------------------------------------------
labels$regions <- data.frame(codes = c(1:13),
                             labels = c("England", "England", "England",
                                        "England", "England",  "England",
                                        "England", "England", "England",
                                        "England", "Wales", "Scotland",
                                        "Northern Ireland"))

labels$regions <- labels$regions %>% mutate(labels = fct_reorder(labels, codes))

# People -----------------------------------------------------------------------
labels$people <- c("people", "children", "adults", "pensioners")

# Income types -----------------------------------------------------------------
labels$inctypes <- c("total", "earn", "ben", "occ", "inv", "oth", "privben", "ded")

# Urban/rural ------------------------------------------------------------------
labels$urbrur <- data.frame(codes = c(1:8), labels = c("Urban", "Urban", "Urban",
                                                       "Urban", "Urban", "Rural",
                                                       "Rural", "Rural"))

# Tenure -----------------------------------------------------------------------
labels$tenure <- data.frame(codes = c(1:4),
                            labels = c("Owned outright",
                                       "Buying with a mortgage",
                                       "Rented from council or housing association",
                                       "Rented privately"))

labels$tenure <- labels$tenure %>% mutate(labels = fct_reorder(labels, codes))

# Economic status --------------------------------------------------------------
labels$economic <- data.frame(codes = c(1:8),
                              labels = c("Self-employed (at least one full-time)",
                                         "All in full-time work",
                                         "Couple: one full-time, one part-time",
                                         "Couple: one full-time, one not in paid work",
                                         "Part-time work only",
                                         "Inactive or retired",
                                         "Unemployed",
                                         "Inactive or retired"))

labels$economic <- labels$economic %>% mutate(labels = fct_reorder(labels, codes))

labels$workinghh <- data.frame(codes = c(0, 1), labels = c("No one in paid work",
                                                           "Someone in paid work"))

labels$workinghh <- labels$workinghh %>% mutate(labels = fct_reorder(labels, codes))

labels$empstatc <- data.frame(codes = c(1:5),
                              labels = c("Full time self employed",
                                         "Full time employed",
                                         "Part time employed or self-employed",
                                         "Unemployed",
                                         "Not working for any other reason"))

labels$empstatc <- labels$empstatc %>% mutate(labels = fct_reorder(labels, codes))

labels$empstati <- data.frame(codes = c(1:11),
                              labels = c("Full-time Employee",
                                         "Part-time Employee",
                                         "Full-time Self-Employed",
                                         "Part-time Self-Employed",
                                         "Unemployed",
                                         "Retired",
                                         "Student",
                                         "Looking after family/home",
                                         "Permanently sick/disabled",
                                         "Temporarily sick/injured",
                                         "Other Inactive"))

labels$empstati <- labels$empstati %>% mutate(labels = fct_reorder(labels, codes))

# Children's economic status ---------------------------------------------------
labels$kideconomic <- data.frame(
  codes = c(1:9),
  labels = c("Lone parent, in full-time work",
             "Lone parent, in part-time work",
             "Lone parent, not working",
             "Couple with children, one or more full-time self-employed",
             "Couple with children, both in full-time work",
             "Couple with children, one in full-time work, one in part-time work",
             "Couple with children, one in full-time work, one not working",
             "Couple with children, one or more in part-time work",
             "Couple with children, both not in work"))

labels$kideconomic <- labels$kideconomic %>% mutate(labels = fct_reorder(labels, codes))

# Family type ------------------------------------------------------------------
labels$familytype <- data.frame(
  codes = c(1:8),
  labels = c("Pensioner couple",
             "Single pensioner - male",
             "Single pensioner - female",
             "Working-age couple with dependent children",
             "Working-age single with dependent children",
             "Working-age couple without dependent children",
             "Working-age single without dependent children - male",
             "Working-age single without dependent children - female"))

labels$familytype <- labels$familytype %>% mutate(labels = fct_reorder(labels, codes))

# Gender (single adult hhlds) --------------------------------------------------

labels$gender <- data.frame(codes = c(1:6),
                            labels = c("Male pensioner",
                                       "Female pensioner",
                                       "Male working-age adult, no dependent children",
                                       "Female working-age adult, no dependent children",
                                       "Male working-age adult with dependent children",
                                       "Female working-age adult with dependent children"))

labels$gender <- labels$gender %>% mutate(labels = fct_reorder(labels, codes))

# Lone parent (household) ------------------------------------------------------
labels$loneparent <- data.frame(codes = c(0, 1),
                                labels = c("No single parent in household",
                                           "Single parent in household"))

labels$loneparent <- labels$loneparent %>% mutate(labels = fct_reorder(labels, codes))

# Baby in household ------------------------------------------------------------
labels$baby <- data.frame(codes = c(0, 1),
                          labels = c("Youngest child in household is 1 or older",
                                     "Youngest child is younger than 1"))

labels$baby <- labels$baby %>% mutate(labels = fct_reorder(labels, codes))

# Mother < 25 in household -----------------------------------------------------
labels$youngmum <- data.frame(codes = c(0, 1),
                              labels = c("No mother under 25 in household",
                                         "Mother under 25 in household"))

labels$youngmum <- labels$youngmum %>% mutate(labels = fct_reorder(labels, codes))

# Marital status ---------------------------------------------------------------
labels$marital <- data.frame(codes = c(1:6),
                             labels = c("Married / Civil Partnership",
                                        "Cohabiting",
                                        "Single",
                                        "Widowed",
                                        "Divorced / Civil Partnership dissolved / separated",
                                        "Divorced / Civil Partnership dissolved / separated"))

labels$marital <- labels$marital %>% mutate(labels = fct_reorder(labels, codes))

# Number of children -----------------------------------------------------------
labels$childno <- data.frame(codes = c(0:10),
                             labels = c("No children in the household",
                                        "1 child in the household",
                                        "2 children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household",
                                        "3 or more children in the household"))

labels$childno <- labels$childno %>% mutate(labels = fct_reorder(labels, codes))

# Number of children (child poverty tables)-------------------------------------
labels$childno_ch <- data.frame(codes = c(0:10),
                                labels = c("No children in the household",
                                           "1-2 children in the household",
                                           "1-2 children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household",
                                           "3 or more children in the household"))

labels$childno_ch <- labels$childno_ch %>% mutate(labels = fct_reorder(labels, codes))

# Disability -------------------------------------------------------------------
labels$disch <- data.frame(codes = c(0, 1),
                           labels = c("In household with no disabled child(ren)",
                                      "In household with disabled child(ren)"))
labels$disad <- data.frame(codes = c(0, 1),
                           labels = c("In household with no disabled adult(s)",
                                      "In household with disabled adult(s)"))
labels$dispp <- data.frame(codes = c(0, 1),
                           labels = c("In household with no disabled person(s)",
                                      "In household with disabled person(s)"))

# Ethnic group -----------------------------------------------------------------
labels$ethnic1213 <- data.frame(codes = c(1:24),
                                labels = c("White - British",
                                           "White - Other",
                                           "White - Other",
                                           "White - Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other"))

labels$ethnic1213 <- labels$ethnic1213 %>% mutate(labels = fct_reorder(labels, codes))

labels$ethnic1112 <- data.frame(codes = c(1:18),
                                labels = c("White - British",
                                           "White - Other",
                                           "White - Other",
                                           "White - Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other"))

labels$ethnic1112 <- labels$ethnic1112 %>% mutate(labels = fct_reorder(labels, codes))

labels$ethnic0203 <- data.frame(codes = c(1:16),
                                labels = c("White - British",
                                           "White - Other",
                                           "White - Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other"))

labels$ethnic0203 <- labels$ethnic0203 %>% mutate(labels = fct_reorder(labels, codes))

labels$ethnic0102 <- data.frame(codes = c(1:15),
                                labels = c("White - British",
                                           "White - Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other"))

labels$ethnic0102 <- labels$ethnic0102 %>% mutate(labels = fct_reorder(labels, codes))

labels$ethnic9495 <- data.frame(codes = c(1:9),
                                labels = c("White - British",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Mixed, Black or Black British, and Other",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Asian or Asian British",
                                           "Mixed, Black or Black British, and Other"))

labels$ethnic9495 <- labels$ethnic9495 %>% mutate(labels = fct_reorder(labels, codes))

# Ethnic group (2-fold for 3-year averages) ------------------------------------
labels$ethnic_2f <- data.frame(codes = c(1:24),
                               labels = c("White - British",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic",
                                          "Minority ethnic"))

labels$ethnic_2f <- labels$ethnic_2f %>% mutate(labels = fct_reorder(labels, codes))

# Religion ---------------------------------------------------------------------
labels$religion <- data.frame(codes = c(1:10),
                              labels = c("No religion",
                                         "Church of Scotland",
                                         "Roman Catholic",
                                         "Other Christian",
                                         "Other religion",
                                         "Other religion",
                                         "Other religion",
                                         "Muslim",
                                         "Other religion",
                                         "Other religion"))

labels$religion <- labels$religion %>% mutate(labels = fct_reorder(labels, codes))

# Sexual identity --------------------------------------------------------------
labels$sexid <- data.frame(codes = c(1, 2, 3, 4),
                           labels = c("Heterosexual / straight",
                                      "Gay / lesbian",
                                      "Bisexual",
                                      "Other"))

labels$sexid <- labels$sexid %>% mutate(labels = fct_reorder(labels, codes))

# Country of origin ------------------------------------------------------------

labels$corign <- data.frame(codes = seq(1, 10, 1),

                            # from 0809
                            # 1	England
                            # 2	Wales
                            # 3	Scotland
                            # 4	Northern Ireland
                            # 5	UK, Britain
                            # 6	Republic of Ireland
                            # 7	Hong Kong
                            # 8	China
                            # 9	Other

                            # from 1314
                            # 1	England
                            # 2	Wales
                            # 3	Scotland
                            # 4	Northern Ireland
                            # 5	UK, Britain
                            # 6	Republic of Ireland
                            # 7	India
                            # 8	Pakistan
                            # 9	Poland
                            # 10	Other

                            labels = c("UK",
                                       "UK",
                                       "UK",
                                       "UK",
                                       "UK",
                                       "Other",
                                       "India & Pakistan",
                                       "India & Pakistan",
                                       "Poland",
                                       "Other"))

labels$corign <- labels$corign %>% mutate(labels = fct_reorder(labels, codes))

# Food security ----------------------------------------------------------------
labels$foodsecurity <- data.frame(codes = c(1:4),
                                  labels = c("High",
                                             "Marginal",
                                             "Low",
                                             "Very low"))

labels$foodsecurity <- labels$foodsecurity %>% mutate(labels = fct_reorder(labels, codes))

# Council areas ----------------------------------------------------------------

# note that the codes change occasionally, so check for missing values after
#   recoding - they might be due to changed codes (see latest FRS varable list)

labels$laua <- data.frame(codes = c(212000033, 212000034, 212000041,
                                    212000035, 212000005, 212000006,
                                    212000042, 212000008, 212000045,
                                    212000010, 212000011, 212000036,
                                    212000014, 212000047, 212000046,
                                    212000017, 212000018, 212000019,
                                    212000020, 212000021, 212000044,
                                    212000023, 212000048, 212000038,
                                    212000026, 212000027, 212000028,
                                    212000029, 212000030, 212000039,
                                    212000040, 212000013,

                                    # new GSS codes added in 04/2019
                                    212000049, 212000050),

                          names = c("Aberdeen City",
                                    "Aberdeenshire",
                                    "Angus",
                                    "Argyll and Bute",
                                    "Clackmannanshire",
                                    "Dumfries and Galloway",
                                    "Dundee City",
                                    "East Ayrshire",
                                    "East Dunbartonshire",
                                    "East Lothian",
                                    "East Renfrewshire",
                                    "City of Edinburgh",
                                    "Falkirk",
                                    "Fife",
                                    "Glasgow City",
                                    "Highland",
                                    "Inverclyde",
                                    "Midlothian",
                                    "Moray",
                                    "North Ayrshire",
                                    "North Lanarkshire",
                                    "Orkney Islands",
                                    "Perth and Kinross",
                                    "Renfrewshire",
                                    "Scottish Borders",
                                    "Shetland Islands",
                                    "South Ayrshire",
                                    "South Lanarkshire",
                                    "Stirling",
                                    "West Dunbartonshire",
                                    "West Lothian",
                                    "Na h-Eileanan Siar",
                                    "Glasgow City",
                                    "North Lanarkshire"))

labels$lac <- data.frame(codes = c(194, 195, 196, 287, 289, 291, 292, 293,
                                   294, 295, 296, 387, 388, 389, 390, 391,
                                   392, 393, 394, 395, 396, 494, 487, 488,
                                   288, 493, 489, 490, 491, 290, 492, 495),
                         names = c("Aberdeen City",
                                   "Aberdeenshire",
                                   "Angus",
                                   "Argyll and Bute",
                                   "Clackmannanshire",
                                   "Dumfries and Galloway",
                                   "Dundee City",
                                   "East Ayrshire",
                                   "East Dunbartonshire",
                                   "East Lothian",
                                   "East Renfrewshire",
                                   "City of Edinburgh",
                                   "Falkirk",
                                   "Fife",
                                   "Glasgow City",
                                   "Highland",
                                   "Inverclyde",
                                   "Midlothian",
                                   "Moray",
                                   "North Ayrshire",
                                   "North Lanarkshire",
                                   "Orkney Islands",
                                   "Perth and Kinross",
                                   "Renfrewshire",
                                   "Scottish Borders",
                                   "Shetland Islands",
                                   "South Ayrshire",
                                   "South Lanarkshire",
                                   "Stirling",
                                   "West Dunbartonshire",
                                   "West Lothian",
                                   "Na h-Eileanan Siar"))
