

# Create list for factor levels and labels


labels <- list()

# Regions -----------------------------------------------------------------------------------------
labels[["regions"]] <- data.frame(codes = c(1:13),
                                  labels = c("England", "England", "England", "England", "England",
                                             "England", "England", "England", "England", "England", 
                                             "Wales", "Scotland", "Northern Ireland"))

labels[["regions"]] <- labels[["regions"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Years -------------------------------------------------------------------------------------------
labels[["years"]] <- data.frame(years = c("9495", "9596", "9697", "9798", "9899",
                                          "9900", "0001", "0102", "0203", "0304",
                                          "0405", "0506", "0607", "0708", "0809",
                                          "0910", "1011", "1112", "1213", "1314",
                                          "1415", "1516", "1617", "1718", "1819"),
                                formatted = c("1994-95", "1995-96", "1996-97", "1997-98", "1998-99",
                                              "1999-00", "2000-01", "2001-02", "2002-03", "2003-04",
                                              "2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
                                              "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
                                              "2014-15", "2015-16", "2016-17", "2017-18", "2018-19"),
                                periods = c(NA, NA, "1994-97", "1995-98", "1996-99", 
                                            "1997-00", "1998-01", "1999-02", "2000-03", 
                                            "2001-04", "2002-05", "2003-06", "2004-07", 
                                            "2005-08", "2006-09", "2007-10", "2008-11", 
                                            "2009-12", "2010-13", "2011-14", "2012-15", 
                                            "2013-16", "2014-17", "2015-18", "2016-19"))


# Add latest year

# labels[["years"]]$years <- c(labels[["years"]]$years, "1920")
# labels[["years"]]$formatted <- c(labels[["years"]]$formatted, "2019-20")
# labels[["years"]]$periods <- c(labels[["years"]]$periods, "2017-20")

# People --------------------------------------------------------------------------------------------
labels[["people"]] <- c("people", "children", "adults", "pensioners")

# Income types --------------------------------------------------------------------------------------
labels[["inctypes"]] <- c("total", "earn", "ben", "occ", "inv", "oth", "privben", "ded")

# Urban/rural ---------------------------------------------------------------------------------------
labels[["urbrur"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6, 7, 8),
                                 labels = c("Urban", 
                                            "Urban",
                                            "Urban", 
                                            "Urban",
                                            "Urban",
                                            "Rural", 
                                            "Rural",
                                            "Rural"))

# Tenure --------------------------------------------------------------------------------------------
labels[["tenure"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Rented from council or housing association",
                                           "Rented from council or housing association",
                                           "Rented privately",
                                           "Rented privately",
                                           "Owned outright",
                                           "Owned with mortgage"))

labels[["tenure"]] <- labels[["tenure"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Economic status -----------------------------------------------------------------------------------
labels[["economic"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6, 7, 8), 
                                   labels = c("Self-employed (at least one FT)",
                                              "All in full-time work",
                                              "Couple: one FT, one PT",
                                              "Couple: one FT, one not in paid work",
                                              "Part-time work only",
                                              "Inactive or retired",
                                              "Unemployed",
                                              "Inactive or retired"))

labels[["economic"]] <- labels[["economic"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Children's economic status ------------------------------------------------------------------------
labels[["kideconomic"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                      labels = c("Lone parent, in full-time work",
                                                "Lone parent, in part-time work",
                                                "Lone parent, not working",
                                                "Couple with children, one or more full-time 
                                                self-employed",
                                                "Couple with children, both in full-time work",
                                                "Couple with children, one in full-time work, one 
                                                in part-time work",
                                                "Couple with children, one in full-time work, one 
                                                not working",
                                                "Couple with children, one or more in part-time work",
                                                "Couple with children, both not in work"))

labels[["kideconomic"]] <- labels[["kideconomic"]] %>%
  mutate(labels = fct_reorder(labels, codes))


# Family type ----------------------------------------------------------------------------------------
labels[["familytype"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6, 7, 8),
                                     labels = c("Pensioner couple",
                                                "Single pensioner - male",
                                                "Single pensioner - female",
                                                "Working-age couple with dependent children",
                                                "Working-age single with dependent children",
                                                "Working-age couple without dependent children",
                                                "Working-age single without dependent children - 
                                                male",
                                                "Working-age single without dependent children - 
                                                female"))

labels[["familytype"]] <- labels[["familytype"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Marital status -------------------------------------------------------------------------------------
labels[["marital"]] <- data.frame(codes = c(1, 2, 3, 4, 5, 6),
                                  labels = c("Married / Civil Partnership",
                                             "Cohabiting",
                                             "Single",
                                             "Widowed",
                                             "Divorced / Civil Partnership dissolved / separated",
                                             "Divorced / Civil Partnership dissolved / separated"))

labels[["marital"]] <- labels[["marital"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Number of children ---------------------------------------------------------------------------------
labels[["childno"]] <- data.frame(codes = c(0, 1,2,3,4,5,6,7,8,9,10),
                                  labels = c("No children",
                                             "1 child",
                                             "2 children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children",
                                             "3 or more children"))

labels[["childno"]] <- labels[["childno"]] %>%
  mutate(labels = fct_reorder(labels, codes))

# Disability -----------------------------------------------------------------------------------------
labels[["disch"]] <- data.frame(codes = c(0, 1),
                                labels = c("In household with no disabled child(ren)",
                                           "In household with disabled child(ren)"))
labels[["disad"]] <- data.frame(codes = c(0, 1),
                                labels = c("In household with no disabled adult(s)",
                                           "In household with disabled adult(s)"))
labels[["dispp"]] <- data.frame(codes = c(0, 1),
                                labels = c("In household with no disabled people",
                                           "In household with disabled people"))

