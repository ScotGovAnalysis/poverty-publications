
# Define strings, factor levels, etc.

# Years ---------------------------------------------------------------------
years <- c("9495", "9596", "9697", "9798", "9899",
           "9900", "0001", "0102", "0203", "0304",
           "0405", "0506", "0607", "0708", "0809",
           "0910", "1011", "1112", "1213", "1314",
           "1415", "1516", "1617", "1718", "1819")

years_formatted <- c("1994-95", "1995-96", "1996-97", "1997-98", "1998-99",
                     "1999-00", "2000-01", "2001-02", "2002-03", "2003-04",
                     "2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
                     "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
                     "2014-15", "2015-16", "2016-17", "2017-18", "2018-19")

periods_formatted <- c("1994-97", "1995-98", "1996-99", "1997-00", 
                       "1998-01", "1999-02", "2000-03", "2001-04",
                       "2002-05", "2003-06", "2004-07", "2005-08",
                       "2006-09", "2007-10", "2008-11", "2009-12",
                       "2010-13", "2011-14", "2012-15", "2013-16",
                       "2014-17", "2015-18", "2016-19")

# Add latest year

# years <- c(years, "1920")
# years_formatted <- c(years_formatted, "2019-20")
# period_formatted <- c(period_formatted, "2017-20")


# People --------------------------------------------------------------------
people <-c("people", "children", "adults", "pensioners")

# Income types --------------------------------------------------------------
inctypes <- c("total", "earn", "ben", "occ", "inv", "oth", "privben", "ded")

# Urbrur classes ------------------------------------------------------------
urbrurcodes <- c(1, 2, 3, 4, 5, 6, 7, 8)
urbrurclasses <- c("Urban", 
                   "Urban",
                   "Urban", 
                   "Urban",
                   "Urban",
                   "Rural", 
                   "Rural",
                   "Rural")

# Tenure --------------------------------------------------------------------
tenurecodes <- c(1, 2, 3, 4, 5, 6)
tenurenames <- c("Rented from council or housing association",
                 "Rented from council or housing association",
                 "Rented privately",
                 "Rented privately",
                 "Owned outright",
                 "Owned with mortgage")

# Economic status -----------------------------------------------------------
ecocodes <- c(1, 2, 3, 4, 5, 6, 7, 8)
econames <- c("Self-employed (at least one FT)",
              "All in full-time work",
              "Couple: one FT, one PT",
              "Couple: one FT, one not in paid work",
              "Part-time work only",
              "Inactive or retired",
              "Unemployed",
              "Inactive or retired")

kidecocodes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
kideconames <- c("Lone parent, in full-time work",
                 "Lone parent, in part-time work",
                 "Lone parent, not working",
                 "Couple with children, one or more full-time self-employed",
                 "Couple with children, both in full-time work",
                 "Couple with children, one in full-time work, one in part-time work",
                 "Couple with children, one in full-time work, one not working",
                 "Couple with children, one or more in part-time work",
                 "Couple with children, both not in work")

# Family type ---------------------------------------------------------------
famcodes <- c(1, 2, 3, 4, 5, 6, 7, 8)
famnames <- c("Pensioner couple",
              "Single pensioner - male",
              "Single pensioner - female",
              "Working-age couple with dependent children",
              "Working-age single with dependent children",
              "Working-age couple without dependent children",
              "Working-age single without dependent children - male",
              "Working-age single without dependent children - female")

# Marital status ------------------------------------------------------------
maritalcodes <- c(1, 2, 3, 4, 5, 6)
maritalnames <- c("Married / Civil Partnership",
                  "Cohabiting",
                  "Single",
                  "Widowed",
                  "Divorced / Civil Partnership dissolved / separated",
                  "Divorced / Civil Partnership dissolved / separated")


# Number of children --------------------------------------------------------

childnocodes <- c(0, 1,2,3,4,5,6,7,8,9,10)
childnolabels <- c("No children",
                   "1 child",
                   "2 children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children",
                   "3 or more children")

# Regions -------------------------------------------------------------------
regioncodes <- c(1:13)
regionnames <- c("England", "England", "England", "England", "England", 
                 "England", "England", "England", "England", "England", 
                 "Wales", "Scotland", "Northern Ireland")