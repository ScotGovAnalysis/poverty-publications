
source("R/04_maketables_headlines_1yr.R", encoding = "UTF-8")
source("R/04_maketables_headlines_3yrs.R", encoding = "UTF-8")

source("R/04_maketables_income_1yr.R", encoding = "UTF-8")
source("R/04_maketables_income_3yrs.R", encoding = "UTF-8")

source("R/04_maketables_characteristics.R", encoding = "UTF-8")
source("R/04_maketables_characteristics_child.R", encoding = "UTF-8")

source("R/04_maketables_all_3yrs.R", encoding = "UTF-8")

source("R/04_maketables_UK_3yrs.R", encoding = "UTF-8")

# Copy all xlsx files to website folder except for UK one,
# which is not for publication

files_to_copy <- list("Child poverty characteristics.xlsx",
                      "Income single year.xlsx",
                      "Income three-year average.xlsx",
                      "Poverty characteristics.xlsx",
                      "Poverty single year.xlsx",
                      "Poverty three-year average.xlsx",
                      "All three-year average.xlsx")

file.copy(from = file.path("output/", files_to_copy),
          to = "website/xls/",
          overwrite = TRUE,
          copy.mode = TRUE)
