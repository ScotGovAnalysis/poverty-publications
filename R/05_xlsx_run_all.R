source("R/04_xlsx_1yr_part1.R", encoding = "UTF-8")
source("R/04_xlsx_1yr_part2.R", encoding = "UTF-8")

source("R/04_xlsx_3yrs_part1.R", encoding = "UTF-8")
source("R/04_xlsx_3yrs_part2.R", encoding = "UTF-8")
source("R/04_xlsx_3yrs_part3.R", encoding = "UTF-8")
source("R/04_xlsx_3yrs_part4.R", encoding = "UTF-8")

source("R/04_xlsx_3yrs_UK.R", encoding = "UTF-8")

# Copy xlsx files for publication to website folder
files_to_copy <- list("All single year.xlsx",
                      "All three-year average.xlsx")

file.copy(from = file.path("output/", files_to_copy),
          to = "website/xls/",
          overwrite = TRUE,
          copy.mode = TRUE)

rm(list = ls())
