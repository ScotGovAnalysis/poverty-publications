source("R/04_maketables_3yrs_part1.R", encoding = "UTF-8")
source("R/04_maketables_3yrs_part2.R", encoding = "UTF-8")
source("R/04_maketables_3yrs_part3.R", encoding = "UTF-8")
source("R/04_maketables_3yrs_part4.R", encoding = "UTF-8")
source("R/04_maketables_singleyear_part1.R", encoding = "UTF-8")
source("R/04_maketables_singleyear_part2.R", encoding = "UTF-8")
source("R/04_maketables_UK_3yrs.R", encoding = "UTF-8")

# Copy all xlsx files to website folder except for UK one,
# which is not for publication

files_to_copy <- list("All single year.xlsx",
                      "All three-year average.xlsx")

file.copy(from = file.path("output/", files_to_copy),
          to = "website/xls/",
          overwrite = TRUE,
          copy.mode = TRUE)

rm(list = ls())
