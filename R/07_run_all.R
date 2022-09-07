# Run once all micro-data is imported to the data folder

# 2 clean datasets ---------------------------------------------------------------
source("R/02_cleanadultdata.R")
source("R/02_cleanbenefitsdata.R")
source("R/02_cleanchilddata.R")
source("R/02_cleanchldcaredata.R")
source("R/02_cleanhbaidata.R")
source("R/02_cleanhouseholdata.R")

# 3 create tidy datasets -------------------------------------------------------
source("R/03_maketidydatasets.R")

# 4 analyse and aggregate ------------------------------------------------------
source("R/04_aggregate_1yr_Scot.R")
source("R/04_aggregate_1yr_UK.R")
source("R/04_aggregate_3yrs_Scot.R")
source("R/04_aggregate_3yrs_UK.R")

# *  re-package existing tables ------------------------------------------------
source("R/04_tables_persistent.R")
source("R/04_tables_uncertainty.R")

# 5 create charts and html tables ----------------------------------------------
source("R/05_charts_3yrs_Scot.R", encoding = "UTF-8")
source("R/05_charts_persistent.R", encoding = "UTF-8")
source("R/05_charts_cpupdate.R", encoding = "UTF-8")

# * create spreadsheets --------------------------------------------------------
# (takes a few minutes (~ 7 min for the big table on a good day))
source("R/05_xlsx_1yr_Scot.R", encoding = "UTF-8")
source("R/05_xlsx_3yrs_Scot.R")
source("R/05_xlsx_3yrs_UK.R")
source("R/05_xlsx_persistent.R")

# 6 spreadsheet postprocessing -------------------------------------------------
# (with VBA)
source("R/06_xlsx_postprocessing.R")

# 7 copy spreadsheets to website folder ----------------------------------------
# (not the UK ones, they are for briefing only)
files_to_copy <- list("data2022_1yr.xlsx", "data2022.xlsx",
                      "data2022_1yr.ods", "data2022.ods",
                      "data2022_persistent.xlsx", "data2022_persistent.ods")
file.copy(from = file.path("output/", files_to_copy), to = "website/xls/",
          overwrite = TRUE, copy.mode = TRUE)

# 8 render website -------------------------------------------------------------
rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')


