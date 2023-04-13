# Run once all micro-data is imported to the data folder

# 0 update scripts ----------- -------------------------------------------------
# Some scripts need updating when a new data year has been added. This is
# usually mentioned at the top of each script under 'TODO'. For example,
# 00_strings.R needs the new year and the new 3-year etc. periods added.

# 1 import data --------------- ------------------------------------------------
# - run 01_importpersistentpovertydata.R and 01_importSASfiles.R individually
# - make sure any changes (noted at top of each script) have been made (i.e. new
#     year)

# 2 clean ------------------------ ---------------------------------------------

source("R/02_cleanbenefitsdata.R")
source("R/02_cleanchilddata.R")
source("R/02_cleanchldcaredata.R")
source("R/02_cleanhouseholdata.R")
source("R/02_cleanadultdata.R")
source("R/02_cleanhbaidata.R")

# 3 tidy -------------------------- --------------------------------------------

source("R/03_maketidydatasets.R")
source("R/03_get_psus_strata.R")

# 4 -------------------------------- -------------------------------------------

# * aggregate ------------------------------------------------------------------

source("R/04_aggregate_1yr_Scot.R")
source("R/04_aggregate_3yrs_Scot.R")

# * confidence intervals -------------------------------------------------------

source("R/04_indicative_CIs.R")

# * re-package existing tables from DWP ----------------------------------------

source("R/04_tables_persistent.R")
source("R/04_tables_uncertainty.R")

# 5 -------------------------------- -------------------------------------------

# * charts & html tables -------------------------------------------------------

source("R/05_charts_3yrs_Scot.R", encoding = "UTF-8")
source("R/05_charts_persistent.R", encoding = "UTF-8")
source("R/05_charts_cpupdate.R", encoding = "UTF-8")

# * spreadsheets ---------------------------------------------------------------

source("R/05_xlsx_1yr_Scot.R", encoding = "UTF-8")
source("R/05_xlsx_1yr_UK.R")
source("R/05_xlsx_3yrs_Scot.R", encoding = "UTF-8")
source("R/05_xlsx_3yrs_UK.R")
source("R/05_xlsx_persistent.R")

# 6 -------------------------------- -------------------------------------------

# * spreadsheet postprocessing -------------------------------------------------
# (with VBA)
source("R/06_xlsx_postprocessing.R")

# 7 -------------------------------- -------------------------------------------

# * website --------------------------------------------------------------------

# optional: delete existing output folder (./website/_site)
rmarkdown::clean_site(input = "./website")

# render website
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')

# * copy to website folder and rename ------------------------------------------

# optional: creates a copy of the website folder with the correct name ("poverty")
#   ready for publishing on data.gov.scot

if (!dir.exists("poverty")){dir.create("poverty")}
file.copy(from = file.path("website/_site/", list.files("website/_site/")),
          to = "poverty",
          recursive = TRUE,
          overwrite = TRUE)
