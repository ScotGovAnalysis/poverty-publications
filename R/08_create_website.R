
# Run code once all micro-data is updated and R/03_maketidydatasets.R runs ok

Sys.time()

# Make spreadsheets (takes a few minutes)
source("R/05_xlsx_run_all.R")

# Prep charts
source("R/06_prepcharts_poverty.R", encoding = "UTF-8")
source("R/06_prepcharts_cpupdate.R", encoding = "UTF-8")

# Make charts and tables for reports and cp update
source("R/07_makecharts_poverty.R", encoding = "UTF-8")
source("R/07_maketables_poverty.R", encoding = "UTF-8")
source("R/07_makecharts_cpupdate.R")
source("R/07_maketables_uncertainty.R")

source("R/07_makecharts_persistentpoverty.R")
source("R/07_maketables_persistentpoverty.R")

# Render website
rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')

Sys.time()


