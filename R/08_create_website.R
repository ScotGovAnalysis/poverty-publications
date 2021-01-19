
# Run code once all micro-data is updated and R/03_maketidydatasets.R runs ok

# Make spreadsheets (takes a few minutes)
source("R/05_run_maketables.R")

# Prep charts
source("R/06_prepcharts_poverty.R")
source("R/06_prepcharts_cpupdate.R")

# Make charts and tables for reports and cp update
source("R/07_makecharts_poverty.R")
source("R/07_maketables_poverty.R", encoding = "UTF-8")
source("R/07_makecharts_persistentpoverty.R")
source("R/07_maketables_persistentpoverty.R")
source("R/07_makecharts_cpupdate.R")

# Render website
rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')


