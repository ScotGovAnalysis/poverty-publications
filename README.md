# About

This repository contains the code for the annual Scottish Government "Poverty an Income Inequality in Scotland" report, the "Persistent POverty in Scotland" report, the accompanying associated and supplementary tables, and the infographic child poverty update. It produces the folder website/\_site which contains all files for a website, including downloadable spreadsheets, to be hosted on data.gov.scot/poverty.

# Workflow

### New data arrives

Run all [Data prep](#markdown-header-data-prep) scripts and make sure 02_clean...data.R and 03_maketidydatasets.R scripts return no errors. Tidy datasets are now available to produce reports and spreadsheets.

### Create spreadsheets

Run 05_run_maketables.R to create all spreadsheets.

### Create website

* Run R/08_create_website.R
* Look at website: website/\_site/index.html
* Check and update all commentary in:

  * website/index.Rmd
  * website/\_poverty_chapters/...
  * website/cpupdate.Rmd
  * website/download.Rmd
  * website/about.Rmd
  * website/persistent.Rmd
  * website/\_persistent_chapters/...

* Run R/08_create_website.R again and repeat until happy

### Create doc reports for briefing and download purposes - to do

* Run ... to create main report docx .


# List of scripts

### Helpers

* R/00_colours.R
* R/00_functions.R
* R/00_strings.R

### Data prep

* R/01_importSASfiles.R - takes hours to run, maybe do at night; script not included on GitHub
* R/01_importpersistentpovertydata.R - depends on persistent poverty data being supplied in a certain format; script not included on GitHub
* R/02_cleanadultdata.R
* R/02_cleanbenefitsdata.R
* R/02_cleanchilddata.R
* R/02_cleanhbaidata.R
* R/02_cleanhouseholdata.R
* R/03_maketidydatasets.R

### Create spreadsheets

* R/04_maketables_characteristics.R
* R/04_maketables_characteristics_child.R
* R/04_maketables_headlines_1yr.R
* R/04_maketables_headlines_3yrs.R
* R/04_maketables_income_1yr.R
* R/04_maketables_income_3yrs.R
* R/04_maketables_UK_3yrs.R
* R/05_run_maketables.R - runs all maketables scripts and copies spreadsheets into website folder

### Create charts and tables included in reports

* R/06_maketables_persistentpoverty.R
* R/06_makecharts_persistentpoverty.R
* R/06_maketables_poverty.R
* R/06_prepcharts_poverty.R
* R/07_makecharts_poverty.R

### Create website

* R/08_create_website.R - creates complete website in website/\_site
