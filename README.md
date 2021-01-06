# About

This repository contains the code for the annual Scottish Government "Poverty an Income Inequality in Scotland" report, the "Persistent POverty in Scotland" report, the accompanying associated and supplementary tables, and the infographic child poverty update. It produces the folder website/\_site which contains all files for a website, including downloadable spreadsheets, to be hosted on data.gov.scot/poverty.

# Workflow

### 1. New data arrives in the SOCJUST SAS library

* Ensure the inflation_index file in the SOCJUST SAS library is up to date
* In R/00_strings.R, uncomment code under '# Add latest year'
* Run all [Data prep](#dataprep) scripts except for R/01_importSASfiles.R (this imports the previous years' datasets)
* Make sure 02_clean...data.R and 03_maketidydatasets.R scripts return no errors. 

Tidy datasets are now available to produce reports and spreadsheets.

### 2. Create spreadsheets

Run 05_run_maketables.R to create all spreadsheets.

### 3. Create website

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

### 4. Create doc reports for briefing and download purposes - to do

* Run ... to create main report docx .

# List of scripts

### Helpers

* R/00_colours.R
* R/00_functions.R
* R/00_strings.R

### Data prep<a name="dataprep"></a>

* R/01_importnewSASfiles.R - takes a while to run, maybe do at night; script not included on GitHub
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
* R/05_run_maketables.R - runs all maketables scripts and copies spreadsheets (for publication) into website folder

### Create charts and tables included in reports

* R/06_maketables_persistentpoverty.R
* R/06_makecharts_persistentpoverty.R
* R/06_maketables_poverty.R
* R/06_prepcharts_poverty.R
* R/07_makecharts_poverty.R

### Create website

* R/08_create_website.R - creates complete website in website/\_site

# List of files in website folder

### Rmd files

These will be compiled into html pages.

* index.Rmd
* \_poverty_chapters/\_chapter01.Rmd
* \_poverty_chapters/\_chapter02.Rmd
* \_poverty_chapters/\_chapter03.Rmd
* \_poverty_chapters/\_chapter04.Rmd
* \_poverty_chapters/\_chapter05.Rmd
* \_poverty_chapters/\_chapter06.Rmd
* \_poverty_chapters/\_chapter07.Rmd
* \_poverty_chapters/\_chapter08.Rmd
* \_poverty_chapters/\_chapter09.Rmd
* \_poverty_chapters/\_chapter10.Rmd
* \_poverty_chapters/\_chapter11.Rmd
* persistent.Rmd
* \_persistent_chapters/\_pers01.Rmd
* \_persistent_chapters/\_pers02.Rmd
* \_persistent_chapters/\_pers03.Rmd
* \_persistent_chapters/\_pers04.Rmd
* \_persistent_chapters/\_pers05.Rmd
* \_persistent_chapters/\_pers06.Rmd
* \_persistent_chapters/\_pers07.Rmd
* \_persistent_chapters/\_pers08.Rmd
* cpupdate.Rmd
* download.Rmd
* about.Rmd

### Helper files for styles, site- and navbar configurations

* styles.css
* \_site.yml
* \_header.html
* \_navbar.html

### Images and spreadsheets

* img/sglogo.svg
* img/badge.png
* img/chart0a.png
* img/chart0b.png
* img/chart0c.png
* img/cp1_relpovch_large.png
* img/cp2_abspovch_large.png
* img/cp3_matdepch_large.png
* img/cp4_perspov_large.png

Note that the spreadsheets are created in step 2.

* xls/Child poverty characteristics.xlsx
* xls/Income single year.xlsx
* xls/Income three-year average.xlsx
* xls/Poverty characteristics.xlsx
* xls/Poverty single year.xlsx
* xls/Poverty three-year average.xlsx


