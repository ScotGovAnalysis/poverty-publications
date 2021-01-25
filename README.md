# About

This repository contains the code for the annual Scottish Government "Poverty an Income Inequality in Scotland" report, the "Persistent Poverty in Scotland" report, the accompanying associated and supplementary tables, and the infographic child poverty update. It produces a folder website/\_site with all files for a website, including downloadable spreadsheets, to be hosted on data.gov.scot/poverty.

# Folder structure



# Workflow

### 0. Files missing in github

The following R files are missing from the github repository, because they contain absolute path names, which are potentially sensitive. These files import data from SAS datasets (FRS and HBAI data, which have access restricted to a few named users) and an Excel spreadsheet (persistent poverty data) and save it as .rds files.

* R/01_importSASfiles.R
* R/01_importnewSASfiles.R
* R/01_importpersistentpovertydata.R

### 1. New data arrives in the SOCJUST SAS library

* Ensure the inflation_index file in the SOCJUST SAS library is up to date
* In R/00_strings.R, add latest year and period values to labels\[\["years"]].
* Run all [Data prep](#dataprep) scripts except for R/01_importSASfiles.R (this imports the previous years' datasets)
* Make sure 02_clean...data.R and 03_maketidydatasets.R scripts return no errors. 

Tidy datasets are now available to produce reports and spreadsheets.

### 2. Create everything

**Run 08_create_website.R to create all spreadsheets and the website.**

(Alternatively, all scripts from R/04_... to R_07_... can also be run individually.)

* Look at (local version of) website: website/\_site/index.html
* Check and update all commentary in:

  * website/index.Rmd
  * website/\_poverty_chapters/...
  * website/cpupdate.Rmd
  * website/download.Rmd
  * website/accessibility.Rmd
  * website/persistent.Rmd
  * website/\_persistent_chapters/...

* Run last line of R/08_create_website.R again, inspect website, make required changes to the .Rmd files, and repeat until happy

### 4. Create doc reports for briefing and download purposes - TO DO

* Run ... to create main report docx .

# List of scripts

### Helpers

* R/00_colours.R
* R/00_functions.R
* R/00_functions_rmd.R
* R/00_strings.R

### Data prep<a name="dataprep"></a>

* R/01_importSASfiles.R - takes a very long time to run, maybe do at night; script not included on GitHub
* R/01_importnewSASfiles.R - takes a while to run, maybe do at night; script not included on GitHub
* R/01_importpersistentpovertydata.R - depends on persistent poverty data being supplied in a certain format; script not included on GitHub
* R/02_cleanadultdata.R
* R/02_cleanbenefitsdata.R
* R/02_cleanchilddata.R
* R/02_cleanhbaidata.R
* R/02_cleanhouseholdata.R
* R/03_maketidydatasets.R

### Create spreadsheets

* R/04_maketables_headlines_1yr.R
* R/04_maketables_income_1yr.R
* R/04_maketables_all_3yr.R
* R/04_maketables_UK_3yrs.R
* R/05_run_maketables.R - runs all maketables (R/04_...) scripts and copies spreadsheets (for publication) into website folder

### Create charts and tables included in reports

* R/06_prepcharts_cpupdate.R
* R/06_prepcharts_poverty.R
* R/07_maketables_persistentpoverty.R
* R/07_makecharts_persistentpoverty.R
* R/07_maketables_poverty.R
* R/07_makecharts_cpupdate.R
* R/07_makecharts_poverty.R

### Create website

* R/08_create_website.R - runs spreadsheets, charts and tables and then creates complete website in website/\_site

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
* cpupdate.Rmd
* download.Rmd
* accessibility.Rmd

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

Note that the spreadsheets are created in step 2.

* xls/All three-year average.xlsx
* xls/All single year.xlsx

