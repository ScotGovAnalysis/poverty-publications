# About

This repository contains the code for the annual Scottish Government "Poverty an Income Inequality in Scotland" report, the "Persistent Poverty in Scotland" report, a child poverty summary and the accompanying reference tables. It produces a folder website/\_site with all files for the website (including downloadable spreadsheets) hosted on data.gov.scot/poverty.

Running this project requires access to restricted Scottish Government datasets, which is only available to a small number of individuals who are responsible for producing the annual poverty reports. However, we hope that the code may still be useful.

# Workflow

### 0. Files missing in github

All data files have been removed. Also, absolute paths have been removed in R scripts.

### 1. Import data and prepare datasets

* Ensure the inflation_index file in the SOCJUST SAS library is up to date (DWP provide latest CPIH inflators)
* In R/00_strings.R, add latest year and period values to labels\[\["years"]].
* Run all [Data prep](#dataprep) scripts.
* Make sure 02_clean...data.R and 03_maketidydatasets.R scripts return no errors. 

Tidy datasets are now available in the data folder to produce reports and spreadsheets.

### 2. Create everything

* Run 07_run_all.R to create all spreadsheets and the website. Alternatively, all scripts can also be run individually and independently once previous scripts have been run before and produced the necessary intermediate datasets.
* Look at (local version of) website: website/\_site/index.html
* Check and update all commentary in:

  * website/index.Rmd
  * website/\_poverty_chapters/...
  * website/persistent.Rmd
  * website/\_persistent_chapters/...
  * website/cpupdate.Rmd
  * website/download.Rmd
  * website/accessibility.Rmd
  * website/uncertainty.Rmd
  * website/contact.Rmd

  Most of the figures in the commentary are automatically produced; however, the text is not.

* Run step 8 (render website) in R/07_run_all.R again, inspect website, make required changes to the commentary in the .Rmd files, and repeat until happy.

# List of scripts

### Helpers

* R/00_colours.R (colour palettes)
* R/00_functions.R
* R/00_strings.R (list of formatted years and periods, categories, etc.)

### Data prep<a name="dataprep"></a>

* R/01_importSASfiles.R - takes a very long time to run, maybe do at night; script not included on GitHub
* R/01_importpersistentpovertydata.R - requires persistent poverty data being supplied in the typical format; script not included on GitHub
* R/02_cleanadultdata.R
* R/02_cleanbenefitsdata.R
* R/02_cleanchilddata.R
* R/02_cleanchldcaredata.R
* R/02_cleanhbaidata.R
* R/02_cleanhouseholdata.R
* R/03_maketidydatasets.R - combines various HBAI and FRS datasets into two tidy datasets: one adult-level and one benefit unit-level

### Aggregate data

* R/04_aggregate_1yr_Scot.R
* R/04_aggregate_3yrs_Scot.R

### Create charts and tables included in reports

* R/04_tables_persistent.R
* R/04_tables_uncertainty.R
* R/05_charts_3yrs_Scot.R
* R/05_charts_cpupdate.R
* R/05_charts_persistent.R 

### Create spreadsheets

* R/05_xlsx_1yr_Scot.R - single-year estimates
* R/05_xlsx_3yrs_Scot.R - three-year averaged data
* R/05_xlsx_persistent.R
* R/05_xlsx_1yr_UK.R - UK comparisons for briefing (not included as not required for website)
* R/05_xlsx_3yrs_UK.R - UK comparisons for briefing (not included as not required for website)
* R/06_xlsx_postprocessing.R - VBA code to finalise spreadsheets

Spreadsheets are saved in output folder.

### Create website

* R/07_run_all.R - runs all previous scripts and then creates complete website in website/\_site.

### Backup

Backups are now done in git in the restricted 'Poverty/Prerelease' datashare folder.

# List of files in website folder

### Rmd files

These will be compiled into seven html pages.

* index.Rmd
* \_poverty_chapters/\_chapter0.Rmd
* \_poverty_chapters/\_chapter1.Rmd
* \_poverty_chapters/\_chapter2.Rmd
* \_poverty_chapters/\_chapter3.Rmd
* \_poverty_chapters/\_chapter4.Rmd
* \_poverty_chapters/\_chapter5.Rmd
* persistent.Rmd
* \_persistent_chapters/\_pers0.Rmd
* \_persistent_chapters/\_pers1.Rmd
* \_persistent_chapters/\_pers2.Rmd
* \_persistent_chapters/\_pers3.Rmd
* accessibility.Rmd
* contact.Rmd
* cpupdate.Rmd
* download.Rmd
* uncertainty.Rmd

### Helper files for styles, site- and navbar configurations, and template

* styles.css
* \_site.yml
* \_footer.html
* \_navbar.html
* \_template.html - this pandoc template differs from the default in how the main html blocks (nav, main, footer) are configured

### Images and spreadsheets

* img/sglogo.svg
* img/badge.png
* img/chart0a.png
* img/chart0b.png
* img/chart0c.png

Note that the spreadsheets are created in step 2.

* xls/data2022.xlsx
* xls/data2022.ods
* xls/data2022_1yr.xlsx
* xls/data2022_1yr.ods
* xls/data2022_persistent.xlsx
* xls/data2022_persistent.ods

