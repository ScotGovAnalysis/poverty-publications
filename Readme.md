# Readme

## About

This repository contains the code for the annual Scottish Government "Poverty and Income Inequality in Scotland" report, the "Persistent Poverty in Scotland" report, a child poverty summary and the accompanying reference tables. It produces a folder **website/\_site** with all files for the website (including downloadable spreadsheets) hosted on [data.gov.scot/poverty](https://data.gov.scot/poverty).

This project does not run.

Running this project requires access to restricted Scottish Government datasets, which are only available to a small number of individuals within the Scottish Government who are responsible for producing the annual poverty reports. However, we hope that the code may still be useful.

## Workflow

### 0. Files missing in github

All **data** files have been removed. Also, absolute paths have been redacted in R scripts.

### 1. Import data and prepare datasets

* Ensure the **inflation_index.xlsx** file in the SOCJUST SAS library is up to date (DWP provide latest CPIH inflators)
* In **R/00_strings.R**, add latest year and period values to labels$years.
* Run all [Data prep](#dataprep) scripts one by one.
* Make sure **02_clean...data.R** and **03_maketidydatasets.R** scripts return no errors.

Tidy datasets are now available in the **data** folder to produce reports and spreadsheets.

### 2. Create everything

* Run **000_RUN_ALL.R** to create all spreadsheets and the website. Alternatively (and essential whenever new data becomes available), all scripts can also be run individually and independently once previous scripts have been run before and produced the necessary intermediate datasets.
* Look at (local version of) website: **website/\_site/index.html** and the other six web pages created
* Check and update all commentary in:

  * **website/index.Rmd**
  * **website/\_poverty_chapters/**...
  * **website/persistent.Rmd**
  * **website/\_persistent_chapters/**...
  * **website/cpupdate.Rmd**
  * **website/download.Rmd**
  * **website/accessibility.Rmd**
  * **website/uncertainty.Rmd**
  * **website/contact.Rmd**

Most of the figures in the commentary are automatically produced; however, the text is not.

* Run step 7 (render website) in **R/000_RUN_ALL.R** again, inspect website, make required changes to the commentary in the .Rmd files, and repeat until happy.

## Required R packages

Any R packages required in each script are loaded at the beginning of the script. Below is an exhaustive list of standard packages used as well as one bespoke package (analysistools).

- tidyverse
- openxlsx (create Excel files)
- readxl (import Excel files)
- scales (format numbers)
- Hmisc (weighted quantiles)
- haven (import SAS files)
- survey (calculate indicative confidence intervals)
- highcharter (interactive charts)
- analysistools (rounding, create n-year averages, see below)

### analysistools package

This contains the functions getrollingmean, getrollingtotal, round2, wtd.median, getdeciles and getquintiles (and zipsave, which is no longer used in this project). The package is available on [github](https://github.com/DataScienceScotland/analysistools), where is can be installed from, or alternatively, the functions can be copied from.

Note that the base R function round performs a different kind of rounding compared to Excel. analysistools::round2 is equivalent to Excel rounding, i.e. rounding 0.5 always rounds up, and rounding 0.499.. always rounds down.

## List of files

* **R_poverty_publications.Rproj** - project file
* **Readme.md** - this file
* **data** folder - contains imported Excel datasets and intermediate .rds datasets
* **R** folder - contains all R scripts, see [section below](#scripts)
* **website** folder - contains all .Rmd files and helper files to create the website, see [section below](#websitefiles)

Files required for VBA postprocessing:

* **Run Excel Macro.vbs**
* **vba_for_postprocessing.xlsm**

## List of scripts in **R** folder<a name="scripts"></a>

### Helpers

These scripts are sourced at the beginning of any of the later scripts if required.

* **R/00_colours.R** (colour palettes)
* **R/00_functions.R**
* **R/00_strings.R** (list of formatted years and periods, categories, etc.)

### Data prep<a name="dataprep"></a>

* **R/01_importSASfiles.R** - takes hours to run if all files need downloading, maybe do at night
* **R/01_importpersistentpovertydata.R** - requires persistent poverty data being supplied in the typical format
* **R/02_cleanadultdata.R**
* **R/02_cleanbenefitsdata.R**
* **R/02_cleanchilddata.R**
* **R/02_cleanchldcaredata.R**
* **R/02_cleanhbaidata.R**
* **R/02_cleanhouseholdata.R**
* **R/03_maketidydatasets.R** - combines various HBAI and FRS datasets into three tidy datasets: an adult-level, a child-level and a benefit unit-level dataset.

### Aggregate data

* **R/04_aggregate_1yr_Scot.R**
* **R/04_aggregate_3yrs_Scot.R**

### Calculate indicative confidence intervals

* **R/04_indicative_CIs.R**

### Create charts and tables included in reports

* **R/04_tables_persistent.R**
* **R/04_tables_uncertainty.R**
* **R/05_charts_3yrs_Scot.R**
* **R/05_charts_cpupdate.R**
* **R/05_charts_persistent.R**

### Create spreadsheets

* **R/05_xlsx_1yr_Scot.R** - single-year estimates
* **R/05_xlsx_3yrs_Scot.R** - three-year averaged data
* **R/05_xlsx_persistent.R** - persistent poverty
* **R/06_xlsx_postprocessing.R** - VBA code to finalise spreadsheets

Spreadsheets are saved in the **website/xls** folder.

### Run all

* **R/000_RUN_ALL.R** - runs the scripts above after importing the data (i.e. all scripts starting with 02_ to 06_) and then also builds the complete website in **website/\_site**.

## List of files in **website** folder<a name="websitefiles"></a>

### Rmd files

These will be compiled into seven html pages.

* **website/index.Rmd**
  * **website/\_poverty_chapters/\_chapter0.Rmd**
  * **website/\_poverty_chapters/\_chapter1.Rmd**
  * **website/\_poverty_chapters/\_chapter2.Rmd**
  * **website/\_poverty_chapters/\_chapter3.Rmd**
  * **website/\_poverty_chapters/\_chapter4.Rmd**
  * **website/\_poverty_chapters/\_chapter5.Rmd**
  * **website/\_poverty_chapters/\_chapter6.Rmd**
  * **website/\_poverty_chapters/\_chapter7.Rmd**
* **website/persistent.Rmd**
  * **website/\_persistent_chapters/\_pers0.Rmd**
  * **website/\_persistent_chapters/\_pers1.Rmd**
  * **website/\_persistent_chapters/\_pers2.Rmd**
  * **website/\_persistent_chapters/\_pers3.Rmd**
* **website/accessibility.Rmd**
* **website/contact.Rmd**
* **website/cpupdate.Rmd**
* **website/download.Rmd**
* **website/uncertainty.Rmd**

### Helper files for styles, site- and navbar configurations, and template

* **website/styles23.css**
* **website/\_site.yml**
* **website/\_footer.html**
* **website/\_navbar.html**
* **website/\_template.html** - this pandoc template differs from the default in how the main html blocks (nav, main, footer) are configured

### Images and spreadsheets

* **website/img/sglogo.svg** - Scottish Government logo
* **website/img/badge.png** - National Statistics badge

Small charts in 'Key trends' section of the Poverty and Income Inequality in Scotland report that link to the relevant report sections:

* **website/img/chart0a.png**
* **website/img/chart0b.png**
* **website/img/chart0c.png**

Note that the spreadsheets are created in step 2.

* **website/xls/data2023.xlsx**
* **website/xls/data2023.ods**
* **website/xls/data2023_1yr.xlsx**
* **website/xls/data2023_1yr.ods**
* **website/xls/data2023_persistent.xlsx**
* **website/xls/data2023_persistent.ods**

## Backups

Backups are now done in git, with a remote repository in the restricted **Communities/Income and Wealth/git remotes** ADM folder.
