# Workflow

### New data arrives

Run **Data prep** scripts and make sure 02_clean...data.R and 03_maketidydatasets.R scripts return no errors. Tidy datasets are now available to produce reports and spreadsheets.

### Create spreadsheets

Run 05_run_maketables.R to create all spreadsheets.

### Create charts

* Run ... to create charts for report.
* Run ... to create charts for child poverty update

### Create doc reports for briefing and download purposes

* Run ... to create main report docx .
* Run ... to create Child poverty update docx (ensure template has correct publication date).

### Create website



# Scripts

### Helpers

* R/00_colours.R
* R/00_functions.R
* R/00_strings.R

### Data prep

* R/01_importSASfiles.R (not included in Git)
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
* R/05_run_maketables.R runs all

### Create charts

* R/...

### Create reports

* Rmd/...
