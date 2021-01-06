
# restart R before running to ensure svg charts are indexed "svg1-..."

rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')



