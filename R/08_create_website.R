

rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')



# Change all html files so that they refer to the root and not to index.html
# (not really necessary but looks tidier in URL bar)

for (file in c("index.html", "cpupdate.html", "download.html",
               "persistent.html", "about.html")) {

html_old <- readLines(paste0("./website/_site/", file))
html_new <- gsub(pattern = 'href="index.html">',
                 replace = 'href="./">',
                 x = html_old)
writeLines(html_new, con = paste0("./website/_site/", file))

}
