
rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')


# Changes to html files

for (file in c("index.html", "cpupdate.html", "download.html",
               "persistent.html", "about.html")) {

html_old <- readLines(paste0("./website/_site/", file))

# 1. Make navbar SG logo and Main report refer to the root and not to index.html
# (not really necessary but looks tidier in URL bar)

html_new <- gsub(pattern = 'href="index.html">',
                 replace = 'href="./">',
                 x = html_old)

# 2. Add skip-links for accessibility

html_new2 <- gsub(pattern = 'alt="Scottish Government logo"/></a>',
                  replace = 'alt="Scottish Government logo"/></a></div><div id="skiplink-container"><div><a href="#header" class="skiplink govuk-link">Skip to main content</a></div>',
                  x = html_new)

writeLines(html_new2, con = paste0("./website/_site/", file))

}


