# Knit the Rmd file to the correct directory for github pages
rmarkdown::render('index.Rmd',
                  output_file = 'docs/index.html')