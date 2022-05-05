# Knit the Rmd file to the correct directory for github pages
rmarkdown::render('current-ice-detention-population.Rmd',
                  output_file = 'docs/current-ice-detention-population.html')