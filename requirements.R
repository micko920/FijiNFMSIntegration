# Windows: Rtools needs to be installed. R 3.5 needs Rtools 3.5 not Rtools 4

packageVersion("rlang") # ‘1.0.6’
#devtools::install_github("r-lib/rlang")
#packageVersion("rlang") # ‘1.1.0.9000’


install.packages(
  c(
    "VGAM",
    "callr",
    "data.table",
    "devtools",
    "dplyr",
    "future",
    "ggplot2",
    "htmlwidgets",
    "kableExtra",
    "knitr",
    "lintr",
    "magrittr",
    "markdown",
    "pdftools",
    "pillar",
    "pkgbuild",
    "promises",
    "rlang",
    "rmarkdown",
    "roxygen2",
    "shiny",
    "shinyjs",
    "shinythemes",
    "shinyvalidate",
    "stringi",
    "stringr",
    "styler",
    "testthat",
    "timevis",
    "tinytex",
    "usethis",
    "utf8",
    "xfun",
    "yaml"
  )
)


#download.file(url = "https://cran.r-project.org/src/contrib/Archive/microbenchmark/microbenchmark_1.4-7.tar.gz" ,
#             destfile ="microbenchmark_1.4-7.tar.gz")
#install.packages("microbenchmark_1.4-7.tar.gz", repos = NULL, type="source")

install.packages("microbenchmark")



tinytex::install_tinytex(force = TRUE)

#devtools::install("../ValueWithUncertainty")
#devtools::install("../MonteCarloUtils")
#devtools::install("../FijiNFMSCalculations")

devtools::install_github("micko920/ValueWithUncertainty@v1.0.1")
devtools::install_github("micko920/MonteCarloUtils@v1.0.1")
devtools::install_github("micko920/FijiNFMSCalculations@v1.0.5")

library(shinyjs)
