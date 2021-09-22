# Windows: Rtools needs to be installed. R 3.5 needs Rtools 3.5 not Rtools 4

install.packages(
  c(
    "VGAM",
    "callr",
    "data.table",
    "devtools",
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



tinytex::install_tinytex()

devtools::install("../ValueWithUncertainty")
devtools::install("../MonteCarloUtils")
devtools::install("../FijiNFMSCalculations")

#devtools::install_github("micko920/ValueWithUncertainty@v1.0.0-alpha")
#devtools::install_github("micko920/MonteCarloUtils@v1.0.0-alpha")
#devtools::install_github("micko920/FijiNFMSCalculations@v1.0.0-alpha")

library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)


install.packages('microbenchmark')