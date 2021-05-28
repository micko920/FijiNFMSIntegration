# Windows: Rtools needs to be installed. R 3.5 needs Rtools 3.5 not Rtools 4

install.packages(
  c(
    "pkgbuild",
    "callr",
    "data.table",
    "devtools",
    "ggplot2",
    "htmlwidgets",
    "knitr",
    "lintr",
    "magrittr",
    "markdown",
    "rlang",
    "rmarkdown",
    "roxygen2",
    "shiny",
    "stringi",
    "stringr",
    "styler",
    "testthat",
    "tinytex",
    "usethis",
    "utf8",
    "VGAM",
    "xfun",
    "timevis",
    "yaml",
    "devtools",
    "pillar",
    "pdftools"
  )
)

tinytex::install_tinytex()

devtools::install("../ValueWithUncertainty")
devtools::install("../MonteCarloUtils")
devtools::install("../FijiNFMSCalculations")


library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)
