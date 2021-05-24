

install.packages(
  c(
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
    "yaml"
  )
)

devtools::install("../ValueWithUncertainty")
devtools::install("../MonteCarloUtils")
devtools::install("../FijiNFMSCalculations")
