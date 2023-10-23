# Windows: Rtools needs to be installed. R 3.5 needs Rtools 3.5 not Rtools 4

packageVersion("rlang") # ‘1.0.6’
#devtools::install_github("r-lib/rlang")
#packageVersion("rlang") # ‘1.1.0.9000’


install.packages(
  c(
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
    "microbenchmark",
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

devtools::install_version("VGAM","1.1.7")
devtools::install_version("microbenchmark","1.4.7")

tinytex::install_tinytex(force = TRUE)

devtools::install("../ValueWithUncertainty",dependencies = FALSE)
devtools::install("../MonteCarloUtils",dependencies = FALSE)
devtools::install("../FijiNFMSCalculations",dependencies = FALSE)

#devtools::install_github("micko920/ValueWithUncertainty@v1.0.1")
#devtools::install_github("micko920/MonteCarloUtils@v1.0.1")
#devtools::install_github("micko920/FijiNFMSCalculations@v1.0.5")

library(shinyjs)
