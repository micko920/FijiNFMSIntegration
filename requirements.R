# Windows: Rtools needs to be installed. R 3.5 needs Rtools 3.5 not Rtools 4

packageVersion("rlang") # ‘1.0.6’
#devtools::install_github("r-lib/rlang")
#packageVersion("rlang") # ‘1.1.0.9000’


install.packages(c("httr"))

packageurl <- "https://cran.r-project.org/src/contrib/Archive/gh/gh_1.3.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

packageurl <- "https://cran.r-project.org/src/contrib/Archive/usethis/usethis_2.2.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")



install.packages(
  c(
    "covr","DT"
  )
)

packageurl <- "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_2.3.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")




install.packages(
  c(
    "callr",
    "data.table",
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
devtools::install_version("survey","4.2-1",upgrade = c("never"))
devtools::install_version("pkgload","1.4.1",upgrade = c("never"))
devtools::install_version("xfun","0.55",upgrade = c("never"))

tinytex::install_tinytex(force = TRUE)


devtools::install("../ValueWithUncertainty",dependencies = FALSE)
devtools::install("../MonteCarloUtils",dependencies = FALSE)
devtools::install("../FijiNFMSCalculations",dependencies = FALSE)

#devtools::install_github("micko920/ValueWithUncertainty@v1.0.1")
#devtools::install_github("micko920/MonteCarloUtils@v1.0.1")
#devtools::install_github("micko920/FijiNFMSCalculations@v1.0.5")

library(shinyjs)
Sys.info()
sessionInfo()

str(allPackage <- installed.packages())
allPackage[order(allPackage[,1]),c(1,3:5)]
