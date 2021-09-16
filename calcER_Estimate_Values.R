# R code to calculate annual emissions(yr-1) for comparison with base FRL

# setwd("C:\eas-2018-prj\FijiGov\NFMSIntegrationFramework\code\calcs")

# Required source files
load(file = "./Data/fiji_frl_input.RData")
# Note all CalcFunctions return CO2e values



# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)



doCalc <- function(statusCallback, interrupted,calcEnv) {


  list2env(calcEnv,environment())

  checkStatus <- function(status) {
    # Check for user interrupts
    if (interrupted()) {
      print("Stopping...")
      stop("User Interrupt")
    }

    # Notify status file of progress
    statusCallback(status)
  }


  checkStatus(10)

  EmRems_Values <- list()
  EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
  EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)

  checkStatus(50)

  ER_Values <- CalcERValues(
   EmRems_Values,
   MonitoringReportParams$ErpaYearlyFRL,
   MonitoringReportParams$ErpaYearlyFRLFDeg
  )

  checkStatus(60)

  MR_Values <-
   create_EstMRValues(UC_ER_Values,
                      ER_Values,
                      EmRems_Values,
                      MonitoredValues,
                      MonitoringReportParams)

  checkStatus(90)
  Table4_2 <- createTable_4_2(MR_Values)

  Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)

  checkStatus(100)

  #Some results
  result <- list()

  result$env <-
    list(
      Table4_2 = Table4_2,
      Table4_3 = Table4_3,
      EmRems_Values = EmRems_Values,
      ER_Values = ER_Values,
      MonitoredValues = MonitoredValues,
      MonitoringReportParams = MonitoringReportParams
    )

  result$html <-list()
  result$html$Table4_2 <- as.tags(HTML(Table4_2 %>%
      kable("html") %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))
  result$html$Table4_3 <- as.tags(HTML(Table4_3 %>%
      kable("html") %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))



  return(result)
}