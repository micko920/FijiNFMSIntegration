
# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

outputFilename <- "Fiji_ER_Estimate_Values"
outputSaveNames <- c(
  "AdjustedAreas",
  "ER_Values",
  "MR_Values",
  "EmRems_Values",
  "MonitoredValues",
  "MonitoringReportParams",
  "Table4_1_ReferenceLevel",
  "Table4_2",
  "Table4_3",
  "Table4_3_t1",
  "Table4_3_t2",
  "Table4_3_t5"
)



CalcER_Estimate_Values <- function(statusCallback, interrupted, calcEnv) {
  list2env(calcEnv, environment())

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

  ### REPORT TABLES
  ER_Values <- CalcERValues(
    EmRems_Values,
    paste0(MonitoredValues$year1$year, "-", MonitoredValues$year2$year),
    ErpaYearlyFRL$erpa_yearly$mp_frl,
    ErpaYearlyFRL$erpa_yearly$yearly[c("value", MonitoredValues$year1$year, MonitoredValues$year2$year)]
  )

  checkStatus(60)

  MR_Values <-
    create_EstMRValues(
      UC_ER_Values,
      ER_Values,
      EmRems_Values,
      MonitoredValues,
      MonitoringReportParams
    )

  checkStatus(90)
  
  Table4_1_ReferenceLevel <- createTable_4_1_ReferenceLevel(MR_Values)
  
  Table4_2 <- createTable_4_2(MR_Values)

  Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)
  
  Table4_3_t1 <- createTable_4_3_t1(MR_Values, MonitoringReportParams)
  
  Table4_3_t2 <- createTable_4_3_t2(MR_Values, MonitoringReportParams)
  
  Table4_3_t5 <- createTable_4_3_t5(MR_Values, MonitoringReportParams)

  checkStatus(100)

  # Some results
  result <- list()
  result$env <-
    list()
  result$env <- mget(outputSaveNames)
  return(result)
}