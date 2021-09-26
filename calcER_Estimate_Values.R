
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
  "Table4_2",
  "Table4_3"
)

source("./FRL_VALUE_FIX.R")

CalcER_Estimate_Values <- function(statusCallback, interrupted, calcEnv) {
  list2env(calcEnv, environment())

  # AA of AD is done on a monitoring period of 2 years.
  # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
  MonitoredValues$year1$DeforAreaLow <- AdjustedAreas$areaLoss[1]
  MonitoredValues$year1$McDeforAreaLow <- AdjustedAreas$MCaadeforL
  MonitoredValues$year2$DeforAreaLow <- AdjustedAreas$areaLoss[1]
  MonitoredValues$year2$McDeforAreaLow <- AdjustedAreas$MCaadeforL
  # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
  MonitoredValues$year1$DeforAreaUp <- AdjustedAreas$areaLoss[2]
  MonitoredValues$year1$McDeforAreaUp <- AdjustedAreas$MCaadeforU
  MonitoredValues$year2$DeforAreaUp <- AdjustedAreas$areaLoss[2]
  MonitoredValues$year2$McDeforAreaUp <- AdjustedAreas$MCaadeforU
  # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
  # AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  # AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  MonitoredValues$year1$AReforArea <- AdjustedAreas$MCaaaforMean
  MonitoredValues$year1$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
  MonitoredValues$year2$AReforArea <- AdjustedAreas$MCaaaforMean
  MonitoredValues$year2$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)



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
    create_EstMRValues(
      UC_ER_Values,
      ER_Values,
      EmRems_Values,
      MonitoredValues,
      MonitoringReportParams
    )

  checkStatus(90)
  Table4_2 <- createTable_4_2(MR_Values)

  Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)

  checkStatus(100)

  # Some results
  result <- list()
  result$env <-
    list()
  result$env <- mget(outputSaveNames)
  return(result)
}