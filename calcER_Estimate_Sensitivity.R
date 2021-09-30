

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)

outputFilename <- "Fiji_ER_Estimate_Sensitivity"
outputSaveNames <- c(
  "TEI_ValuesOrdered"
)



### Start of Calc ####
CalcER_Estimate_Sensitivity <- function(statusCallback, interrupted, calcEnv) {
  MCRuns <<- calcEnv$MCRuns
  MCTolerance <<- calcEnv$MCTolerance

  list2env(calcEnv, environment())

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



  checkStatus <- function(status, notification) {
    # Check for user interrupts
    if (interrupted()) {
      print("Stopping...")
      stop("User Interrupt")
    }

    # Notify status file of progress
    statusCallback(status, notification)
  }



  formatDecimal <- function(x) {
    return(format(round(x, 4), nsmall = 4))
  }

  checkStatus(10, "Calculating EmRem values....")

  EmRems_Values <- list()
  EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
  EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)


  checkStatus(11, "Calculating ER values....")

  ER_Values <- CalcERValues(
    EmRems_Values,
    MonitoringReportParams$ErpaYearlyFRL,
    MonitoringReportParams$ErpaYearlyFRLFDeg
  )

  UC_Values <- list()
  UC_Values <- createUC_Values(MonitoringReportParams)


  UC_MV_Values <- list()
  UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
  UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)


  UC_EmRems_Values <- list()

  checkStatus(21, "Calculating UC year 1 values....")

  UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)


  checkStatus(25, "Calculating UC year 2 values....")

  UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)

  checkStatus(30, "Calculating UC ER values....")
  UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)

  ###
  # Total Effect Index of value (x) is  1 minus the variance effect of all
  # other variables.

  calcTEI <- function(newSamples, oldVar) {
    return(1 - (var(newSamples) / oldVar))
  }


  V_all <- var(UC_ER_Values$McMpEstERsDefEnh$MCresults)

  TEI_Values <- list()

  TEI_Values$params <- data.frame(
    name = character(length(UC_Values)),
    v = numeric(length(UC_Values)),
    stringsAsFactors = FALSE
  )

  totalSteps <- (length(UC_Values) + length(UC_MV_Values$year1))
  progressStep <- 60 / totalSteps
  currProgress <- 40

  for (i in 1:length(UC_Values)) {
    UC_Values[[i]] <- ValueWithUncertaintyFixed(UC_Values[[i]])

    UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
    UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)

    UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)

    UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)

    UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)

    TEI_Values$params$name[i] <- names(UC_Values[i])
    TEI_Values$params$v[i] <- formatDecimal(calcTEI(UC_ER_Values$McMpEstERsDefEnh$MCresults, V_all))
    UC_Values[[i]] <- ValueWithUncertaintySampled(UC_Values[[i]])


    checkStatus(currProgress, paste("Sensitivty Analysis for ", TEI_Values$params$name[i], " complete"))
    currProgress <- currProgress + progressStep
  }

  TEI_Values$activityData <- data.frame(
    name = character(length(UC_MV_Values$year1)),
    v = numeric(length(UC_MV_Values$year1)),
    stringsAsFactors = FALSE
  )

  for (i in 1:length(UC_MV_Values$year1)) {
    UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
    UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)

    UC_MV_Values$year1[[i]] <- ValueWithUncertaintyFixed(UC_MV_Values$year1[[i]])
    UC_MV_Values$year2[[i]] <- ValueWithUncertaintyFixed(UC_MV_Values$year2[[i]])


    UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)

    UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)

    UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)

    TEI_Values$activityData$name[i] <- names(UC_MV_Values$year1[i])
    TEI_Values$activityData$v[i] <- formatDecimal(calcTEI(UC_ER_Values$McMpEstERsDefEnh$MCresults, V_all))

    UC_MV_Values$year1[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year1[[i]])
    UC_MV_Values$year2[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year2[[i]])

    checkStatus(currProgress, paste("Sensitivty Analysis for ", TEI_Values$activityData$name[i], " complete"))
    currProgress <- currProgress + progressStep
  }

  TEI_ValuesParamslessFRL <- data.frame(TEI_Values$params[-c(16), ])
  TEI_ValuesForFIMS <- rbind(TEI_ValuesParamslessFRL, TEI_Values$activityData)
  TEI_ValuesOrdered <- TEI_ValuesForFIMS[order(TEI_ValuesForFIMS[, 1]), ]


  # Some results
  result <- list()
  result$env <-
    list()
  result$env <- mget(outputSaveNames)
  return(result)
}
