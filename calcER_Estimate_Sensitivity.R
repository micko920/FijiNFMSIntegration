

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


  ## MGG - patch account for half year growth and compound growth of first year in 2nd year
  MonitoredValues$year2$AReforArea <- data.frame(
    year = c(2019, 2020),
    area_ha = c(
      AdjustedAreas$MCaaaforMean,
      AdjustedAreas$MCaaaforMean
    ),
    age_yrs = c(0.5,1.5)
  )
  
  MonitoredValues$year1$AReforArea <- data.frame(
    year = c(2019),
    area_ha = c(
      AdjustedAreas$MCaaaforMean
    ),
    age_yrs = c(0.5)
  )

  ## MGG - patch for FDeg survey area
  ## MGG - patch account for half year growth and compound growth of first year in 2nd year
  MonitoredValues$year2$FDegFellArea <- data.frame(
			year = c(2019, 2020),
			area_ha = c(
				MonitoredValues$year1$FDegFellAreaSurveyArea,
				MonitoredValues$year2$FDegFellAreaSurveyArea
			),
			age_yrs = c(0.5,1.5)
  )

  MonitoredValues$year1$FDegFellArea <- data.frame(
			year = c(2019),
			area_ha = c(
				MonitoredValues$year1$FDegFellAreaSurveyArea
			),
			age_yrs = c(0.5)
  )

  ## MGG - patch for FPlnAreaPlantHwd survey area
  ## MGG - patch account for half year growth and compound growth of first year in 2nd year
  MonitoredValues$year2$FPlnAreaPlantHwd <- data.frame(
			year = c(2019, 2020),
			area_ha = c(
				MonitoredValues$year1$FPlnAreaPlantHwdSurveyArea,
				MonitoredValues$year2$FPlnAreaPlantHwdSurveyArea
			),
			age_yrs = c(1.5,0.5)
  )

  MonitoredValues$year1$FPlnAreaPlantHwd <- data.frame(
			year = c(2019),
			area_ha = c(
				MonitoredValues$year1$FPlnAreaPlantHwdSurveyArea
			),
			age_yrs = c(0.5)
  )

  ## MGG - patch for FPlnAreaPlantSwd survey area
  ## MGG - patch account for half year growth and compound growth of first year in 2nd year
  MonitoredValues$year2$FPlnAreaPlantSwd <- data.frame(
			year = c(2019, 2020),
			area_ha = c(
				MonitoredValues$year1$FPlnAreaPlantSwdSurveyArea,
				MonitoredValues$year2$FPlnAreaPlantSwdSurveyArea
			),
			age_yrs = c(1.5,0.5)
  )

  MonitoredValues$year1$FPlnAreaPlantSwd <- data.frame(
			year = c(2019),
			area_ha = c(
				MonitoredValues$year1$FPlnAreaPlantSwdSurveyArea
			),
			age_yrs = c(0.5)
  )

  # Eric number 428.5581 ha area of degradation activities (standard error of 88.5538 ha).
  # Period for 2019-2020 so divide by 2
  # FCPF uses 90% CI (5% to 95%)
  ## MGG - patch for new data for NFDeg
  MonitoredValues$year1$NFDegArea <- 428.5581 / 2
  MonitoredValues$year1$NFDegArea_LCI <- (428.5581 / 2) - (qnorm(0.95) * 88.5538 / sqrt(2))
  MonitoredValues$year1$NFDegArea_UCI <- (428.5581 / 2) + (qnorm(0.95) * 88.5538 / sqrt(2))
  MonitoredValues$year2$NFDegArea <- 428.5581 / 2
  MonitoredValues$year2$NFDegArea_LCI <- (428.5581 / 2) - (qnorm(0.95) * 88.5538 / sqrt(2))
  MonitoredValues$year2$NFDegArea_UCI <- (428.5581 / 2) + (qnorm(0.95) * 88.5538 / sqrt(2))

  MonitoringReportParams$ErpaYearlyFRL <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","UCI"]
  MonitoringReportParams$ErpaYearlyFRLLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","LCI"]
  MonitoringReportParams$ErpaYearlyFRLFDeg <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLFDegUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","UCI"]
  MonitoringReportParams$ErpaYearlyFRLFDegLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","LCI"]
  MonitoringReportParams$ErpaYearlyFRLDefor <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLDeforUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","UCI"]
  MonitoringReportParams$ErpaYearlyFRLDeforLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","LCI"]
  MonitoringReportParams$ErpaYearlyFRLEnh <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLEnhUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","UCI"]
  MonitoringReportParams$ErpaYearlyFRLEnhLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","LCI"]
  MonitoringReportParams$ErpaYearlyFRLFDegNonProxy <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLFDegNonProxyUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","UCI"]
  MonitoringReportParams$ErpaYearlyFRLFDegNonProxyLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","LCI"]


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
    MonitoringReportParams$ErpaYearlyFRLFDeg,
    MonitoringReportParams$ErpaYearlyFRLDefor,
    MonitoringReportParams$ErpaYearlyFRLEnh,
    MonitoringReportParams$ErpaYearlyFRLFDegNonProxy 
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
