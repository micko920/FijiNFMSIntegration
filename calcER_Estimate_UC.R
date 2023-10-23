
# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)



outputFilename <- "Fiji_ER_Estimate_UC"
outputSaveNames <- c(
  "ResultsTables",
  "EmRems_Values",
  "ER_Values",
  "MR_Values",
  "UC_Values",
  "UC_MV_Values",
  "UC_EmRems_Values",
  "MCRuns",
  "MCTolerance",
  "seed",
  "Table4_2",
  "Table4_3",
  "Table5_2_2",
  "Table7_2",
  "Table8"
)


### Start of Calc ####
CalcER_Estimate_UC <- function(statusCallback, interrupted, calcEnv) {
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


  ## MGG - patch for ARefor survey area to override Adjusted Areas sampled
  MonitoredValues$year1$AReforSurveyArea <- 616
  MonitoredValues$year2$AReforSurveyArea <- 667

  ## MGG - patch account for half year growth and compound growth of first year in 2nd year
  MonitoredValues$year2$AReforArea <- data.frame(
			year = c(2019, 2020),
			area_ha = c(
				MonitoredValues$year1$AReforSurveyArea,
				MonitoredValues$year2$AReforSurveyArea
			),
			age_yrs = c(0.5,1.5)
  )

  MonitoredValues$year1$AReforArea <- data.frame(
			year = c(2019),
			area_ha = c(
				MonitoredValues$year1$AReforSurveyArea
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
			age_yrs = c(0.5,1.5)
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
			age_yrs = c(0.5,1.5)
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


  checkStatus(10, "Calculating EmRem values....")

  # print(date())
  #
  EmRems_Values <- list()
  EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
  EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)
  #
  #
  checkStatus(30, "Calculating ER values....")
  # print(date())
  #
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

  checkStatus(50, "Calculating UC year 1 values....")
  # print(date())
  #
  UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)
  #
  #
  checkStatus(60, "Calculating UC year 2 values....")
  # print(date())
  #
  UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)
  #
  checkStatus(80, "Calculating UC ER values....")
  # print(date())
  #
  UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)
  #
  MR_Values <- create_MRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)


  Table4_2 <- createTable_4_2(MR_Values)

  Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)

  Table5_2_2 <- createTable_5_2_2(MR_Values)

  Table7_2 <- createTable_7_2(MR_Values, MonitoringReportParams)

  Table8 <- createTable_8(MR_Values, MonitoringReportParams)

  ResultsTables <- list()
  ResultsTables$year1 <- data.frame(
    stratum = c(
      "Deforestation",
      "Forest Deg (felling)",
      "Forest Deg (fire)",
      "Forest Plantations",
      "Sum Emissions",
      "Forest Deg (felling)",
      "Afforestation",
      "Forest Plantations",
      "Sum Removals",
      "Deforestation",
      "Forest Degradation",
      "Enhancement",
      "Total"
    ),
    Estimate = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year1$McEstEmFell$value[[1]],
      UC_EmRems_Values$year1$McEstEmFire$value[[1]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[1]],
      UC_EmRems_Values$year1$McGrossEm$value[[1]],
      UC_EmRems_Values$year1$McEstRemFell$value[[1]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[1]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[1]],
      UC_EmRems_Values$year1$McGrossRem$value[[1]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year1$McFDeg$value[[1]],
      UC_EmRems_Values$year1$McEnh$value[[1]],
      UC_EmRems_Values$year1$McNetEmRems$value[[1]]
    ),
    LCI = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year1$McEstEmFell$value[[2]],
      UC_EmRems_Values$year1$McEstEmFire$value[[2]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[2]],
      UC_EmRems_Values$year1$McGrossEm$value[[2]],
      UC_EmRems_Values$year1$McEstRemFell$value[[2]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[2]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[2]],
      UC_EmRems_Values$year1$McGrossRem$value[[2]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year1$McFDeg$value[[2]],
      UC_EmRems_Values$year1$McEnh$value[[2]],
      UC_EmRems_Values$year1$McNetEmRems$value[[2]]
    ),
    UCI = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year1$McEstEmFell$value[[3]],
      UC_EmRems_Values$year1$McEstEmFire$value[[3]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[3]],
      UC_EmRems_Values$year1$McGrossEm$value[[3]],
      UC_EmRems_Values$year1$McEstRemFell$value[[3]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[3]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[3]],
      UC_EmRems_Values$year1$McGrossRem$value[[3]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year1$McFDeg$value[[3]],
      UC_EmRems_Values$year1$McEnh$value[[3]],
      UC_EmRems_Values$year1$McNetEmRems$value[[3]]
    )
  )

  ResultsTables$year2 <- data.frame(
    stratum = c(
      "Deforestation", "Forest Deg (felling)", "Forest Deg (fire)",
      "Forest Plantations", "Sum Emissions", "Forest Deg (felling)", "Afforestation",
      "Forest Plantations", "Sum Removals", "Deforestation", "Forest Degradation", "Enhancement", "Total"
    ),
    Estimate = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year2$McEstEmFell$value[[1]],
      UC_EmRems_Values$year2$McEstEmFire$value[[1]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[1]],
      UC_EmRems_Values$year2$McGrossEm$value[[1]],
      UC_EmRems_Values$year2$McEstRemFell$value[[1]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[1]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[1]],
      UC_EmRems_Values$year2$McGrossRem$value[[1]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year2$McFDeg$value[[1]],
      UC_EmRems_Values$year2$McEnh$value[[1]],
      UC_EmRems_Values$year2$McNetEmRems$value[[1]]
    ),
    LCI = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year2$McEstEmFell$value[[2]],
      UC_EmRems_Values$year2$McEstEmFire$value[[2]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[2]],
      UC_EmRems_Values$year2$McGrossEm$value[[2]],
      UC_EmRems_Values$year2$McEstRemFell$value[[2]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[2]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[2]],
      UC_EmRems_Values$year2$McGrossRem$value[[2]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year2$McFDeg$value[[2]],
      UC_EmRems_Values$year2$McEnh$value[[2]],
      UC_EmRems_Values$year2$McNetEmRems$value[[2]]
    ),
    UCI = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year2$McEstEmFell$value[[3]],
      UC_EmRems_Values$year2$McEstEmFire$value[[3]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[3]],
      UC_EmRems_Values$year2$McGrossEm$value[[3]],
      UC_EmRems_Values$year2$McEstRemFell$value[[3]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[3]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[3]],
      UC_EmRems_Values$year2$McGrossRem$value[[3]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year2$McFDeg$value[[3]],
      UC_EmRems_Values$year2$McEnh$value[[3]],
      UC_EmRems_Values$year2$McNetEmRems$value[[3]]
    )
  )



  # Some results
  result <- list()
  result$env <-
    list()
  result$env <- mget(outputSaveNames)
  return(result)
}