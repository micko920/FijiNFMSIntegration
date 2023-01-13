
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
  MonitoredValues$year1$NFDegArea_LCI <- (428.5581 / 2 - (qnorm(0.95) * 88.5538 / sqrt(2)))
  MonitoredValues$year1$NFDegArea_UCI <- (428.5581 / 2 + (qnorm(0.95) * 88.5538 / sqrt(2)))
  MonitoredValues$year2$NFDegArea <- 428.5581 / 2
  MonitoredValues$year2$NFDegArea_LCI <- (428.5581 / 2 - (qnorm(0.95) * 88.5538 / sqrt(2)))
  MonitoredValues$year2$NFDegArea_UCI <- (428.5581 / 2 + (qnorm(0.95) * 88.5538 / sqrt(2)))

  MonitoringReportParams$ErpaYearlyFRL <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLFDeg <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLDefor <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLEnh <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","MP_FRL"]
  MonitoringReportParams$ErpaYearlyFRLFDegNonProxy <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","MP_FRL"]


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

  browser()
  ER_Values <- CalcERValues(
    EmRems_Values,
    MonitoringReportParams$ErpaYearlyFRL,
    MonitoringReportParams$ErpaYearlyFRLFDeg,
    MonitoringReportParams$ErpaYearlyFRLDefor,
    MonitoringReportParams$ErpaYearlyFRLEnh
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