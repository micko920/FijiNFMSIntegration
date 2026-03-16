


source("./getDataPath.R")
load(file = getDataPath("Fiji_ER_Estimate_Params.RData"))


# Eric number 428.5581 ha area of degradation activities (standard error of 88.5538 ha).
# Period for 2019-2020 so divide by 2
# FCPF uses 90% CI (5% to 95%)
## MGG - patch for new data for NFDeg
# MonitoredValues$year1$NFDegArea <- 428.5581 / 2
# MonitoredValues$year1$NFDegArea_LCI <- (428.5581 / 2 - (qnorm(0.95) * 88.5538 / sqrt(2)))
# MonitoredValues$year1$NFDegArea_UCI <- (428.5581 / 2 + (qnorm(0.95) * 88.5538 / sqrt(2)))
# MonitoredValues$year2$NFDegArea <- 428.5581 / 2
# MonitoredValues$year2$NFDegArea_LCI <- (428.5581 / 2 - (qnorm(0.95) * 88.5538 / sqrt(2)))
# MonitoredValues$year2$NFDegArea_UCI <- (428.5581 / 2 + (qnorm(0.95) * 88.5538 / sqrt(2)))

MonitoredValues <- list()

MonitoredValues$year <- list()


####################################################################################
## Monitored Values: 2019 to 2020
####################################################################################

MonitoredValues$year[['2019']] <- list(
      year = 2019L,
      FPlnVolHarvHwd = 21253,
      FPlnAreaStockHwd = 40909L,
      FPlnAreaPlantHwd = structure(
        list(
          area_ha = 4008,
          age_yrs = 0.5
        ),
        class = "data.frame",
        row.names = c(NA,-1L)
      ),
      FPlnAreaHarvHwd = 104L,
      FPlnAreaJustGrowsHwd = 40805L,
      FPlnVolHarvSwd = 419088,
      FPlnAreaStockSwd = 24698L,
      FPlnAreaPlantSwd = structure(
        list(
          area_ha = 2008,
          age_yrs = 0.5
        ),
        class = "data.frame",
        row.names = c(NA,-1L)
      ),
      FPlnAreaHarvSwd = 909L,
      FPlnAreaJustGrowsSwd = 23789L,
      FDegFellVol = 22293,
      FDegFellArea = structure(
        list(
          area_ha = 1670,
          age_yrs = 0.5
        ),
        class = "data.frame",
        row.names = c(NA,-1L)
      ),
      DeforAreaLow = 159.30517699115,
      DeforAreaUp = 2.01943462897526,
      AReforArea = 11.8882375195155,
      AReforSurveyArea = 616,
      NFDegArea = 214.27905,
      NFDegArea_LCI = 111.283262810747,
      NFDegArea_UCI = 317.274837189253,
      FDegFellAreaSurveyArea = 1670,
      FPlnAreaPlantHwdSurveyArea = 4008,
      FPlnAreaPlantSwdSurveyArea = 2008
    )

MonitoredValues$year[['2019']]$FDegBurnData <- read.table( getDataPath("Burn2019.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2019']]$FDegBurnData))  == 0)) MonitoredValues$year[['2019']]$FDegBurnData <- list()

MonitoredValues$year[['2020']] <- list(
      year = 2020L,
      FPlnVolHarvHwd = 22254,
      FPlnAreaStockHwd = 44813L,
      FPlnAreaPlantHwd = structure(
        list(
          area_ha = c(4008, 0),
          age_yrs = c(1, 0.5)
        ),
        class = "data.frame",
        row.names = c(NA,-2L)
      ),
      FPlnAreaHarvHwd = 143L,
      FPlnAreaJustGrowsHwd = 44670L,
      FPlnVolHarvSwd = 385314,
      FPlnAreaStockSwd = 26094L,
      FPlnAreaPlantSwd = structure(
        list(
          area_ha = c(2008, 1910),
          age_yrs = c(1,0.5)
        ),
        class = "data.frame",
        row.names = c(NA,-2L)
      ),
      FPlnAreaHarvSwd = 1377L,
      FPlnAreaJustGrowsSwd = 24717L,
      FDegFellVol = 20841,
      FDegFellArea = structure(
        list(
          area_ha = c(1670, 1457),
          age_yrs = c(1, 0.5)
        ),
        class = "data.frame",
        row.names = c(NA,-2L)
      ),
      DeforAreaLow = 159.30517699115,
      DeforAreaUp = 2.01943462897526,
      AReforArea = 11.8882375195155,
      AReforSurveyArea = 667,
      NFDegArea = 214.27905,
      NFDegArea_LCI = 111.283262810747,
      NFDegArea_UCI = 317.274837189253,
      FDegFellAreaSurveyArea = 1457,
      FPlnAreaPlantHwdSurveyArea = 0,
      FPlnAreaPlantSwdSurveyArea = 1910
    )
MonitoredValues$year[['2020']]$FDegBurnData <- read.table( getDataPath("Burn2020.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2020']]$FDegBurnData))  == 0)) MonitoredValues$year[['2020']]$FDegBurnData <- list()


load(file = getPeriodDataPath("Fiji_ER_Estimate_AccuracyAssessment", "2019-2020", "RData"))
# AA of AD is done on a monitoring period of 2 years.
# Area of deforestation in natural forest lowland (ha)
MonitoredValues$year[['2019']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
MonitoredValues$year[['2019']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
MonitoredValues$year[['2019']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
MonitoredValues$year[['2019']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
MonitoredValues$year[['2020']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
MonitoredValues$year[['2020']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
MonitoredValues$year[['2020']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
MonitoredValues$year[['2020']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
# Area of deforestation in natural forest upland (ha)
MonitoredValues$year[['2019']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
MonitoredValues$year[['2019']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
MonitoredValues$year[['2019']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
MonitoredValues$year[['2019']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)
MonitoredValues$year[['2020']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
MonitoredValues$year[['2020']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
MonitoredValues$year[['2020']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
MonitoredValues$year[['2020']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)

MonitoredValues$year[['2020']]$AReforArea <- data.frame(
  area_ha = c(
    AdjustedAreas$MCaaaforMean,
    AdjustedAreas$MCaaaforMean
  ),
  age_yrs = c(1.0,0.5),
  lci = c(
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI),
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  ),
  uci = c(
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI),
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
  )
)

MonitoredValues$year[['2019']]$AReforArea <- data.frame(
  area_ha = c(
    AdjustedAreas$MCaaaforMean
  ),
  age_yrs = c(0.5),
  lci = c(
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  ),
  uci = c(
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
  )
)
# Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
MonitoredValues$year[['2019']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
MonitoredValues$year[['2019']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
MonitoredValues$year[['2019']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
MonitoredValues$year[['2020']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
MonitoredValues$year[['2020']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
MonitoredValues$year[['2020']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)


####################################################################################
## End of Monitored Values: 2019 to 2020
####################################################################################





####################################################################################
## Monitored Values: 2021 to 2022
####################################################################################



MonitoredValues$year[['2021']] <- list(
  year = 2021L,
  FPlnVolHarvHwd = 37242,
  FPlnAreaStockHwd = 0, # not used
  FPlnAreaPlantHwd = structure(
    list(
      area_ha = c(4008,0,101),
      age_yrs = c(1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  FPlnAreaHarvHwd = 0, #not used
  FPlnAreaJustGrowsHwd = 0, # not used
  FPlnVolHarvSwd = 603437,
  FPlnAreaStockSwd = 0, # Not used
  FPlnAreaPlantSwd = structure(
    list(
      area_ha = c(2008, 1910, 4237),
      age_yrs = c(1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  FPlnAreaHarvSwd = 0, # not used
  FPlnAreaJustGrowsSwd = 0, # not used
  FDegFellVol = 35959,
  FDegFellArea = structure(
    list(
      area_ha = c(1670, 1457, 4278),
      age_yrs = c(1, 1, 0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  DeforAreaLow = 0, # AA boot
  DeforAreaUp = 0, # AA boot
  AReforArea = 0, # AA boot
  AReforSurveyArea = 0, # AA boot
  NFDegArea = 214.27905, # still to be done
  NFDegArea_LCI = 111.283262810747, # still to be done
  NFDegArea_UCI = 317.274837189253, # still to be done
  FDegFellAreaSurveyArea = 0, # not used
  FPlnAreaPlantHwdSurveyArea = 0, # not used
  FPlnAreaPlantSwdSurveyArea = 0  # not used
)
MonitoredValues$year[['2021']]$FDegBurnData <- read.table( getDataPath("Burn2021.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2021']]$FDegBurnData))  == 0)) MonitoredValues$year[['2021']]$FDegBurnData <- list()



MonitoredValues$year[['2022']] <- list(
  year = 2022L,
  testdebug = "test",
  FPlnVolHarvHwd = 46915,
  FPlnAreaStockHwd = 0, # not used
  FPlnAreaPlantHwd = structure(
    list(
      area_ha = c(4008,0,101, 250),
      age_yrs = c(1,1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  FPlnAreaHarvHwd = 0, # not used
  FPlnAreaJustGrowsHwd = 0, # no used
  FPlnVolHarvSwd = 380905,
  FPlnAreaStockSwd = 0, # not used
  FPlnAreaPlantSwd = structure(
    list(
      area_ha = c(2008, 1910, 4237, 7495),
      age_yrs = c(1,1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  FPlnAreaHarvSwd = 0, # not used
  FPlnAreaJustGrowsSwd = 0, # not used
  FDegFellVol = 34299,
  FDegFellArea = structure(
    list(
      area_ha = c(1670, 1457, 4278, 3169),
      age_yrs = c(1, 1, 1, 0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  DeforAreaLow = 0, # AA boot
  DeforAreaUp = 0, # AA boot
  AReforArea = 0, # AA boot
  AReforSurveyArea = 0, # AA boot
  NFDegArea = 214.27905, # still to be done
  NFDegArea_LCI = 111.283262810747, # still to be done
  NFDegArea_UCI = 317.274837189253, # still to be done
  FDegFellAreaSurveyArea = 0, # not used
  FPlnAreaPlantHwdSurveyArea = 0, # not used
  FPlnAreaPlantSwdSurveyArea = 0  # not used
)
MonitoredValues$year[['2022']]$FDegBurnData <- read.table( getDataPath("Burn2022.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2022']]$FDegBurnData))  == 0)) MonitoredValues$year[['2022']]$FDegBurnData <- list()

load(file = getPeriodDataPath("Fiji_ER_Estimate_AccuracyAssessment", "2021-2022", "RData"))
# AA of AD is done on a monitoring period of 2 years.
# Area of deforestation in natural forest lowland (ha)
MonitoredValues$year[['2021']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
MonitoredValues$year[['2021']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
MonitoredValues$year[['2021']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
MonitoredValues$year[['2021']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
MonitoredValues$year[['2022']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
MonitoredValues$year[['2022']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
MonitoredValues$year[['2022']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
MonitoredValues$year[['2022']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
# Area of deforestation in natural forest upland (ha)
MonitoredValues$year[['2021']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
MonitoredValues$year[['2021']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
MonitoredValues$year[['2021']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
MonitoredValues$year[['2021']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)
MonitoredValues$year[['2022']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
MonitoredValues$year[['2022']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
MonitoredValues$year[['2022']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
MonitoredValues$year[['2022']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)

MonitoredValues$year[['2022']]$AReforArea <- data.frame(
  area_ha = c(
    MonitoredValues$year[['2020']]$AReforArea$area_ha, # 2 vales 2019, 2020
    AdjustedAreas$MCaaaforMean,
    AdjustedAreas$MCaaaforMean
  ),
  age_yrs = c(
    1.0,
    1.0,
    1.0,
    0.5
  ),
  lci = c(
    MonitoredValues$year[['2019']]$McAReforArea_LCI,
    MonitoredValues$year[['2020']]$McAReforArea_LCI,
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI),
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  ),
  uci = c(
    MonitoredValues$year[['2019']]$McAReforArea_UCI,
    MonitoredValues$year[['2020']]$McAReforArea_UCI,
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI),
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
  )
)

MonitoredValues$year[['2021']]$AReforArea <- data.frame(
  area_ha = c(
    MonitoredValues$year[['2020']]$AReforArea$area_ha, # 2 vales 2019, 2020
    AdjustedAreas$MCaaaforMean
  ),
  age_yrs = c(
    1.0,
    1.0,
    0.5
  ),
  lci = c(
    MonitoredValues$year[['2019']]$McAReforArea_LCI,
    MonitoredValues$year[['2020']]$McAReforArea_LCI,
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  ),
  uci = c(
    MonitoredValues$year[['2019']]$McAReforArea_UCI,
    MonitoredValues$year[['2020']]$McAReforArea_UCI,
    quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
  )
)
# Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
MonitoredValues$year[['2021']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
MonitoredValues$year[['2021']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
MonitoredValues$year[['2021']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
MonitoredValues$year[['2022']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
MonitoredValues$year[['2022']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
MonitoredValues$year[['2022']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)


####################################################################################
## End of Monitored Values: 2021 to 2022
####################################################################################





####################################################################################
## Monitored Values: 2023 to 2024
####################################################################################




MonitoredValues$year[['2023']] <- list(
  year = 2023L,
  FPlnVolHarvHwd = 37242,
  FPlnAreaStockHwd = 0, # not used
  FPlnAreaPlantHwd = structure(
    list(
      area_ha = c(4008,0,101),
      age_yrs = c(1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  FPlnAreaHarvHwd = 0, #not used
  FPlnAreaJustGrowsHwd = 0, # not used
  FPlnVolHarvSwd = 603437,
  FPlnAreaStockSwd = 0, # Not used
  FPlnAreaPlantSwd = structure(
    list(
      area_ha = c(2008, 1910, 4237),
      age_yrs = c(1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  FPlnAreaHarvSwd = 0, # not used
  FPlnAreaJustGrowsSwd = 0, # not used
  FDegFellVol = 35959,
  FDegFellArea = structure(
    list(
      area_ha = c(1670, 1457, 4278),
      age_yrs = c(1, 1, 0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  ),
  DeforAreaLow = 0, # AA boot
  DeforAreaUp = 0, # AA boot
  AReforArea = 0, # AA boot
  AReforSurveyArea = 0, # AA boot
  NFDegArea = 214.27905, # still to be done
  NFDegArea_LCI = 111.283262810747, # still to be done
  NFDegArea_UCI = 317.274837189253, # still to be done
  FDegFellAreaSurveyArea = 0, # not used
  FPlnAreaPlantHwdSurveyArea = 0, # not used
  FPlnAreaPlantSwdSurveyArea = 0  # not used
)
MonitoredValues$year[['2023']]$FDegBurnData <- read.table( getDataPath("Burn2023.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2023']]$FDegBurnData))  == 0)) MonitoredValues$year[['2023']]$FDegBurnData <- list()



MonitoredValues$year[['2024']] <- list(
  year = 2024L,
  testdebug = "test",
  FPlnVolHarvHwd = 46915,
  FPlnAreaStockHwd = 0, # not used
  FPlnAreaPlantHwd = structure(
    list(
      area_ha = c(4008,0,101, 250),
      age_yrs = c(1,1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  FPlnAreaHarvHwd = 0, # not used
  FPlnAreaJustGrowsHwd = 0, # no used
  FPlnVolHarvSwd = 380905,
  FPlnAreaStockSwd = 0, # not used
  FPlnAreaPlantSwd = structure(
    list(
      area_ha = c(2008, 1910, 4237, 7495),
      age_yrs = c(1,1,1,0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  FPlnAreaHarvSwd = 0, # not used
  FPlnAreaJustGrowsSwd = 0, # not used
  FDegFellVol = 34299,
  FDegFellArea = structure(
    list(
      area_ha = c(1670, 1457, 4278, 3169),
      age_yrs = c(1, 1, 1, 0.5)
    ),
    class = "data.frame",
    row.names = c(NA,-4L)
  ),
  DeforAreaLow = 0, # AA boot
  DeforAreaUp = 0, # AA boot
  AReforArea = 0, # AA boot
  AReforSurveyArea = 0, # AA boot
  NFDegArea = 214.27905, # still to be done
  NFDegArea_LCI = 111.283262810747, # still to be done
  NFDegArea_UCI = 317.274837189253, # still to be done
  FDegFellAreaSurveyArea = 0, # not used
  FPlnAreaPlantHwdSurveyArea = 0, # not used
  FPlnAreaPlantSwdSurveyArea = 0  # not used
)
MonitoredValues$year[['2024']]$FDegBurnData <- read.table( getDataPath("Burn2024.txt"), sep="\t", header=TRUE)[, c("area_ha", "age_yrs")]
if (all(lengths(na.omit(MonitoredValues$year[['2024']]$FDegBurnData))  == 0)) MonitoredValues$year[['2024']]$FDegBurnData <- list()

FileLoaded <- FALSE
tryCatch(
  {
    load(file = getPeriodDataPath("Fiji_ER_Estimate_AccuracyAssessment", "2023-2024", "RData"))
    FileLoaded <<- TRUE
  },
  error = function(e) {},
  warning = function(w) {},
  finally = function() {})


if (FileLoaded) {
  print("File loaded")
  # AA of AD is done on a monitoring period of 2 years.
  # Area of deforestation in natural forest lowland (ha)
  MonitoredValues$year[['2023']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
  MonitoredValues$year[['2023']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
  MonitoredValues$year[['2023']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
  MonitoredValues$year[['2023']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
  MonitoredValues$year[['2024']]$DeforAreaLow <- AdjustedAreas$areaLoss[1]
  MonitoredValues$year[['2024']]$McDeforAreaLow <- AdjustedAreas$MCaadeforL
  MonitoredValues$year[['2024']]$McDeforAreaLow_LCI <- quantile(AdjustedAreas$MCaadeforL,probs=QLCI)
  MonitoredValues$year[['2024']]$McDeforAreaLow_UCI <- quantile(AdjustedAreas$MCaadeforL,probs=QUCI)
  # Area of deforestation in natural forest upland (ha)
  MonitoredValues$year[['2023']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
  MonitoredValues$year[['2023']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
  MonitoredValues$year[['2023']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
  MonitoredValues$year[['2023']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)
  MonitoredValues$year[['2024']]$DeforAreaUp <- AdjustedAreas$areaLoss[2]
  MonitoredValues$year[['2024']]$McDeforAreaUp <- AdjustedAreas$MCaadeforU
  MonitoredValues$year[['2024']]$McDeforAreaUp_LCI <- quantile(AdjustedAreas$MCaadeforU,probs=QLCI)
  MonitoredValues$year[['2024']]$McDeforAreaUp_UCI <- quantile(AdjustedAreas$MCaadeforU,probs=QUCI)
  
  MonitoredValues$year[['2024']]$AReforArea <- data.frame(
    area_ha = c(
      MonitoredValues$year[['2022']]$AReforArea$area_ha, # 4 vales 2019, 2020, 2021, 2022
      AdjustedAreas$MCaaaforMean,
      AdjustedAreas$MCaaaforMean
    ),
    age_yrs = c(
      1.0,
      1.0,
      1.0,
      1.0,
      1.0,
      0.5
    ),
    lci = c(
      MonitoredValues$year[['2019']]$McAReforArea_LCI,
      MonitoredValues$year[['2020']]$McAReforArea_LCI,
      MonitoredValues$year[['2021']]$McAReforArea_LCI,
      MonitoredValues$year[['2022']]$McAReforArea_LCI,
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI),
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
    ),
    uci = c(
      MonitoredValues$year[['2019']]$McAReforArea_UCI,
      MonitoredValues$year[['2020']]$McAReforArea_UCI,
      MonitoredValues$year[['2021']]$McAReforArea_UCI,
      MonitoredValues$year[['2022']]$McAReforArea_UCI,
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI),
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
    )
  )
  
  MonitoredValues$year[['2023']]$AReforArea <- data.frame(
    area_ha = c(
      MonitoredValues$year[['2022']]$AReforArea$area_ha, # 4 vales 2019, 2020, 2021, 2022
      AdjustedAreas$MCaaaforMean
    ),
    age_yrs = c(
      1.0,
      1.0,
      1.0,
      1.0,
      0.5
    ),
    lci = c(
      MonitoredValues$year[['2019']]$McAReforArea_LCI,
      MonitoredValues$year[['2020']]$McAReforArea_LCI,
      MonitoredValues$year[['2021']]$McAReforArea_LCI,
      MonitoredValues$year[['2022']]$McAReforArea_LCI,
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
    ),
    uci = c(
      MonitoredValues$year[['2019']]$McAReforArea_UCI,
      MonitoredValues$year[['2020']]$McAReforArea_UCI,
      MonitoredValues$year[['2021']]$McAReforArea_UCI,
      MonitoredValues$year[['2022']]$McAReforArea_UCI,
      quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
    )
  )
  # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
  # AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  # AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  MonitoredValues$year[['2023']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
  MonitoredValues$year[['2023']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  MonitoredValues$year[['2023']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
  MonitoredValues$year[['2024']]$McAReforArea <- rowSums(AdjustedAreas$MCaaafor)
  MonitoredValues$year[['2024']]$McAReforArea_LCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QLCI)
  MonitoredValues$year[['2024']]$McAReforArea_UCI <- quantile(rowSums(AdjustedAreas$MCaaafor),probs=QUCI)
}

####################################################################################
## End of Monitored Values: 2021 to 2022
####################################################################################





####################################################################################
## Set the values to be used for this run
####################################################################################


MonitoredValues$year1 <- MonitoredValues$year[[as.character(MonitoringReportParams$period$start)]]
MonitoredValues$year2 <- MonitoredValues$year[[as.character(MonitoringReportParams$period$end)]]
print(paste0("Period:", 
               as.character(MonitoringReportParams$period$end), "-", as.character(MonitoringReportParams$period$end)
             ))

####################################################################################
## Save the data
####################################################################################

fullFilename <- paste("Fiji_ER_Estimate_Params", "RData", sep = ".")
save(
  list = 
  c("ErpaYearlyFRL",
    "MonitoredValues",
    "ReportSettings",
    "MonitoringReportParams"),
  file = paste(getDataPath(fullFilename)))