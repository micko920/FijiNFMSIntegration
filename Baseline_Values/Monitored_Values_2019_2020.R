# Monitored Values


MonitoredValues <- list()
MonitoredValues$year1 <- list()
MonitoredValues$year2 <- list()

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$year <- "2018"
MonitoredValues$year1$DFAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
MonitoredValues$year1$DFAreaLow_UCI <- 9437
MonitoredValues$year1$DFAreaLow_LCI <- 5531

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$DFAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
MonitoredValues$year1$DFAreaUp_UCI <- 2889
MonitoredValues$year1$DFAreaUp_LCI <- 1627

MonitoredValues$year1$ARArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# ARAreaLow      #ARArea = Sum of ARAreaLow and ARAreaUp
# ARAreaUp       #ARArea = Sum of ARAreaLow and ARAreaUp
# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$ARArea_UCI <- 8124
MonitoredValues$year1$ARArea_LCI <- 4415

# Biomass Burned - Area and Average age of forest burned (used FRL data here)
MonitoredValues$year1$FDBurnData <- read.table("./Baseline_Values/FRLBurnData.txt", header = T)[, c("year", "area_ha", "age_yrs")]

# Without Uncertainty
MonitoredValues$year1$FPVolHarvHW <- 19801 # volume of hardwood harvested (m3) REAL DATA
MonitoredValues$year1$FPAreaStockHW <- 56950.5 # Existing stock at start of yr (hardwood)
MonitoredValues$year1$FPAreaPlantHW <- 3050.30 # Area planted during the yr (hardwood)
MonitoredValues$year1$FPAreaHarvestHW <- 135 # Area harvested during the yr (hardwood) REAL DATA
MonitoredValues$year1$FPAreaJustGrowsHW <- MonitoredValues$year1$FPAreaStockHW - MonitoredValues$year1$FPAreaHarvestHW
MonitoredValues$year1$FPVolHarvSW <- 386985 # volume of softwood harvested (m3) REAL DATA
MonitoredValues$year1$FPAreaStockSW <- 24698 # Existing stock at start of yr (softwood) REAL DATA
MonitoredValues$year1$FPAreaPlantSW <- 2008 # Area planted during the yr (softwood) REAL DATA
MonitoredValues$year1$FPAreaHarvestSW <- 909 # Area harvested during the yr (softwood) REAL DATA
MonitoredValues$year1$FPAreaJustGrowsSW <- MonitoredValues$year1$FPAreaStockSW - MonitoredValues$year1$FPAreaHarvestSW
MonitoredValues$year1$FDFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
MonitoredValues$year1$FDFellArea <- 11669.9 # Area of natural forest felled (ha)

# TODO make up new values for 2019
MonitoredValues$year2$year <- "2019"
MonitoredValues$year2$DFAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
MonitoredValues$year2$DFAreaLow_UCI <- 9437
MonitoredValues$year2$DFAreaLow_LCI <- 5531

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year2$DFAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
MonitoredValues$year2$DFAreaUp_UCI <- 2889
MonitoredValues$year2$DFAreaUp_LCI <- 1627

MonitoredValues$year2$ARArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# ARAreaLow      #ARArea = Sum of ARAreaLow and ARAreaUp
# ARAreaUp       #ARArea = Sum of ARAreaLow and ARAreaUp
# data taken from output of FRL Accuracy Assessment
MonitoredValues$year2$ARArea_UCI <- 8124
MonitoredValues$year2$ARArea_LCI <- 4415

# Biomass Burned - Area and Average age of forest burned (used FRL data here)
MonitoredValues$year2$FDBurnData <- read.table("./Baseline_Values/FRLBurnData.txt", header = T)[, c("year", "area_ha", "age_yrs")]

# Without Uncertainty
MonitoredValues$year2$FPVolHarvHW <-  21441 # volume of hardwood harvested (m3) REAL DATA 
MonitoredValues$year2$FPAreaStockHW <- 56950.5 # Existing stock at start of yr (hardwood)
MonitoredValues$year2$FPAreaPlantHW <- 3050.30 # Area planted during the yr (hardwood)
MonitoredValues$year2$FPAreaHarvestHW <- 143 # Area harvested during the yr (hardwood) REAL DATA
MonitoredValues$year2$FPAreaJustGrowsHW <- MonitoredValues$year1$FPAreaStockHW - MonitoredValues$year1$FPAreaHarvestHW
MonitoredValues$year2$FPVolHarvSW <- 479959 # volume of softwood harvested (m3) REAL DATA
MonitoredValues$year2$FPAreaStockSW <- 26094 # Existing stock at start of yr (softwood) REAL DATA
MonitoredValues$year2$FPAreaPlantSW <- 1910 # Area planted during the yr (softwood) REAL DATA
MonitoredValues$year2$FPAreaHarvestSW <- 1377 # Area harvested during the yr (softwood) REAL DATA
MonitoredValues$year2$FPAreaJustGrowsSW <- MonitoredValues$year1$FPAreaStockSW - MonitoredValues$year1$FPAreaHarvestSW
MonitoredValues$year2$FDFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
MonitoredValues$year2$FDFellArea <- 11669.9 # Area of natural forest felled (ha)
