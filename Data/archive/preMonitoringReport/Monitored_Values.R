# Monitored Values


MonitoredValues <- list()
MonitoredValues$year1 <- list()
MonitoredValues$year2 <- list()

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$year <- "2018"
MonitoredValues$year1$DeforAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
MonitoredValues$year1$DeforAreaLow_UCI <- 9437
MonitoredValues$year1$DeforAreaLow_LCI <- 5531

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$DeforAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
MonitoredValues$year1$DeforAreaUp_UCI <- 2889
MonitoredValues$year1$DeforAreaUp_LCI <- 1627

MonitoredValues$year1$AReforArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# data taken from output of FRL Accuracy Assessment
MonitoredValues$year1$AReforArea_UCI <- 8124
MonitoredValues$year1$AReforArea_LCI <- 4415

# Biomass Burned - Area and Average age of forest burned (used FRL data here)
MonitoredValues$year1$FDegBurnData <- read.table("./Data/preMonitoringReport/FRLBurnData.txt", header = T)[, c("year", "area_ha", "age_yrs")]

# Without Uncertainty
MonitoredValues$year1$FPlnVolHarvHwd <- 62199.6 # volume of hardwood harvested (m3)
MonitoredValues$year1$FPlnAreaStockHwd <- 56950.5 # Existing stock at start of yr (hardwood)
MonitoredValues$year1$FPlnAreaPlantHwd <- 3050.30 # Area planted during the yr (hardwood)
MonitoredValues$year1$FPlnAreaHarvHwd  <- 3316.60 # Area harvested during the yr (hardwood)
MonitoredValues$year1$FPlnAreaJustGrowsHwd <- MonitoredValues$year1$FPlnAreaStockHwd - MonitoredValues$year1$FPlnAreaHarvHwd 
MonitoredValues$year1$FPlnVolHarvSwd <- 334463 # volume of softwood harvested (m3)
MonitoredValues$year1$FPlnAreaStockSwd <- 49106.0 # Existing stock at start of yr (softwood)
MonitoredValues$year1$FPlnAreaPlantSwd <- 370.820 # Area planted during the yr (softwood)
MonitoredValues$year1$FPlnAreaHarvSwd <- 1282.40 # Area harvested during the yr (softwood)
MonitoredValues$year1$FPlnAreaJustGrowsSwd <- MonitoredValues$year1$FPlnAreaStockSwd - MonitoredValues$year1$FPlnAreaHarvSwd
MonitoredValues$year1$FDegFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
MonitoredValues$year1$FDegFellArea <- 11669.9 # Area of natural forest felled (ha)

# TODO make up new values for 2019
MonitoredValues$year2$year <- "2019"
MonitoredValues$year2$DeforAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
MonitoredValues$year2$DeforAreaLow_UCI <- 9437
MonitoredValues$year2$DeforAreaLow_LCI <- 5531

# data taken from output of FRL Accuracy Assessment
MonitoredValues$year2$DeforAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
MonitoredValues$year2$DeforAreaUp_UCI <- 2889
MonitoredValues$year2$DeforAreaUp_LCI <- 1627

MonitoredValues$year2$AReforArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# data taken from output of FRL Accuracy Assessment
MonitoredValues$year2$AReforArea_UCI <- 8124
MonitoredValues$year2$AReforArea_LCI <- 4415

# Biomass Burned - Area and Average age of forest burned (used FRL data here)
MonitoredValues$year2$FDegBurnData <- read.table("./Data/preMonitoringReport/FRLBurnData.txt", header = T)[, c("year", "area_ha", "age_yrs")]

# Without Uncertainty
MonitoredValues$year2$FPlnVolHarvHwd <- 62199.6 # volume of hardwood harvested (m3)
MonitoredValues$year2$FPlnAreaStockHwd <- 56950.5 # Existing stock at start of yr (hardwood)
MonitoredValues$year2$FPlnAreaPlantHwd <- 3050.30 # Area planted during the yr (hardwood)
MonitoredValues$year2$FPlnAreaHarvHwd  <- 3316.60 # Area harvested during the yr (hardwood)
MonitoredValues$year2$FPlnAreaJustGrowsHwd <- MonitoredValues$year1$FPlnAreaStockHwd - MonitoredValues$year1$FPlnAreaHarvHwd 
MonitoredValues$year2$FPlnVolHarvSwd <- 334463 # volume of softwood harvested (m3)
MonitoredValues$year2$FPlnAreaStockSwd <- 49106.0 # Existing stock at start of yr (softwood)
MonitoredValues$year2$FPlnAreaPlantSwd <- 370.820 # Area planted during the yr (softwood)
MonitoredValues$year2$FPlnAreaHarvSwd <- 1282.40 # Area harvested during the yr (softwood)
MonitoredValues$year2$FPlnAreaJustGrowsSwd <- MonitoredValues$year1$FPlnAreaStockSwd - MonitoredValues$year1$FPlnAreaHarvSwd
MonitoredValues$year2$FDegFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
MonitoredValues$year2$FDegFellArea <- 11669.9 # Area of natural forest felled (ha)