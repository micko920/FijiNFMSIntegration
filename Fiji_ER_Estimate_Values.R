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

# Set up
options(digits = 6)
options(show.error.locations = TRUE)

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on

# Yearly Data (to be input for each year)
# .....................................................................................
# Used input data from baseline FRL, actual data to be input for each year
source(file = "./Baseline_Values/Monitored_Values.R")

# Calculated in the FRL #####################################################################
# All values are now in the FijiNFMSCalculations package.

# ER monitoring Report Parameters #################################################################

# External NFMS Inputs
source(file = "./Baseline_Values/ER_Monitoring_Report_Parameters.R")

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

# Load all necessary data
load(file = "./Data/Fiji_ER_EstimateResults_AdjustedAreas.RData")

# AA of AD is done on a monitoring period of 2 years.
# Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
MonitoredValues$year1$DeforAreaLow <- AdjustedAreas$areaLoss[1] / 2
MonitoredValues$year1$McDeforAreaLow <- AdjustedAreas$MCaadeforL / 2
MonitoredValues$year2$DeforAreaLow <- AdjustedAreas$areaLoss[1] / 2
MonitoredValues$year2$McDeforAreaLow <- AdjustedAreas$MCaadeforL / 2
# Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
MonitoredValues$year1$DeforAreaUp <- AdjustedAreas$areaLoss[2] / 2
MonitoredValues$year1$McDeforAreaUp <- AdjustedAreas$MCaadeforU / 2
MonitoredValues$year2$DeforAreaUp <- AdjustedAreas$areaLoss[2] / 2
MonitoredValues$year2$McDeforAreaUp <- AdjustedAreas$MCaadeforU / 2
# Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
# AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
# AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
MonitoredValues$year1$AReforArea <- AdjustedAreas$MCaaaforMean / 2
MonitoredValues$year1$McAReforArea <- rowSums(AdjustedAreas$MCaaafor) /2
MonitoredValues$year2$AReforArea <- AdjustedAreas$MCaaaforMean / 2
MonitoredValues$year2$McAReforArea <- rowSums(AdjustedAreas$MCaaafor) /2

EmRems_Values <- list()
EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)

ER_Values <- CalcERValues(
  EmRems_Values,
  MonitoringReportParams$ErpaYearlyFRL,
  MonitoringReportParams$ErpaYearlyFRLFDeg
)

MR_Values <- create_EstMRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)

Table4_2 <- createTable_4_2(MR_Values)

Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)


if (debug_er) {
  print(Table4_2)
  print(Table4_3)
  print(MonitoredValues)
  print(EmRems_Values)
  print(ER_Values)
}


if (debug_er | show_output) {
  old_width <- options("width" = 120)
  print(MR_Values)
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_ER_EstimateResults_Values.txt")
  #print(MonitoredValues)

  print("***** EmRems_Values *******")
  print(EmRems_Values)
  print("****** ER_Values *******")
  print(ER_Values)
  print("***** MR_Values ********")
  print(MR_Values)
  sink()
  options(old_width)
}

