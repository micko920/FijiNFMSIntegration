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

EmRems_Values <- list()
EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)

ER_Values <- CalcERValues(
  EmRems_Values,
  MonitoringReportParams$ErpaYearlyFRL,
  MonitoringReportParams$ErpaYearlyFRLFDeg
)

source(file = "./Funcs/MR_Values.R")

MR_Values <- create_EstMRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)

source(file = "./Funcs/TableCreationFunctions.R")

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
  sink("Fiji_ER_EstimateResults_Values.txt")
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

