# Required source files
load(file = "./Data/fiji_frl_input.RData")
# Note all CalcFunctions return CO2e values

# new line

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)

# Set up
options(digits = 6)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 1.5e+03
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

#### Values used to calculate 2019-2020 output
# MCRuns <- 1.5e+06
# MCTolerance <- 0.0025
# set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI


debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples


# Yearly Data (to be input for each year)
# .....................................................................................
# Used input data from baseline FRL, actual data to be input for each year
source(file = "./Baseline_Values/Monitored_Values.R")
#source(file = "./Baseline_Values/Monitored_Values_2019_2020.R")


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


print("Calculating EmRem values....")
print(date())

EmRems_Values <- list()
EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)

print("Calculating ER values....")
print(date())

ER_Values <- CalcERValues(
  EmRems_Values,
  MonitoringReportParams$ErpaYearlyFRL,
  MonitoringReportParams$ErpaYearlyFRLFDeg
)

UC_Values <- list()
UC_Values <- createUC_Values()


UC_MV_Values <- list()
UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)


UC_EmRems_Values <- list()

print("Calculating UC year 1 values....")
print(date())

UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)


print("Calculating UC year 2 values....")
print(date())

UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)

print("Calculating UC ER values....")
print(date())

UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)

if (debug_er) {
  print(UC_ER_Values)
}


MR_Values <- create_MRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)


Table4_2 <- createTable_4_2(MR_Values)

Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)

Table5_2_2 <- createTable_5_2_2(MR_Values)

Table7_2 <- createTable_7_2(MR_Values, MonitoringReportParams)

Table8 <- createTable_8(MR_Values, MonitoringReportParams)

if (debug_er) {
  print(Table4_2)
  print(Table4_3)
  print(Table5_2_2)
  print(Table7_2)
  print(Table8)
}

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


if (debug_er) {
  print(ResultsTables)
}
save(
  list = c(
    "ResultsTables",
    "EmRems_Values",
    "ER_Values",
    "MR_Values",
    "UC_Values",
    "UC_MV_Values",
    "UC_EmRems_Values",
    "Table4_2",
    "Table4_3",
    "Table5_2_2",
    "Table7_2",
    "Table8"
  ),
  file = "./Data/Fiji_ER_EstimateResults_UC.Rdata"
)
if (debug_er | show_output) {
  old_width <- options("width" = 120)
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_ER_EstimateResults_UC.txt")
  print(ResultsTables)
  print(MR_Values)
  sink()
  options(old_width)
}


