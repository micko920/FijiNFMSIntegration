# R code to calculate annual emissions(yr-1) for comparison with base FRL

# setwd("C:\eas-2018-prj\FijiGov\NFMSIntegrationFramework\code\calcs")

# Required source files
load(file = "./Data/fiji_frl_input.RData")
source(file = "./Utils/UncertaintyFunc.R")
# source(file = "./Utils/aaboot.R")
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
options(digits = 6) # 6 significant figures
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

MCRuns <- 1.5e+06 # limit the number of runs in MC simulation - change as required
MCTolerance <- 0.01 # how stable the UCI and LCI should be before stopping

# This number was used to generate the chk file.
# MCRuns <- 1.5e+06
# MCTolerance <- 0.0025
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples


# Yearly Data (to be input for each year)
# .....................................................................................
# Used input data from baseline FRL, actual data to be input for each year
source(file = "./Baseline_Values/Monitored_Values.R")

# results of accuracy assessment for uncertainty analysis
# aa_boot <- read.table("./Data/aa_boot.txt", header = T) # or use new AccuracyAssessment.R file
# results of FRL Deforestation Emissions Factors for uncertainty analysis
# v_dc <- read.table("./Data/v_dc.txt", header = T)

# Calculated in the FRL #####################################################################
# All values are now in the FijiNFMSCalculations package.


# ER monitoring Report Parameters #################################################################

# External NFMS Inputs

source(file = "./Baseline_Values/ER_Monitoring_Report_Parameters.R")

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

CalcMonteCarlo <- function(title, # Name to use on debug and plots
                           est, # CO2e estimate
                           calc, # Function to calculate CO2e from inputs
                           calcArgs, # function to create #MC runs of CO2e possibilities using uncertainties
                           iterations = MCRuns,
                           Confidence = CI, #  confidence interval
                           previous = vector()) {
  result <- StableMonteCarloSamples(calc, calcArgs, est, tolerance = MCTolerance, limit = iterations, debug = plot_mc_output)
  if (plot_mc_output) PlotMonteCarloSamples(result, est, title)
  return(list(value = EstimateWithBounds(est, result), MCresults = result))
}

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

##### Value Models #####

vwuNorm <- function(v, n, ...) {
  sd <- (diff(range(v)) / (2 * qnorm(p = QUCI))) # LCI,UCI are 90% QCI (5%,95%)
  return(rnorm(n, mean = ValueWithUncertaintyValue(v), sd = sd))
}

vwuTriangle <- function(v, n, ...) {
  return(rtriangle(n = n, theta = ValueWithUncertaintyValue(v), lower = min(v), upper = max(v)))
}

create_vwuSampled <- function(value_samples) {
  return(function(v, n, ...) {
    return(sample(value_samples, size = n, replace = TRUE))
  })
}

source(file = "./Funcs/UC_Values.R")

UC_Values <- list()
UC_Values <- createUC_Values()

source(file = "./Funcs/UC_MV_Values.R")

UC_MV_Values <- list()
UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)

source(file = "./Funcs/UC_ER_Values.R")

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

source(file = "./Funcs/MR_Values.R")

MR_Values <- create_MRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)

source(file = "./Funcs/TableCreationFunctions.R")

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
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[1]],
    UC_EmRems_Values$year1$EstEmFellFinal$value[[1]],
    UC_EmRems_Values$year1$EmFireFinal$value[[1]],
    UC_EmRems_Values$year1$EmEstFPTotalFinal$value[[1]],
    UC_EmRems_Values$year1$GrossEmTotalFinal$value[[1]],
    UC_EmRems_Values$year1$EstRemFellFinal$value[[1]],
    UC_EmRems_Values$year1$RemEstARFinal$value[[1]],
    UC_EmRems_Values$year1$RemEstFPTotalFinal$value[[1]],
    UC_EmRems_Values$year1$GrossRemTotalFinal$value[[1]],
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[1]],
    UC_EmRems_Values$year1$FDFinal$value[[1]],
    UC_EmRems_Values$year1$ECFinal$value[[1]],
    UC_EmRems_Values$year1$NetEmissionsFinal$value[[1]]
  ),
  LCI = c(
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[2]],
    UC_EmRems_Values$year1$EstEmFellFinal$value[[2]],
    UC_EmRems_Values$year1$EmFireFinal$value[[2]],
    UC_EmRems_Values$year1$EmEstFPTotalFinal$value[[2]],
    UC_EmRems_Values$year1$GrossEmTotalFinal$value[[2]],
    UC_EmRems_Values$year1$EstRemFellFinal$value[[2]],
    UC_EmRems_Values$year1$RemEstARFinal$value[[2]],
    UC_EmRems_Values$year1$RemEstFPTotalFinal$value[[2]],
    UC_EmRems_Values$year1$GrossRemTotalFinal$value[[2]],
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[2]],
    UC_EmRems_Values$year1$FDFinal$value[[2]],
    UC_EmRems_Values$year1$ECFinal$value[[2]],
    UC_EmRems_Values$year1$NetEmissionsFinal$value[[2]]
  ),
  UCI = c(
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[3]],
    UC_EmRems_Values$year1$EstEmFellFinal$value[[3]],
    UC_EmRems_Values$year1$EmFireFinal$value[[3]],
    UC_EmRems_Values$year1$EmEstFPTotalFinal$value[[3]],
    UC_EmRems_Values$year1$GrossEmTotalFinal$value[[3]],
    UC_EmRems_Values$year1$EstRemFellFinal$value[[3]],
    UC_EmRems_Values$year1$RemEstARFinal$value[[3]],
    UC_EmRems_Values$year1$RemEstFPTotalFinal$value[[3]],
    UC_EmRems_Values$year1$GrossRemTotalFinal$value[[3]],
    UC_EmRems_Values$year1$McEstEmRemsDefor$value[[3]],
    UC_EmRems_Values$year1$FDFinal$value[[3]],
    UC_EmRems_Values$year1$ECFinal$value[[3]],
    UC_EmRems_Values$year1$NetEmissionsFinal$value[[3]]
  )
)

ResultsTables$year2 <- data.frame(
  stratum = c(
    "Deforestation", "Forest Deg (felling)", "Forest Deg (fire)",
    "Forest Plantations", "Sum Emissions", "Forest Deg (felling)", "Afforestation",
    "Forest Plantations", "Sum Removals", "Deforestation", "Forest Degradation", "Enhancement", "Total"
  ),
  Estimate = c(
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[1]],
    UC_EmRems_Values$year2$EstEmFellFinal$value[[1]],
    UC_EmRems_Values$year2$EmFireFinal$value[[1]],
    UC_EmRems_Values$year2$EmEstFPTotalFinal$value[[1]],
    UC_EmRems_Values$year2$GrossEmTotalFinal$value[[1]],
    UC_EmRems_Values$year2$EstRemFellFinal$value[[1]],
    UC_EmRems_Values$year2$RemEstARFinal$value[[1]],
    UC_EmRems_Values$year2$RemEstFPTotalFinal$value[[1]],
    UC_EmRems_Values$year2$GrossRemTotalFinal$value[[1]],
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[1]],
    UC_EmRems_Values$year2$FDFinal$value[[1]],
    UC_EmRems_Values$year2$ECFinal$value[[1]],
    UC_EmRems_Values$year2$NetEmissionsFinal$value[[1]]
  ),
  LCI = c(
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[2]],
    UC_EmRems_Values$year2$EstEmFellFinal$value[[2]],
    UC_EmRems_Values$year2$EmFireFinal$value[[2]],
    UC_EmRems_Values$year2$EmEstFPTotalFinal$value[[2]],
    UC_EmRems_Values$year2$GrossEmTotalFinal$value[[2]],
    UC_EmRems_Values$year2$EstRemFellFinal$value[[2]],
    UC_EmRems_Values$year2$RemEstARFinal$value[[2]],
    UC_EmRems_Values$year2$RemEstFPTotalFinal$value[[2]],
    UC_EmRems_Values$year2$GrossRemTotalFinal$value[[2]],
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[2]],
    UC_EmRems_Values$year2$FDFinal$value[[2]],
    UC_EmRems_Values$year2$ECFinal$value[[2]],
    UC_EmRems_Values$year2$NetEmissionsFinal$value[[2]]
  ),
  UCI = c(
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[3]],
    UC_EmRems_Values$year2$EstEmFellFinal$value[[3]],
    UC_EmRems_Values$year2$EmFireFinal$value[[3]],
    UC_EmRems_Values$year2$EmEstFPTotalFinal$value[[3]],
    UC_EmRems_Values$year2$GrossEmTotalFinal$value[[3]],
    UC_EmRems_Values$year2$EstRemFellFinal$value[[3]],
    UC_EmRems_Values$year2$RemEstARFinal$value[[3]],
    UC_EmRems_Values$year2$RemEstFPTotalFinal$value[[3]],
    UC_EmRems_Values$year2$GrossRemTotalFinal$value[[3]],
    UC_EmRems_Values$year2$McEstEmRemsDefor$value[[3]],
    UC_EmRems_Values$year2$FDFinal$value[[3]],
    UC_EmRems_Values$year2$ECFinal$value[[3]],
    UC_EmRems_Values$year2$NetEmissionsFinal$value[[3]]
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
  file = "Fiji_ER_EstimateResults_UC.Rdata"
)
if (debug_er | show_output) {
  old_width <- options("width" = 120)
  #**************************************************************************
  # put results in txt file
  sink("Fiji_ER_EstimateResults_UC.txt")
  print(ResultsTables)
  print(MR_Values)
  sink()
  options(old_width)
}
