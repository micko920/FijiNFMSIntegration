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
options(digits = 8)
options(show.error.locations = TRUE)

#MCRuns <- 1.5e+06 # limit the number of runs in MC simulation - change as required
#MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

# This number was used to generate the chk file.
MCRuns <- 1.5e+06 #  number of runs in MC simulation - change as required
MCTolerance <- 0.0025
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples


# Yearly Data (to be input for each year)
# .....................................................................................
# Used input data from baseline FRL, actual data to be input for each year
source(file = "./Baseline_Values/Monitored_Values_2019_2020.R")

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


formatDecimal <- function(x) {
  return(format(round(x, 4), nsmall = 4))
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
  sd <- (diff(range(v)) / (2 * qnorm(p = QUCI))) # LCI,UCI are 5% and 95%
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
print("Running Sensitivity Analysis [Parameters]...")
print(date())
print(paste("Checking ", length(UC_Values), " parameters"))

for (i in 1:length(UC_Values)) {
  UC_Values[[i]] <- ValueWithUncertaintyFixed(UC_Values[[i]])

  UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
  UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)

  UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)

  UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)

  UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)

  TEI_Values$params$name[i] <- names(UC_Values[i])
  TEI_Values$params$v[i] <- formatDecimal(calcTEI(UC_ER_Values$McMpEstERsDefEnh$MCresults, V_all))
  print(date())
  print(paste(TEI_Values$params$name[i], ": ", TEI_Values$params$v[i]))
  print(paste(i, "of ", length(UC_Values)))
  UC_Values[[i]] <- ValueWithUncertaintySampled(UC_Values[[i]])
}

TEI_Values$activityData <- data.frame(
  name = character(length(UC_MV_Values$year1)),
  v = numeric(length(UC_MV_Values$year1)),
  stringsAsFactors = FALSE
)
print("Running Sensitivity Analysis [Activity Data]...")
print(date())
print(paste("Checking ", length(UC_MV_Values$year1), " activitiy data values"))

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
  print(date())
  print(paste(TEI_Values$activityData$name[i], ": ", TEI_Values$activityData$v[i]))
  print(paste(i, "of ", length(UC_MV_Values$year1)))
  UC_MV_Values$year1[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year1[[i]])
  UC_MV_Values$year2[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year2[[i]])
}


if (debug_er | show_output) {
  old_width <- options("width" = 120)
  print(TEI_Values)
  #**************************************************************************
  # put results in txt file
  sink("Fiji_ER_EstimateResults_Sensitivity.txt")
  print(TEI_Values)
  sink()
  options(old_width)
}
