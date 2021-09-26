
# Load all necessary data
load(file = "./Data/preMonitoringReport/fiji_frl_input.RData")

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))
options(max.print=50)

# This number was used to generate the chk file.
MCRuns <- 1.5e+06
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- FALSE #Turn printed output on
show_output <- TRUE #Turn final table printed output on


source(file = "./Data/preMonitoringReport/FRL_Parameters.R")

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

print("Running NFI calculations and generating emission factors....")
timestamp <- Sys.time()
print(date())

## Emissions Factors calculated from NFI
EmissionFactors <- calcEmissionFactors()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

# The final table ##################################################################
if (debug_frl | show_output) {
  print(EmissionFactors)
}

save(
  list = c(
    "EmissionFactors"
  ),
  file = "./Data/preMonitoringReport/fiji_frl_emission_factors.RData"
)

