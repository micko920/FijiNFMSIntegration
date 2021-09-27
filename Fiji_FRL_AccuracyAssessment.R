


# Load all necessary data
load(file = "./Data/preMonitoringReport/fiji_frl_input.RData")
# aa_sample <- read.csv(file = "./Data/frlCorrection/aa_sample.csv")
# lcc_mapped_areas <- read.csv(file = "./Data/frlCorrection/lcc_mapped_areas.csv")


# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

options("width" = 120)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))
options(max.print = 50)

# This number was used to generate the chk file.
MCRuns <- 10000
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on


source(file = "./Data/preMonitoringReport/FRL_Parameters.R")

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

print("Running Accuracy Assessment and generating adjusted areas....")
timestamp <- Sys.time()
print(date())

## Accuracy Assessment using bootstrap
AdjustedAreas <- calcFRLAdjustedAreas()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit = "auto"))

# The final table ##################################################################
if (debug_frl | show_output) {
  print(AdjustedAreas)
}

save(
  list = c(
    "AdjustedAreas"
  ),
  file = "./Data/preMonitoringReport/fiji_frl_adjusted_areas.RData"
  # file = "./Data/frlCorrection/fiji_frl_adjusted_areas.RData"
)
