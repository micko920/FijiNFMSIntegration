
# Load all necessary data
load(file = "./Data/fiji_frl_input.RData")

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

# Set up
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 1.5e+06
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- FALSE #Turn printed output on
show_output <- TRUE #Turn final table printed output on


source(file = "./Baseline_Values/FRL_Parameters.R")

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

print("Running Accuracy Assessment and generating adjusted areas....")
timestamp <- Sys.time()
print(date())

## Accuracy Assessment using bootstrap
AdjustedAreas <- calcAdjustedAreas()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

# The final table ##################################################################
if (debug_frl | show_output) {
  print(AdjustedAreas)
}

save(
  list = c(
    "AdjustedAreas"
  ),
  file = "./Data/fiji_frl_adjusted_areas.RData"
)
