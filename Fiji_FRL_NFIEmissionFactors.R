

getDataPath<-function(filename) {
  return(paste0("./Data/frlUpdateOct23/", filename))
}


# Load all necessary data
load(file = getDataPath("fiji_frl_input.RData"))

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
options("width" = 320)

MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- TRUE # Turn printed output on
debug_er <- TRUE # Turn printed output on
show_output <- TRUE #Turn final table printed output on


source(file = getDataPath("FRL_Parameters.R"))

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

outputFilename <- "Fiji_FRL_NFIEmmissionFactors"
pdf(paste0(outputFilename, ".pdf"))

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
  file = getDataPath("fiji_frl_emission_factors.RData")
)

