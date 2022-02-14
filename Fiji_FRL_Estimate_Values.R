

getDataPath<-function(filename) {
  return(paste0("./Data/frlUpdate14Feb22/", filename))
}

  
# Load all necessary data
load(file = getDataPath("fiji_frl_input.RData"))

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)

# Set up
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))
options(max.print=50)

# This number was used to generate the chk file.
#MCRuns <- 1.5e+06
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- TRUE # Turn printed output on
debug_er <- TRUE # Turn printed output on
show_output <- TRUE # Turn final table printed output on


source(file = getDataPath("FRL_Parameters.R"))

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################


print("Running FRL Estimate Calculations...")
timestamp <- Sys.time()
print(date())

# Load all necessary data
load(file = getDataPath("fiji_frl_adjusted_areas.RData"))
load(file = getDataPath("fiji_frl_emission_factors.RData"))


## Deforestation
FRLDeforestation <- calcFRLDeforestation()

## Felling in Natural Forest
FRLFelling <- calcFRLFelling()

## Burning
FRLBurning <- calcFRLBurning()

## Fuelwood
FRLFuelwood <- calcFRLFuelwood()

## Hardwood Plantations
FRLHardwoodPlantations <- calcFRLHardwoodPlantations()

## Softwood Plantations
FRLSoftwoodPlantations <- calcFRLSoftwoodPlantations()

## Gross and Net Calc for Hardwood and Softwood Plantations
FRLPlantations <- calcFRLPlantations()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit = "auto"))

# The final table ##################################################################
if (debug_frl | show_output) {
    print(FRLDeforestation)
    print(FRLFelling)
    print(FRLBurning)
    print(FRLFuelwood)
    print(FRLHardwoodPlantations)
    print(FRLSoftwoodPlantations)
    print(FRLPlantations)
}

save(
  list = c(
    "FRLDeforestation",
    "FRLFelling",
    "FRLBurning",
    "FRLFuelwood",
    "FRLHardwoodPlantations",
    "FRLSoftwoodPlantations",
    "FRLPlantations"
  ),
  file = getDataPath("fiji_frl_estimate_values.RData")
)
