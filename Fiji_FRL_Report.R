
# Load all necessary data
load(file = "./Data/preMonitoringReport/fiji_frl_input.RData")
#load(file = "./Data/frlCorrection/fiji_frl_input.RData")
#aa_sample <- read.csv(file = "./Data/frlCorrection/aa_sample.csv")
#lcc_mapped_areas <- read.csv(file = "./Data/frlCorrection/lcc_mapped_areas.csv")

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)

library(microbenchmark)

# Set up
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))
options(max.print=50)

# This number was used to generate the chk file.
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- TRUE #Turn printed output on
show_output <- TRUE #Turn final table printed output on


source(file = "./Data/preMonitoringReport/FRL_Parameters.R")

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################


print("Calculating FRL....")
print(paste("Runs -- ", FRLParams$runs))
timestamp <- Sys.time()
print(date())

# DF = deforestation; AR = afforestation/reforestation

## Accuracy Assessment using bootstrap
AdjustedAreas <- calcFRLAdjustedAreas()

## Emissions Factors calculated from NFI
EmissionFactors <- calcEmissionFactors()

## Deforestation
FRLDeforestation <- calcFRLDeforestation()

## Felling in Natural Forest
FRLFelling <- calcFRLFelling()

## Burning
FRLBurning <- calcFRLBurning()

## Fuelwood
FRLFuelwood <- calcFRLFuelwood()

## Degradation
calcFRLDegradation()

## Hardwood Plantations
FRLHardwoodPlantations <- calcFRLHardwoodPlantations()

## Softwood Plantations
FRLSoftwoodPlantations <- calcFRLSoftwoodPlantations()

## Gross and Net Calc for Hardwood and Softwood Plantations
FRLPlantations <- calcFRLPlantations()

## FRL Table
FRLTable <- calcFRLTable()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

# The final FRL table ##################################################################
if (debug_frl | show_output) {
  print(FRLTable$frltab)
}

# FD = forest degradation
# EC = enhancement of forest carbon stocks
# AR = afforestation/reforestation

# aaeDF        # Gross emissions deforestation
# aaeFD_L      # Gross emissions FD logging
# aaeFD_BSW    # Gross emissions FD biomass. burning Softwood
# aaeEC_HS     # Gross emissions EC Hard- & Softwood Plantations
# aae_Combined # Gross emissions (all sources)

#  Gross removals ................................................
# aarFD_L       # Gross removals FD logging
# aarEC_AR      # Gross removals EC AR
# aarEC_HS      # Gross removals EC hard- & Softwood Plantations
# aar_Combined  # Gross removals (all sinks)

#  Net emissions .................................................
# aaneDF        # Net emissions deforestation
# aaneFD        # Net emissions forest degradation
# aaneEC        # Net emissions EC

if (debug_frl) print(sessionInfo())
