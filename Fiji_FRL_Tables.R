
# Load all necessary data
load(file = "./Data/fiji_frl_input.RData")

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

# This number was used to generate the chk file.
MCRuns <- 1.5e+06
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- TRUE #Turn printed output on
show_output <- TRUE #Turn final table printed output on


source(file = "./Baseline_Values/FRL_Parameters.R")

# End of Parameters -- Start of calculations #######################################################
####################################################################################################


# Load all necessary data
load(file = "./Data/fiji_frl_adjusted_areas.RData")
load(file = "./Data/fiji_frl_emission_factors.RData")
load(file = "./Data/fiji_frl_estimate_values.RData")

## Degradation
FRLDegradation <- calcFRLDegradation()

## FRL Table
FRLTable <- debug(calcFRLTable)

# The final table ##################################################################
if (debug_frl | show_output) {
  print(FRLDegradation)
  print(FRLTable)
}

save(
  list = c(
    "AdjustedAreas",
    "EmissionFactors",
    "FRLDeforestation",
    "FRLFelling",
    "FRLBurning",
    "FRLFuelwood",
    "FRLHardwoodPlantations",
    "FRLSoftwoodPlantations",
    "FRLPlantations",
    "FRLDegradation",
    "FRLTable"
  ),
  file = "./Data/fiji_frl_tables.RData"
)


# The final FRL table ##################################################################
if (debug_frl | show_output) {
  FRLTable$frltab
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_FRL_Results.txt")
  print(FRLTable$frltab)
  sink()
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
