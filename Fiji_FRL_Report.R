

getDataPath<-function(filename) {
  return(paste0("./Data/frlUpdateOct22/", filename))
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

library(microbenchmark)

# Set up
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


print("Calculating FRL....")
print(paste("Runs -- ", FRLParams$runs))
timestamp <- Sys.time()
print(date())

# Load all necessary data
load(file = getDataPath("fiji_frl_adjusted_areas.RData"))
load(file = getDataPath("fiji_frl_emission_factors.RData"))
load(file = getDataPath("fiji_frl_estimate_values.RData"))


## FRL Table
FRLTable <- calcFRLTable()

## FRL Table
ErpaYearlyFRL  <- calcFRLMonitoringPeriodProjection()

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

# The final FRL table ##################################################################
if (debug_frl | show_output) {
  print(FRLTable$frltab)
  print(ErpaYearlyFRL$Projections)
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

save(
  list = c(
    "FRLTable",
    "ErpaYearlyFRL"
  ),
  file = getDataPath("fiji_frl_overall_years.RData")
)

if (debug_frl) print(sessionInfo())
