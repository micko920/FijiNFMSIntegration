


getDataPath<-function(filename) {
  return(paste0("./Data/frlAuditMay24/", filename))
}


# Load all necessary data
aa_sample <- read.csv(file = getDataPath("aa_sample.csv"))
lcc_mapped_areas <- read.csv(file = getDataPath("lcc_mapped_areas.csv"))


# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))
options(max.print = 50)
options("width" = 320)

MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping

debug_frl <- TRUE # Turn printed output on
debug_er <- TRUE # Turn printed output on
show_output <- TRUE # Turn final table printed output on



source(file = getDataPath("FRL_Parameters.R"))

MCRuns <- FRLParams$runs

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

outputFilename <- "Fiji_FRL_AccuracyAssessment"
pdf(paste0(outputFilename, ".pdf"))

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
  file = getDataPath("fiji_frl_adjusted_areas.RData")
)
