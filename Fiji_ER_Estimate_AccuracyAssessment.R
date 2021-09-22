
aa_sample <- read.csv(file = "./Data/aa_sample.csv")
lcc_mapped_areas <- read.csv(file = "./Data/lcc_mapped_areas.csv")
print(aa_sample)




# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

# Set upwarnin
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 1
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

source("./calcER_Estimate_AccuracyAssessment.R")

print("Running Accuracy Assessment and generating adjusted areas....")
timestamp <- Sys.time()
print(date())



## Accuracy Assessment using bootstrap, 2 years is 1 period.
# Sub Monitoring period pro rata is handled in the report.
# AdjustedAreas <- CalcAdjustedAreas(lcc_mapped_areas, aa_sample,1)

statusCallback <- function(perc_complete, notification) {
    if (missing(notification))
      msg <- "Running ...."
    else
      msg <- notification
    if (!missing(perc_complete))
      msg <- paste0(msg, " [", perc_complete, "% Complete]")
    print(msg)
}

interrupted <- function() {
  return(false)
}

calcEnv <- as.list(environment())

result <- CalcER_Estimate_AccuracyAssessment(statusCallback, interrupted, calcEnv) 

list2env(result$env,environment())

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))




if (debug_er | show_output) {
  old_width <- options("width" = 120)
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_ER_EstimateResults_AccuracyAssessment.txt")
  print(AdjustedAreas)
  sink()
  options(old_width)
}

save(
  list = c(
    "AdjustedAreas"
  ),
  file = "./Data/Fiji_ER_EstimateResults_AdjustedAreas.RData"
)
