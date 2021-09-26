# Note all CalcFunctions return CO2e values

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

load(file = "./Data/MonitoringReport2021/Fiji_ER_Estimate_AccuracyAssessment.RData")
load(file = "./Data/MonitoringReport2021/Fiji_ER_Estimate_Params.RData")

options(digits = 6)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 1000
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
seed <- 08121976
set.seed(seed) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

source("./calcER_Estimate_Values.R")

print("Running ER Estimate Values....")
timestamp <- Sys.time()
print(date())

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
  return(FALSE)
}


calcEnv <- as.list(environment())

result <- CalcER_Estimate_Values(statusCallback, interrupted, calcEnv)

print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

list2env(result$env,environment())

if (debug_er) {
  print(Table4_2)
  print(Table4_3)
  print(MonitoredValues)
  print(EmRems_Values)
  print(ER_Values)
}

save(
  list = outputSaveNames,
  file = paste("./Data/MonitoringReport2021", outputFilename, sep="/")
)

if (debug_er | show_output) {
  old_width <- options("width" = 120)
  print(MR_Values)
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_ER_EstimateResults_Values.txt")
  #print(MonitoredValues)

  print("***** EmRems_Values *******")
  print(EmRems_Values)
  print("****** ER_Values *******")
  print(ER_Values)
  print("***** MR_Values ********")
  print(MR_Values)
  sink()
  options(old_width)
}

