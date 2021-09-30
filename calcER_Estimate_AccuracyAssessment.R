
# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

outputFilename <- "Fiji_ER_Estimate_AccuracyAssessment"
outputSaveNames <- c(
  "AdjustedAreas"
)





### Start of Calc ####
CalcER_Estimate_AccuracyAssessment <- function(statusCallback, interrupted, calcEnv) {
  MCRuns <<- calcEnv$MCRuns
  MCTolerance <<- calcEnv$MCTolerance

  list2env(calcEnv, environment())


  checkStatus <- function(status, notification) {
    # Check for user interrupts
    if (interrupted()) {
      print("Stopping...")
      stop("User Interrupt")
    }

    # Notify status file of progress
    statusCallback(status, notification)
  }

  AdjustedAreas <- CalcAdjustedAreas(lcc_mapped_areas, aa_sample, 2, progress = checkStatus)


  # Some results
  result <- list()
  result$env <-
    list()
  result$env <- mget(outputSaveNames)
  return(result)
}
