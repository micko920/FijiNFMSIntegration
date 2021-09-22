
# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

# This number was used to generate the chk file.
# MCRuns <- 100
# MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
# set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples



### Start of Calc ####
CalcER_Estimate_AccuracyAssessment <- function(statusCallback, interrupted,calcEnv) {
  
  MCRuns <<- calcEnv$MCRuns
  MCTolerance <<- calcEnv$MCTolerance
  
  list2env(calcEnv,environment())
  
  
  checkStatus <- function(status, notification) {
    # Check for user interrupts
    if (interrupted()) {
      print("Stopping...")
      stop("User Interrupt")
    }
    
    # Notify status file of progress
    statusCallback(status, notification)
  }
  
  AdjustedAreas <- CalcAdjustedAreas(lcc_mapped_areas, aa_sample, 1)

 
  #Some results
  results <- list()
  
  results$env <- mget(c(
     "AdjustedAreas"
   ))
  
  
  # results$html <-list()
  # results$html$done <- div("Run Finished - Adjusted Areas Calculated")

  return(results)
}