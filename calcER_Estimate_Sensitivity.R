# R code to calculate annual emissions(yr-1) for comparison with base FRL

# setwd("C:\eas-2018-prj\FijiGov\NFMSIntegrationFramework\code\calcs")

# Required source files
load(file = "./Data/fiji_frl_input.RData")
# Note all CalcFunctions return CO2e values

# new line

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(ValueWithUncertainty)
library(MonteCarloUtils)
library(FijiNFMSCalculations)

# Set up
options(digits = 8)
options(show.error.locations = TRUE)

# This number was used to generate the chk file.
MCRuns <- 500
# MCRuns <- 1.5e+06 # limit the number of runs in MC simulation - change as required
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

#### Values used to calculate 2019-2020 output - about 4 hours
# MCRuns <- 1.5e+06 #  number of runs in MC simulation - change as required
# MCTolerance <- 0.0025
# set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples

### Start of Calc ####
CalcERSens <- function(statusCallback, interrupted, calcEnv) {
  
  
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
  

  
  formatDecimal <- function(x) {
    return(format(round(x, 4), nsmall = 4))
  }
  
  checkStatus(10, "Calculating EmRem values....")
  
  # print(date())
  # 
  EmRems_Values <- list()
  EmRems_Values$year1 <- CalcEmRemsValues(MonitoredValues$year1)
  EmRems_Values$year2 <- CalcEmRemsValues(MonitoredValues$year2)
  # 
  # 
  checkStatus(30, "Calculating ER values....")
  # print(date())
  # 
  ER_Values <- CalcERValues(
    EmRems_Values,
    MonitoringReportParams$ErpaYearlyFRL,
    MonitoringReportParams$ErpaYearlyFRLFDeg
  )
  
  UC_Values <- list()
  UC_Values <- createUC_Values()
  
  
  UC_MV_Values <- list()
  UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
  UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)
  
  
  UC_EmRems_Values <- list()
  
  checkStatus(50, "Calculating UC year 1 values....")
  # print(date())
  # 
  UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)
  # 
  # 
  checkStatus(60, "Calculating UC year 2 values....")
  # print(date())
  # 
  UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)
  # 
  checkStatus(80, "Calculating UC ER values....")
  # print(date())
  # 
  UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)
  # 
  
  ###
  # Total Effect Index of value (x) is  1 minus the variance effect of all
  # other variables.
  
  calcTEI <- function(newSamples, oldVar) {
    return(1 - (var(newSamples) / oldVar))
  }
  
  
  V_all <- var(UC_ER_Values$McMpEstERsDefEnh$MCresults)
  
  TEI_Values <- list()
  
  TEI_Values$params <- data.frame(
    name = character(length(UC_Values)),
    v = numeric(length(UC_Values)),
    stringsAsFactors = FALSE
  )
  # print("Running Sensitivity Analysis [Parameters]...")
  # print(date())
  # print(paste("Checking ", length(UC_Values), " parameters"))
  
  for (i in 1:length(UC_Values)) {
    UC_Values[[i]] <- ValueWithUncertaintyFixed(UC_Values[[i]])
    
    UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
    UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)
    
    UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)
    
    UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)
    
    UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)
    
    TEI_Values$params$name[i] <- names(UC_Values[i])
    TEI_Values$params$v[i] <- formatDecimal(calcTEI(UC_ER_Values$McMpEstERsDefEnh$MCresults, V_all))
    print(date())
    print(paste(TEI_Values$params$name[i], ": ", TEI_Values$params$v[i]))
    print(paste(i, "of ", length(UC_Values)))
    UC_Values[[i]] <- ValueWithUncertaintySampled(UC_Values[[i]])
  }
  
  TEI_Values$activityData <- data.frame(
    name = character(length(UC_MV_Values$year1)),
    v = numeric(length(UC_MV_Values$year1)),
    stringsAsFactors = FALSE
  )
 
  #  print("Running Sensitivity Analysis [Activity Data]...")
  #  print(date())
  #  print(paste("Checking ", length(UC_MV_Values$year1), " activitiy data values"))
  
  for (i in 1:length(UC_MV_Values$year1)) {
    UC_MV_Values$year1 <- createUC_MV_Values(MonitoredValues$year1)
    UC_MV_Values$year2 <- createUC_MV_Values(MonitoredValues$year2)
    
    UC_MV_Values$year1[[i]] <- ValueWithUncertaintyFixed(UC_MV_Values$year1[[i]])
    UC_MV_Values$year2[[i]] <- ValueWithUncertaintyFixed(UC_MV_Values$year2[[i]])
    
    
    UC_EmRems_Values$year1 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year1, EmRems_Values$year1, MonitoredValues$year1)
    
    UC_EmRems_Values$year2 <- createUC_EmRemsValues(UC_Values, UC_MV_Values$year2, EmRems_Values$year2, MonitoredValues$year2)
    
    UC_ER_Values <- createUC_ERValues(UC_EmRems_Values, UC_MV_Values, UC_Values, MonitoringReportParams)
    
    TEI_Values$activityData$name[i] <- names(UC_MV_Values$year1[i])
    TEI_Values$activityData$v[i] <- formatDecimal(calcTEI(UC_ER_Values$McMpEstERsDefEnh$MCresults, V_all))
    # print(date())
    # print(paste(TEI_Values$activityData$name[i], ": ", TEI_Values$activityData$v[i]))
    # print(paste(i, "of ", length(UC_MV_Values$year1)))
    UC_MV_Values$year1[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year1[[i]])
    UC_MV_Values$year2[[i]] <- ValueWithUncertaintySampled(UC_MV_Values$year2[[i]])
  }
  
  result <- list()
  result$html <-list()
  result$html$TEI_Values$params <- as.tags(HTML(TEI_Values$params %>%
                                                  kable("html") %>%
                                                  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
  ))
  result$html$TEI_Values$activityData <- as.tags(HTML(TEI_Values$activityData %>%
                                           kable("html") %>%
                                           kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))
  
  
  
  result$env <- mget(c(
    "TEI_Values")
    )
    
  
  return(result)
}
  


