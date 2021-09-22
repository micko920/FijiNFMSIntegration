

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

debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples

### Start of Calc ####
CalcER_Estimate_Sensitivity <- function(statusCallback, interrupted, calcEnv) {
  
  MCRuns <<- calcEnvExtended$MCRuns
  MCTolerance <<- calcEnvExtended$MCTolerance
  
  list2env(calcEnv,environment())
  
  # AA of AD is done on a monitoring period of 2 years.
  # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
  MonitoredValues$year1$DeforAreaLow <- AdjustedAreas$areaLoss[1] / 2
  MonitoredValues$year1$McDeforAreaLow <- AdjustedAreas$MCaadeforL / 2
  MonitoredValues$year2$DeforAreaLow <- AdjustedAreas$areaLoss[1] / 2
  MonitoredValues$year2$McDeforAreaLow <- AdjustedAreas$MCaadeforL / 2
  # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
  MonitoredValues$year1$DeforAreaUp <- AdjustedAreas$areaLoss[2] / 2
  MonitoredValues$year1$McDeforAreaUp <- AdjustedAreas$MCaadeforU / 2
  MonitoredValues$year2$DeforAreaUp <- AdjustedAreas$areaLoss[2] / 2
  MonitoredValues$year2$McDeforAreaUp <- AdjustedAreas$MCaadeforU / 2
  # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
  # AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  # AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  MonitoredValues$year1$AReforArea <- AdjustedAreas$MCaaaforMean / 2
  MonitoredValues$year1$McAReforArea <- rowSums(AdjustedAreas$MCaaafor) /2
  MonitoredValues$year2$AReforArea <- AdjustedAreas$MCaaaforMean / 2
  MonitoredValues$year2$McAReforArea <- rowSums(AdjustedAreas$MCaaafor) /2
  
  
  
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
  
  TEI_ValuesParamslessFRL <- data.frame(TEI_Values$params [-c(16),])
  TEI_ValuesForFIMS <- rbind(TEI_ValuesParamslessFRL,TEI_Values$activityData)
  TEI_ValuesOrdered <- TEI_ValuesForFIMS[order(TEI_ValuesForFIMS[,2]),]
 
  
  result <- list()

  result$env <- mget(c(
    "TEI_ValuesOrdered")
    )
    
  
  return(result)
}
  


