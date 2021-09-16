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

# This number was used to generate the chk file.
# MCRuns <- 1.5e+03
MCRuns <- 100
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI


debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples

### Start of Calc ####
CalcERUC <- function(statusCallback, interrupted,calcEnv) {
  
  
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
  MR_Values <- create_MRValues(UC_ER_Values, ER_Values, EmRems_Values, MonitoredValues, MonitoringReportParams)


  Table4_2 <- createTable_4_2(MR_Values)

  Table4_3 <- createTable_4_3(MR_Values, MonitoringReportParams)

  Table5_2_2 <- createTable_5_2_2(MR_Values)

  Table7_2 <- createTable_7_2(MR_Values, MonitoringReportParams)

  Table8 <- createTable_8(MR_Values, MonitoringReportParams)

  ResultsTables <- list()
  ResultsTables$year1 <- data.frame(
    stratum = c(
      "Deforestation",
      "Forest Deg (felling)",
      "Forest Deg (fire)",
      "Forest Plantations",
      "Sum Emissions",
      "Forest Deg (felling)",
      "Afforestation",
      "Forest Plantations",
      "Sum Removals",
      "Deforestation",
      "Forest Degradation",
      "Enhancement",
      "Total"
    ),
    Estimate = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year1$McEstEmFell$value[[1]],
      UC_EmRems_Values$year1$McEstEmFire$value[[1]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[1]],
      UC_EmRems_Values$year1$McGrossEm$value[[1]],
      UC_EmRems_Values$year1$McEstRemFell$value[[1]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[1]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[1]],
      UC_EmRems_Values$year1$McGrossRem$value[[1]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year1$McFDeg$value[[1]],
      UC_EmRems_Values$year1$McEnh$value[[1]],
      UC_EmRems_Values$year1$McNetEmRems$value[[1]]
    ),
    LCI = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year1$McEstEmFell$value[[2]],
      UC_EmRems_Values$year1$McEstEmFire$value[[2]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[2]],
      UC_EmRems_Values$year1$McGrossEm$value[[2]],
      UC_EmRems_Values$year1$McEstRemFell$value[[2]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[2]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[2]],
      UC_EmRems_Values$year1$McGrossRem$value[[2]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year1$McFDeg$value[[2]],
      UC_EmRems_Values$year1$McEnh$value[[2]],
      UC_EmRems_Values$year1$McNetEmRems$value[[2]]
    ),
    UCI = c(
      UC_EmRems_Values$year1$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year1$McEstEmFell$value[[3]],
      UC_EmRems_Values$year1$McEstEmFire$value[[3]],
      UC_EmRems_Values$year1$McGrossEmFPln$value[[3]],
      UC_EmRems_Values$year1$McGrossEm$value[[3]],
      UC_EmRems_Values$year1$McEstRemFell$value[[3]],
      UC_EmRems_Values$year1$McEstRemARefor$value[[3]],
      UC_EmRems_Values$year1$McGrossRemFPln$value[[3]],
      UC_EmRems_Values$year1$McGrossRem$value[[3]],
      UC_EmRems_Values$year1$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year1$McFDeg$value[[3]],
      UC_EmRems_Values$year1$McEnh$value[[3]],
      UC_EmRems_Values$year1$McNetEmRems$value[[3]]
    )
  )
  
  ResultsTables$year2 <- data.frame(
    stratum = c(
      "Deforestation", "Forest Deg (felling)", "Forest Deg (fire)",
      "Forest Plantations", "Sum Emissions", "Forest Deg (felling)", "Afforestation",
      "Forest Plantations", "Sum Removals", "Deforestation", "Forest Degradation", "Enhancement", "Total"
    ),
    Estimate = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year2$McEstEmFell$value[[1]],
      UC_EmRems_Values$year2$McEstEmFire$value[[1]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[1]],
      UC_EmRems_Values$year2$McGrossEm$value[[1]],
      UC_EmRems_Values$year2$McEstRemFell$value[[1]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[1]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[1]],
      UC_EmRems_Values$year2$McGrossRem$value[[1]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[1]],
      UC_EmRems_Values$year2$McFDeg$value[[1]],
      UC_EmRems_Values$year2$McEnh$value[[1]],
      UC_EmRems_Values$year2$McNetEmRems$value[[1]]
    ),
    LCI = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year2$McEstEmFell$value[[2]],
      UC_EmRems_Values$year2$McEstEmFire$value[[2]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[2]],
      UC_EmRems_Values$year2$McGrossEm$value[[2]],
      UC_EmRems_Values$year2$McEstRemFell$value[[2]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[2]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[2]],
      UC_EmRems_Values$year2$McGrossRem$value[[2]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[2]],
      UC_EmRems_Values$year2$McFDeg$value[[2]],
      UC_EmRems_Values$year2$McEnh$value[[2]],
      UC_EmRems_Values$year2$McNetEmRems$value[[2]]
    ),
    UCI = c(
      UC_EmRems_Values$year2$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year2$McEstEmFell$value[[3]],
      UC_EmRems_Values$year2$McEstEmFire$value[[3]],
      UC_EmRems_Values$year2$McGrossEmFPln$value[[3]],
      UC_EmRems_Values$year2$McGrossEm$value[[3]],
      UC_EmRems_Values$year2$McEstRemFell$value[[3]],
      UC_EmRems_Values$year2$McEstRemARefor$value[[3]],
      UC_EmRems_Values$year2$McGrossRemFPln$value[[3]],
      UC_EmRems_Values$year2$McGrossRem$value[[3]],
      UC_EmRems_Values$year2$McGrossEmDefor$value[[3]],
      UC_EmRems_Values$year2$McFDeg$value[[3]],
      UC_EmRems_Values$year2$McEnh$value[[3]],
      UC_EmRems_Values$year2$McNetEmRems$value[[3]]
    )
  )
  
  

  #Some results
  result <- list()

  result$env <- mget(c(
    "ResultsTables",
    "EmRems_Values",
    "ER_Values",
    "MR_Values",
    "UC_Values",
    "UC_MV_Values",
    "UC_EmRems_Values",
    "Table4_2",
    "Table4_3",
    "Table5_2_2",
    "Table7_2",
    "Table8"
  ))

   
  result$html <-list()
  # result$html$myEnv <- lapply(names(environment()), div)
 
  
  result$html$Table4_2 <- as.tags(HTML(Table4_2 %>%
                                          kable("html") %>%
                                          kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
   ))
  result$html$Table4_3 <- as.tags(HTML(Table4_3 %>%
                                         kable("html") %>%
                                         kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
  ))
  result$html$Table5_2_2 <- as.tags(HTML(Table5_2_2 %>%
                                           kable("html") %>%
                                           kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))
  result$html$Table7_2 <- as.tags(HTML(Table7_2 %>%
                                           kable("html") %>%
                                           kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))
  result$html$Table8 <- as.tags(HTML(Table8 %>%
                                           kable("html") %>%
                                           kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
    ))
  
  return(result)
}