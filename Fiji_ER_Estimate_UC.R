
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

load(file = "./Data/MonitoringReport2021/Fiji_ER_Estimate_AccuracyAssessment.RData")
load(file = "./Data/MonitoringReport2021/Fiji_ER_Estimate_Params.RData")
load(file = "./Data/MonitoringReport2021/Fiji_ER_Estimate_Values.RData")

options(digits = 6)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 1.5e+03
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
seed <- 08121976
set.seed(seed) # Seed set to remove random nature of MC Analysis for LCI & UCI

#### Values used to calculate 2019-2020 output
# MCRuns <- 1.5e+06
# MCTolerance <- 0.0025
# set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI


debug_er <- FALSE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

source("./calcER_Estimate_UC.R")

print("Running ER Estimate Uncertainty...")
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

result <- CalcER_Estimate_UC(statusCallback, interrupted, calcEnv)


print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit="auto"))

list2env(result$env,environment())


if (debug_er) {
  print(Table4_2)
  print(Table4_3)
  print(Table5_2_2)
  print(Table7_2)
  print(Table8)
}

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


if (debug_er) {
  print(ResultsTables)
}

save(
  list = outputSaveNames,
  file = paste("./Data/MonitoringReport2021", outputFilename, sep="/")
)

if (debug_er | show_output) {
  old_width <- options("width" = 120)
  #**************************************************************************
  # put results in txt file
  sink("./chks/Fiji_ER_EstimateResults_UC.txt")
  print(ResultsTables)
  print(MR_Values)
  sink()
  options(old_width)
}


