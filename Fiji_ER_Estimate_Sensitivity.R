# Note all CalcFunctions return CO2e values

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

options(digits = 8)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
#### Values used to calculate 2019-2020 output - about 4 hours
MCRuns <- 1.5e+06 #  number of runs in MC simulation - change as required
MCTolerance <- 0.0025
seed <- 08121976
set.seed(seed) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- TRUE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

source("./calcER_Estimate_Sensitivity.R")

print("Running ER Estimate Sensitivity...")
timestamp <- Sys.time()
print(date())

statusCallback <- function(perc_complete, notification) {
        if (missing(notification)) {
                      msg <- "Running ...."
              } else {
                      msg <- notification
              }
        if (!missing(perc_complete)) {
                      msg <- paste0(msg, " [", round(perc_complete, 0), "% Complete]")
              }
        print(msg)
}

interrupted <- function() {
        return(FALSE)
}


calcEnv <- as.list(environment())

result <- CalcER_Estimate_Sensitivity(statusCallback, interrupted, calcEnv)


print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit = "auto"))

list2env(result$env, environment())

fullFilename <- paste(outputFilename, "RData", sep = ".")
save(
        list = outputSaveNames,
        file = paste(paste("./Data/MonitoringReport2021", fullFilename, sep = "/"))
)

if (debug_er | show_output) {
        old_width <- options("width" = 120)
        print(TEI_ValuesOrdered)
        #**************************************************************************
        # put results in txt file
        sink("./chks/Fiji_ER_EstimateResults_Sensitivity.txt")
        print(TEI_ValuesOrdered)
        sink()
        options(old_width)
}
