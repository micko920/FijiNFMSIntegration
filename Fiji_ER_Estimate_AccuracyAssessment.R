

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
library(FijiNFMSCalculations)

getDataPath<-function(filename) {
  return(paste0("./Data/mrUpdateOct23/", filename))
}

aa_sample <- read.csv(file = getDataPath("aa_sample.csv"))
lcc_mapped_areas <- read.csv(file = getDataPath("lcc_mapped_areas.csv"))
load(file = getDataPath("Fiji_ER_Estimate_Params.RData"))

options("width" = 220)
options(digits = 6)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
MCRuns <- 10000
#MCRuns <- 100
MCTolerance <- 0.0115 # how stable the UCI and LCI should be before stopping
seed <- 08121976
set.seed(seed) # Seed set to remove random nature of MC Analysis for LCI & UCI

debug_er <- TRUE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- FALSE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################



source("./calcER_Estimate_AccuracyAssessment.R")

pdf(paste0(outputFilename, ".pdf"))

print("Running Accuracy Assessment and generating adjusted areas....")
timestamp <- Sys.time()
print(date())



statusCallback <- function(perc_complete, notification) {
        if (missing(notification)) {
                msg <- "Running ...."
        } else {
                msg <- notification
        }
        if (!missing(perc_complete)) {
                msg <- paste0(msg, " [", perc_complete, "% Complete]")
        }
        print(msg)
}

interrupted <- function() {
        return(FALSE)
}


calcEnv <- as.list(environment())

## Accuracy Assessment using bootstrap
# Sub Monitoring period pro rata is handled in the report.
result <- CalcER_Estimate_AccuracyAssessment(statusCallback, interrupted, calcEnv)


print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit = "auto"))

list2env(result$env, environment())

fullFilename <- paste(outputFilename, "RData", sep = ".")
save(
        list = outputSaveNames,
        file = paste(getDataPath(fullFilename))
)


if (debug_er | show_output) {
        old_width <- options("width" = 220)
        #**************************************************************************
        # put results in txt file
        sink("./chks/Fiji_ER_EstimateResults_AccuracyAssessment.txt")
        print(AdjustedAreas)
        sink()
        options(old_width)
}
