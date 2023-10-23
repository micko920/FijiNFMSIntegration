
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

getDataPath<-function(filename) {
  return(paste0("./Data/mrUpdateOct23/", filename))
}

load(file = getDataPath("Fiji_ER_Estimate_AccuracyAssessment.RData"))
load(file = getDataPath("Fiji_ER_Estimate_Params.RData"))
load(file = getDataPath("fiji_frl_overall_years.RData"))
load(file = getDataPath("Fiji_ER_Estimate_Values.RData"))

options(digits = 6)
options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

# This number was used to generate the chk file.
#### Values used to calculate 2019-2020 output
MCRuns <- 1.5e+06
#MCRuns <- 100000
MCTolerance <- 0.0025
seed <- 08121976
set.seed(seed) # Seed set to remove random nature of MC Analysis for LCI & UCI


debug_er <- TRUE # Turn printed output on
show_output <- TRUE # Turn final table printed output on
plot_mc_output <- TRUE # Turn on plots for MC samples

# End of Parameters -- Start of calculations #######################################################
####################################################################################################

source("./calcER_Estimate_UC.R")

pdf(paste0(outputFilename, ".pdf"))

print("Running ER Estimate Uncertainty...")
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

formatPercent <- function(x) {
  return(paste(format(round(x * 100, 2), nsmall = 2), "%"))
}

halfwidth <- function(value) {
  return( abs(
            ifelse(value[[2]] > value[[3]],
                 (value[[2]] - value[[3]]),
                 (value[[3]] - value[[2]]))/ 2
            )
  )
}

relativeMargin <- function(value) {
  return( abs(halfwidth(value) / value[[1]]) )
}

calcEnv <- as.list(environment())

result <- CalcER_Estimate_UC(statusCallback, interrupted, calcEnv)


print(date())
print("Execution time: ")
print(difftime(Sys.time(), timestamp, unit = "auto"))

list2env(result$env, environment())


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
        ),
        HW = c(
               halfwidth(UC_EmRems_Values$year1$McGrossEmDefor$value),
               halfwidth(UC_EmRems_Values$year1$McEstEmFell$value),
               halfwidth(UC_EmRems_Values$year1$McEstEmFire$value),
               halfwidth(UC_EmRems_Values$year1$McGrossEmFPln$value),
               halfwidth(UC_EmRems_Values$year1$McGrossEm$value),
               halfwidth(UC_EmRems_Values$year1$McEstRemFell$value),
               halfwidth(UC_EmRems_Values$year1$McEstRemARefor$value),
               halfwidth(UC_EmRems_Values$year1$McGrossRemFPln$value),
               halfwidth(UC_EmRems_Values$year1$McGrossRem$value),
               halfwidth(UC_EmRems_Values$year1$McGrossEmDefor$value),
               halfwidth(UC_EmRems_Values$year1$McFDeg$value),
               halfwidth(UC_EmRems_Values$year1$McEnh$value),
               halfwidth(UC_EmRems_Values$year1$McNetEmRems$value)
               ),
        RM = c(
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossEmDefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McEstEmFell$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McEstEmFire$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossEmFPln$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossEm$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McEstRemFell$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McEstRemARefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossRemFPln$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossRem$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McGrossEmDefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McFDeg$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McEnh$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year1$McNetEmRems$value))
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
        ),
        HW = c(
               halfwidth(UC_EmRems_Values$year2$McGrossEmDefor$value),
               halfwidth(UC_EmRems_Values$year2$McEstEmFell$value),
               halfwidth(UC_EmRems_Values$year2$McEstEmFire$value),
               halfwidth(UC_EmRems_Values$year2$McGrossEmFPln$value),
               halfwidth(UC_EmRems_Values$year2$McGrossEm$value),
               halfwidth(UC_EmRems_Values$year2$McEstRemFell$value),
               halfwidth(UC_EmRems_Values$year2$McEstRemARefor$value),
               halfwidth(UC_EmRems_Values$year2$McGrossRemFPln$value),
               halfwidth(UC_EmRems_Values$year2$McGrossRem$value),
               halfwidth(UC_EmRems_Values$year2$McGrossEmDefor$value),
               halfwidth(UC_EmRems_Values$year2$McFDeg$value),
               halfwidth(UC_EmRems_Values$year2$McEnh$value),
               halfwidth(UC_EmRems_Values$year2$McNetEmRems$value)
               ),
        RM = c(
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossEmDefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McEstEmFell$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McEstEmFire$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossEmFPln$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossEm$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McEstRemFell$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McEstRemARefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossRemFPln$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossRem$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McGrossEmDefor$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McFDeg$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McEnh$value)),
               formatPercent(relativeMargin(UC_EmRems_Values$year2$McNetEmRems$value))
               )
)


if (debug_er) {
        print(ResultsTables)
}


fullFilename <- paste(outputFilename, "RData", sep = ".")
save(
        list = outputSaveNames,
        file = paste(getDataPath(fullFilename))
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
