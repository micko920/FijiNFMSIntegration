# Function to display final estimate, UCI and LCI

EstimateWithBounds <- function(CO2eEst, # calculated previously from function
                               CO2eFromMC, # calculated from MC simulation
                               Confidence = 0.9 #  confidence interval
) {
  Estimate <- CO2eEst
  LCI <- quantile(CO2eFromMC, CalcLowerCI(Confidence))
  UCI <- quantile(CO2eFromMC, CalcUpperCI(Confidence))
  df <- data.frame(source = ("Emissions"), Estimate, LCI, UCI)
  EmTable <- unlist(df[c(2, 3, 4)])
}

CalcLowerCI <- function(Confidence) {
  (1 - Confidence) / 2
}
CalcUpperCI <- function(Confidence) {
  1 - (1 - Confidence) / 2
}
