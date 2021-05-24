



createUC_MV_Values <- function(MV) {
  result <- list()

  #### Values with Uncertainty #####

  result$DFAreaLow <- ValueWithUncertainty(
    Value = MV$DFAreaLow,
    LowerCI = MV$DFAreaLow_LCI,
    UpperCI = MV$DFAreaLow_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$DFAreaLow) <- c("DFAreaLow")

  result$DFAreaUp <- ValueWithUncertainty(
    Value = MV$DFAreaUp,
    LowerCI = MV$DFAreaUp_LCI,
    UpperCI = MV$DFAreaUp_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$DFAreaUp) <- c("DFAreaUp")

  result$ARArea <- ValueWithUncertainty(
    Value = MV$ARArea,
    LowerCI = MV$ARArea_LCI,
    UpperCI = MV$ARArea_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$ARArea) <- c("ARArea")

  result$FDFellArea <- ValueWithUncertainty(
    Value = MV$FDFellArea,
    LowerCI = MV$FDFellArea - MV$FDFellArea * ErrAreaFell,
    UpperCI = MV$FDFellArea + MV$FDFellArea * ErrAreaFell,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FDFellArea) <- c("FDFellArea")

  return(result)
}
