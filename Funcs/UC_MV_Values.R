



createUC_MV_Values <- function(MV) {
  result <- list()

  #### Values with Uncertainty #####

  result$DeforAreaLow <- ValueWithUncertainty(
    Value = MV$DeforAreaLow,
    LowerCI = MV$DeforAreaLow_LCI,
    UpperCI = MV$DeforAreaLow_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$DeforAreaLow) <- c("DeforAreaLow")

  result$DeforAreaUp <- ValueWithUncertainty(
    Value = MV$DeforAreaUp,
    LowerCI = MV$DeforAreaUp_LCI,
    UpperCI = MV$DeforAreaUp_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$DeforAreaUp) <- c("DeforAreaUp")

  result$ARArea <- ValueWithUncertainty(
    Value = MV$ARArea,
    LowerCI = MV$ARArea_LCI,
    UpperCI = MV$ARArea_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$ARArea) <- c("ARArea")

  result$FDegFellArea <- ValueWithUncertainty(
    Value = MV$FDegFellArea,
    LowerCI = MV$FDegFellArea - MV$FDegFellArea * ErrAreaFell,
    UpperCI = MV$FDegFellArea + MV$FDegFellArea * ErrAreaFell,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FDegFellArea) <- c("FDegFellArea")

  return(result)
}
