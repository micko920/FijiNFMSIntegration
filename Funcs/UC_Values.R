



createUC_Values <- function() {
  result <- list()

  ## TODO: these are still to be implimented with variance
  # BiomassToCarbonConv <- ValueWithUncertainty(
  #   Value = BiomassToCarbonConv,
  #   LowerCI = BiomassToCarbonConv_LCI,
  #   UpperCI = BiomassToCarbonConv_UCI,
  #   model = vwuTriangle, fixed = FALSE
  # )

  # # Combustion factor in Softwood Plantations (IPCC, 2006).
  # CombustFactor <- ValueWithUncertainty(
  #   Value = CombustFactor,
  #   LowerCI = CombustFactor - (CombustFactor * errCombustFactor),
  #   UpperCI = CombustFactor + (CombustFactor * errCombustFactor),
  #   model = vwuTriangle, fixed = FALSE
  # )


  # # Emission factor CO_2 (IPCC default)
  # EFCO2 <- rnorm(n = 1, mean = EFCO2, sd = EFCO2sd)

  # EFCH4 <- ValueWithUncertainty(
  #   Value = EFCH4,
  #   LowerCI = EFCH4 - (EFCH4 * errGHG),
  #   UpperCI = EFCH4 + (EFCH4 * errGHG),
  #   model = vwuTriangle, fixed = FALSE
  # )

  # EFN2O <- ValueWithUncertainty(
  #   Value = EFN2O,
  #   LowerCI = EFN2O - (EFN2O * errGHG),
  #   UpperCI = EFN2O + (EFN2O * errGHG),
  #   model = vwuTriangle, fixed = FALSE
  # )

  # # Uncertainty not included
  # #   GWPCO2 <- GWPCO2

  # GWPCH4 <- ValueWithUncertainty(
  #   Value = GWPCH4,
  #   LowerCI = GWPCH4 - (GWPCH4 * errGHG),
  #   UpperCI = GWPCH4 + (GWPCH4 * errGHG),
  #   model = vwuTriangle, fixed = FALSE
  # )

  # GWPN2O <- ValueWithUncertainty(
  #   Value = GWPN2O,
  #   LowerCI = GWPN2O - (GWPN2O * errGHG),
  #   UpperCI = GWPN2O + (GWPN2O * errGHG),
  #   model = vwuTriangle, fixed = FALSE
  # )

  #### Values with Uncertainty #####

  result$EFDeforUp <- ValueWithUncertainty(
    Value = EFDeforUp,
    LowerCI = EFDeforUp_LCI,
    UpperCI = EFDeforUp_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$EFDeforUp) <- c("EFDeforUp")

  result$EFDeforLow <- ValueWithUncertainty(
    Value = EFDeforLow,
    LowerCI = EFDeforLow_LCI,
    UpperCI = EFDeforLow_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$EFDeforLow) <- c("EFDeforLow")

  result$TEF <- ValueWithUncertainty(
    Value = TEF,
    LowerCI = TEF - TEF * errTEF,
    UpperCI = TEF + TEF * errTEF,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$TEF) <- c("TEF")

  result$MAIBsw <- ValueWithUncertainty(
    Value = MAIBsw,
    LowerCI = MAIBsw - MAIBsw * errMAIBsw,
    UpperCI = MAIBsw + MAIBsw * errMAIBsw,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$MAIBsw) <- c("MAIBsw")

  result$MAICFell <- ValueWithUncertainty(
    Value = MAICFell,
    LowerCI = MAICFell - MAICFell * ErrMAICFell,
    UpperCI = MAICFell + MAICFell * ErrMAICFell,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$MAICFell) <- c("MAICFell")

  result$MAIVar <- ValueWithUncertainty(
    Value = MAIVar,
    LowerCI = MAIVar - (MAIVar * errMAIVar),
    UpperCI = MAIVar + (MAIVar * errMAIVar),
    model = vwuTriangle, fixed = FALSE
  )
  names(result$MAIVar) <- c("MAIVar")

  result$MAIVhw <- ValueWithUncertainty(
    Value = MAIVhw,
    LowerCI = MAIVhw - MAIVhw * errMAIVhw,
    UpperCI = MAIVhw + MAIVhw * errMAIVhw,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$MAIVhw) <- c("MAIVhw")

  result$BiomassConvExpansionAR <- ValueWithUncertainty(
    Value = BiomassConvExpansionAR,
    LowerCI = BiomassConvExpansionAR - (BiomassConvExpansionAR * errBiomassConvExpansionAR),
    UpperCI = BiomassConvExpansionAR + (BiomassConvExpansionAR * errBiomassConvExpansionAR),
    model = vwuTriangle, fixed = FALSE
  )
  names(result$BiomassConvExpansionAR) <- c("BiomassConvExpansionAR")

  result$BiomassConvExpansionHW <- ValueWithUncertainty(
    Value = BiomassConvExpansionHW,
    LowerCI = BiomassConvExpansionHW - BiomassConvExpansionHW * errBiomassConvExpansionHW,
    UpperCI = BiomassConvExpansionHW + BiomassConvExpansionHW * errBiomassConvExpansionHW,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$BiomassConvExpansionHW) <- c("BiomassConvExpansionHW")

  # Random realization of BCEF_IM
  result$BiomassConvExpansionIncHW <- ValueWithUncertainty(
    Value = BiomassConvExpansionIncHW,
    LowerCI = BiomassConvExpansionIncHW - BiomassConvExpansionIncHW * errBiomassConvExpansionIncHW,
    UpperCI = BiomassConvExpansionIncHW + BiomassConvExpansionIncHW * errBiomassConvExpansionIncHW,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$BiomassConvExpansionIncHW) <- c("BiomassConvExpansionIncHW")

  result$RootToShootTropRain <- ValueWithUncertainty(
    Value = RootToShootTropRain,
    LowerCI = RootToShootTropRain - (RootToShootTropRain * errRootToShootTropRain),
    UpperCI = RootToShootTropRain + (RootToShootTropRain * errRootToShootTropRain),
    model = vwuTriangle, fixed = FALSE
  )
  names(result$RootToShootTropRain) <- c("RootToShootTropRain")

  result$RootToShootDryLandSmall <- ValueWithUncertainty(
    Value = RootToShootDryLandSmall,
    LowerCI = errLowerRtSDryLandSmall,
    UpperCI = errUpperRtSDryLandSmall,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$RootToShootDryLandSmall) <- c("RootToShootDryLandSmall")

  result$RootToShootDryLandBig <- ValueWithUncertainty(
    Value = RootToShootDryLandBig,
    LowerCI = errLowerRtSDryLandBig,
    UpperCI = errUpperRtSDryLandBig,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$RootToShootDryLandBig) <- c("RootToShootDryLandBig")

  result$Recovery <- ValueWithUncertainty(
    Value = RecoveryRateSW,
    LowerCI = qnorm(p = c(QLCI), mean = RecoveryRateSW, sd = (RecoveryRateSW * errRecoveryRateSW)),
    UpperCI = qnorm(p = c(QUCI), mean = RecoveryRateSW, sd = (RecoveryRateSW * errRecoveryRateSW)),
    model = vwuNorm, fixed = FALSE
  )
  names(result$Recovery) <- c("Recovery")

  result$WoodDensity <- ValueWithUncertainty(
    Value = WoodDensitySW,
    LowerCI = qnorm(p = c(QLCI), mean = WoodDensitySW, sd = SDWoodDensitySW),
    UpperCI = qnorm(p = c(QUCI), mean = WoodDensitySW, sd = SDWoodDensitySW),
    model = vwuNorm, fixed = FALSE
  )
  names(result$WoodDensity) <- c("WoodDensity")

  result$FRL <- ValueWithUncertainty(
    Value = FRL,
    LowerCI = FRL_LCI,
    UpperCI = FRL_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FRL) <- c("FRL")

  result$FRLDeforestation <- ValueWithUncertainty(
    Value = FRLDeforestation,
    LowerCI = FRLDeforestation_LCI,
    UpperCI = FRLDeforestation_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FRLDeforestation) <- c("FRLDeforestation")

  result$FRLForestDegradation <- ValueWithUncertainty(
    Value = FRLForestDegradation,
    LowerCI = FRLForestDegradation_LCI,
    UpperCI = FRLForestDegradation_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FRLForestDegradation) <- c("FRLForestDegradation")

  result$FRLRemovalsBySinks <- ValueWithUncertainty(
    Value = FRLRemovalsBySinks,
    LowerCI = FRLRemovalsBySinks_LCI,
    UpperCI = FRLRemovalsBySinks_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FRLRemovalsBySinks) <- c("FRLRemovalsBySinks")

  return(result)
}
