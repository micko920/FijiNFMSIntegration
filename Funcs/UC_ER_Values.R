

calcUCModel <- function(lci, median, uci) {
  result <- list()
  result$median <- median
  result$UCI <- uci
  result$LCI <- lci
  result$halfWidth <- (uci - lci) / 2
  result$relativeMargin <- result$halfWidth / median
  result$conserFactor <- 0.15
  if (result$relativeMargin <= 0.15) {
    result$conserFactor <- 0
  } else if (result$relativeMargin > 0.15 && result$relativeMargin <= 0.3) {
    result$conserFactor <- 0.04
  } else if (result$relativeMargin > 0.3 && result$relativeMargin <= 0.6) {
    result$conserFactor <- 0.08
  } else if (result$relativeMargin > 0.6 && result$relativeMargin <= 1) {
    result$conserFactor <- 0.12
  }
  return(result)
}

createUC_EmRemsValues <- function(UC, UC_MV, EmRems, MV) {
  result <- list()
  local <- list()

  ################# 1. Deforestation (DF) #############

  # 1.3 Net emisions from deforestation: = AD (area) x EF (emission factor)



  # Emission estimates for deforestation (tCO2e):
  CalcEmDFUpArgs <- function() {
    return(list(UC_MV$DFAreaUp, UC$EFDFUp))
  }

  EmDFUpFinal <- CalcMonteCarlo("EmEstDFUp", EmRems$EmEstDFUp, CalcEmDF, CalcEmDFUpArgs)
  if (debug_er) EmDFUpFinal

  local$EmEstDFUp <- ValueWithUncertainty(
    Value = EmRems$EmEstDFUp,
    LowerCI = EmDFUpFinal$value[2],
    UpperCI = EmDFUpFinal$value[3],
    model = create_vwuSampled(EmDFUpFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstDFUp) <- c("EmEstDFUp")

  CalcEmDFLowArgs <- function() {
    return(list(UC_MV$DFAreaLow, UC$EFDFLow))
  }


  EmDFLowFinal <- CalcMonteCarlo("EmEstDFLow", EmRems$EmEstDFLow, CalcEmDF, CalcEmDFLowArgs)
  if (debug_er) EmDFLowFinal
  local$EmEstDFLow <- ValueWithUncertainty(
    Value = EmRems$EmEstDFLow,
    LowerCI = EmDFLowFinal$value[2],
    UpperCI = EmDFLowFinal$value[3],
    model = create_vwuSampled(EmDFLowFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstDFLow) <- c("EmEstDFLow")

  # Total emissions from Deforestation (tCO2e)
  CalcEmTotalDFArgs <- function() {
    return(list(local$EmEstDFUp, local$EmEstDFLow))
  }


  ## MGG - UC
  # Final Estimate of emissions with UCI and LCI
  result$EmEstDFTotalFinal <- CalcMonteCarlo("EmEstDFTotal", EmRems$EmEstDFTotal, CalcEmTotalDF, CalcEmTotalDFArgs)
  if (debug_er) result$EmEstDFTotalFinal
  local$EmEstDFTotal <- ValueWithUncertainty(
    Value = EmRems$EmEstDFTotal,
    LowerCI = result$EmEstDFTotalFinal$value[2],
    UpperCI = result$EmEstDFTotalFinal$value[3],
    model = create_vwuSampled(result$EmEstDFTotalFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstDFTotal) <- c("EmEstDFTotal")

  #################### 2. Forest Degradation (FD) ########

  ## 2.1 Felling in Natural Forest (Emissions and Removals)

  ## Yearly EMISSIONS from felling in natural forest (tCO2e)

  # Estimate of CO2e emissions from felling
  # Uncertainty associated with the TEF, create arguments for MC simuation i.e. random value for TEF,
  # Felling Volume uncertainty not included in MC, just use actual volume

  CalcEmFellArgs <- function() {
    return(list(UC$TEF, MV$FDFellVol))
  }


  ## MGG - UC
  result$EmEstFellFinal <- CalcMonteCarlo("EmEstFell", EmRems$EmEstFell, CalcEmFell, CalcEmFellArgs)
  if (debug_er) result$EmEstFellFinal
  local$EmEstFell <- ValueWithUncertainty(
    Value = EmRems$EmEstFell,
    LowerCI = result$EmEstFellFinal$value[2],
    UpperCI = result$EmEstFellFinal$value[3],
    model = create_vwuSampled(result$EmEstFellFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstFell) <- c("EmEstFell")

  ## Yearly REMOVALS from felling in natural forest (tCO2e)
  #  no need to (* delta t) as delta t = 1 for 1 year
  # Estimate of CO2e removals from felling
  # Uncertainty associated with the with MAIC and Area Felled
  # Calculate the arguments
  CalcRemFellArgs <- function() {
    return(list(UC_MV$FDFellArea, UC$MAICFell))
  }


  ## MGG - UC
  # Final Estimate for Removals with UCI and LCI
  result$RemEstFellFinal <- CalcMonteCarlo("RemEstFell", EmRems$RemEstFell, CalcRemFell, CalcRemFellArgs)
  if (debug_er) result$RemEstFellFinal
  local$RemEstFell <- ValueWithUncertainty(
    Value = EmRems$RemEstFell,
    LowerCI = result$RemEstFellFinal$value[2],
    UpperCI = result$RemEstFellFinal$value[3],
    model = create_vwuSampled(result$RemEstFellFinal$MCresults), fixed = FALSE
  )
  names(local$RemEstFell) <- c("RemEstFell")

  CalcEmTotalFellArgs <- function() {
    return(list(local$EmEstFell, local$RemEstFell))
  }

  # Final Estimate of emissions with UCI and LCI
  EmEstFellTotalFinal <- CalcMonteCarlo("EmEstFellTotal", EmRems$EstFellTotal, CalcEmTotalFell, CalcEmTotalDFArgs)
  if (debug_er) EmEstFellTotalFinal

  ##***********************************************************
  ## 2.2 Biomass Burning

  # Uncertainty analysis For Biomass Burning
  # Random inputs for the MC simulation
  CalcEmFireArgs <- function() {
    return(list(
      MV$FDBurnData$age_yrs, UC$MAIBsw, UC$RootToShootDryLandSmall, MV$FDBurnData$area_ha
    ))
  }

  ## MGG - UC
  # Yearly Emissions from biomass burning  (tCO2e)
  result$EmFireFinal <- CalcMonteCarlo("EmFire", EmRems$EmFire, CalcEmFire, CalcEmFireArgs)
  local$EmFire <- ValueWithUncertainty(
    Value = EmRems$EmFire,
    LowerCI = result$EmFireFinal$value[2],
    UpperCI = result$EmFireFinal$value[3],
    model = create_vwuSampled(result$EmFireFinal$MCresults), fixed = FALSE
  )
  names(local$EmFire) <- c("EmFire")
  #*********************************************************
  # 2.3 Fuelwood - Excluded from ER calculations
  # **********************************************************

  #*******************************************************************
  ############## 3. Enhancement ##########
  # 3.1 Afforestation

  # Yearly Removals from Afforestation  (tCO2e)
  # Uncertainty with the estimated total MAICAGBar and Root To Shoot Tropical Rain Ratio
  # Calculate arguments
  CalcRemARArgs <- function() {
    return(list(UC_MV$ARArea, UC$MAIVar, UC$BiomassConvExpansionAR, UC$RootToShootTropRain))
  }


  ## MGG - UC
  result$RemEstARFinal <- CalcMonteCarlo("RemEstAR", EmRems$RemEstAR, CalcRemARTotal, CalcRemARArgs)
  if (debug_er) result$RemEstARFinal
  local$RemEstAR <- ValueWithUncertainty(
    Value = EmRems$RemEstAR,
    LowerCI = result$RemEstARFinal$value[2],
    UpperCI = result$RemEstARFinal$value[3],
    model = create_vwuSampled(result$RemEstARFinal$MCresults), fixed = FALSE
  )
  names(local$RemEstAR) <- c("RemEstAR")
  # 3.2 Forest Plantations

  # Emissions Hardwood plantations ####
  # Uncertainty assessment (Monte Carlo simulations) related to Biomass Conversion and Expansion
  # Factor, Root To shoot Tropical Rain
  CalcEmFPHWArgs <- function() {
    return(list(MV$FPVolHarvHW, UC$BiomassConvExpansionHW, UC$RootToShootTropRain))
  }

  # Final Estimate for Emissions with UCI and LCI
  EmEstFPHWFinal <- CalcMonteCarlo("EmEstFPHW", EmRems$EmEstFPHW, CalcEmForPlantHW, CalcEmFPHWArgs)
  if (debug_er) EmEstFPHWFinal
  local$EmEstFPHW <- ValueWithUncertainty(
    Value = EmRems$EmEstFPHW,
    LowerCI = EmEstFPHWFinal$value[2],
    UpperCI = EmEstFPHWFinal$value[3],
    model = create_vwuSampled(EmEstFPHWFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstFPHW) <- c("EmEstFPHW")
  # Emissions Softwood plantations ####
  CalcEmFPSWArgs <- function() {
    return(list(MV$FPVolHarvSW, UC$Recovery, UC$WoodDensity, UC$RootToShootDryLandBig))
  }

  # Final Estimate for SW Emissions with UCI and LCI
  EmEstFPSWFinal <- CalcMonteCarlo("EmEstFPSW", EmRems$EmEstFPSW, CalcEmForPlantSW, CalcEmFPSWArgs)
  if (debug_er) EmEstFPSWFinal
  local$EmEstFPSW <- ValueWithUncertainty(
    Value = EmRems$EmEstFPSW,
    LowerCI = EmEstFPSWFinal$value[2],
    UpperCI = EmEstFPSWFinal$value[3],
    model = create_vwuSampled(EmEstFPSWFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstFPSW) <- c("EmEstFPSW")


  # Removals Hardwood plantations ####
  # Uncertainty assessment (removals) of MAIV, Biomass Conversion and Expansion Factor,
  # Root To Shoot Tropical Rain, MAIC, Average annual area of forest that just grows
  CalcRemFPHWArgs <- function() {
    return(list(MV$FPAreaJustGrowsHW, MV$FPAreaPlantHW, MV$FPAreaHarvestHW, UC$MAIVhw, UC$BiomassConvExpansionIncHW, UC$RootToShootTropRain))
  }


  # Final Estimate for HW Removals with UCI and LCI
  RemEstFPHWFinal <- CalcMonteCarlo("RemEstFPHW", EmRems$RemEstFPHW, CalcRemForPlantHW, CalcRemFPHWArgs)
  if (debug_er) RemEstFPHWFinal
  local$RemEstFPHW <- ValueWithUncertainty(
    Value = EmRems$RemEstFPHW,
    LowerCI = RemEstFPHWFinal$value[2],
    UpperCI = RemEstFPHWFinal$value[3],
    model = create_vwuSampled(RemEstFPHWFinal$MCresults), fixed = FALSE
  )
  names(local$RemEstFPHW) <- c("RemEstFPHW")
  # Estimate of softwood removals for yr (tCO2e) ####
  CalcRemFPSWArgs <- function() {
    return(list(UC$MAIBsw, MV$FPAreaJustGrowsSW, MV$FPAreaPlantSW, MV$FPAreaHarvestSW))
  }

  # Final Estimate for SW Removals with UCI and LCI
  RemEstFPSWFinal <- CalcMonteCarlo("RemEstFPSW", EmRems$RemEstFPSW, CalcRemForPlantSW, CalcRemFPSWArgs)
  if (debug_er) RemEstFPSWFinal
  local$RemEstFPSW <- ValueWithUncertainty(
    Value = EmRems$RemEstFPSW,
    LowerCI = RemEstFPSWFinal$value[2],
    UpperCI = RemEstFPSWFinal$value[3],
    model = create_vwuSampled(RemEstFPSWFinal$MCresults), fixed = FALSE
  )
  names(local$RemEstFPSW) <- c("RemEstFPSW")


  # **************************************************************
  # Gross emissions Forest Plantations (Hard- and Softwood)
  CalcEmEstFPTotalArgs <- function() {
    return(list(local$EmEstFPHW, local$EmEstFPSW))
  }

  ## MGG - UC
  # Gross emissions Forest Plantations (Hard- and Softwood)
  result$EmEstFPTotalFinal <- CalcMonteCarlo("EmEstFPTotal", EmRems$EmEstFPTotal, CalcEmEstTotalFP, CalcEmEstFPTotalArgs)
  if (debug_er) result$EmEstFPTotalFinal
  local$EmEstFPTotal <- ValueWithUncertainty(
    Value = EmRems$EmEstFPTotal,
    LowerCI = result$EmEstFPTotalFinal$value[2],
    UpperCI = result$EmEstFPTotalFinal$value[3],
    model = create_vwuSampled(result$EmEstFPTotalFinal$MCresults), fixed = FALSE
  )
  names(local$EmEstFPTotal) <- c("EmEstFPTotal")


  # Gross removals Forest Plantations (Hard- and Softwood)
  CalcRemEstFPTotalArgs <- function() {
    return(list(local$RemEstFPHW, local$RemEstFPSW))
  }

  ## MGG - UC
  # Gross removals Forest Plantations (Hard- and Softwood)
  result$RemEstFPTotalFinal <- CalcMonteCarlo("RemEstFPTotal", EmRems$RemEstFPTotal, CalcRemTotalFP, CalcRemEstFPTotalArgs)
  if (debug_er) result$RemFPTotalFinal
  local$RemEstFPTotal <- ValueWithUncertainty(
    Value = EmRems$RemEstFPTotal,
    LowerCI = result$RemEstFPTotalFinal$value[2],
    UpperCI = result$RemEstFPTotalFinal$value[3],
    model = create_vwuSampled(result$RemEstFPTotalFinal$MCresults), fixed = FALSE
  )
  names(local$RemEstFPTotal) <- c("RemEstFPTotal")

  # Net Emissions from Forest Plantations (Hard- and Softwood)
  CalcFPTotalArgs <- function() {
    return(list(local$EmEstFPTotal, local$RemEstFPTotal))
  }

  # Net Emissions from Forest Plantations (Hard- and Softwood)
  FPTotalFinal <- CalcMonteCarlo("FPTotal", EmRems$FPTotal, CalcTotalFP, CalcFPTotalArgs)
  local$FPTotal <- ValueWithUncertainty(
    Value = EmRems$FPTotal,
    LowerCI = FPTotalFinal$value[2],
    UpperCI = FPTotalFinal$value[3],
    model = create_vwuSampled(FPTotalFinal$MCresults), fixed = FALSE
  )
  names(local$FPTotal) <- c("FPTotal")

  FPTotalResults <- data.frame(
    stratum = c("Emissions", "Removals", "Total"),
    # Average annual emissions from Forest Degradation
    Estimate = c(result$EmEstFPTotalFinal$value[1], result$RemEstFPTotalFinal$value[1], FPTotalFinal$value[1]),
    # Lower confidence interval bound
    LCI = c(result$EmEstFPTotalFinal$value[2], result$RemEstFPTotalFinal$value[2], FPTotalFinal$value[2]),
    # Upper confidence interval bound
    UCI = c(result$EmEstFPTotalFinal$value[3], result$RemEstFPTotalFinal$value[3], FPTotalFinal$value[3])
  )
  if (debug_er) FPTotalResults


  #*************************************************************
  # 4. Final table of results

  # Gross Emissions Total
  CalcGrossEmTotalArgs <- function() {
    return(list(local$EmEstDFTotal, local$EmEstFell, local$EmFire, local$EmEstFPTotal))
  }

  ## MGG - UC
  result$GrossEmTotalFinal <- CalcMonteCarlo("GrossEmTotal", EmRems$GrossEmTotal, CalcGrossEmTotal, CalcGrossEmTotalArgs)
  local$GrossEmTotal <- ValueWithUncertainty(
    Value = EmRems$GrossEmTotal,
    LowerCI = result$GrossEmTotalFinal$value[2],
    UpperCI = result$GrossEmTotalFinal$value[3],
    model = create_vwuSampled(result$GrossEmTotalFinal$MCresults), fixed = FALSE
  )
  names(local$GrossEmTotal) <- c("GrossEmTotal")


  # Gross Emissions without degradation
  CalcGrossEmNoFDTotalArgs <- function() {
    return(list(local$EmEstDFTotal, local$EmEstFPTotal))
  }

  result$GrossEmissionsNoFDFinal <- CalcMonteCarlo("GrossEmissonsNoFD", EmRems$GrossEmNoFDTotal, CalcGrossEmNoFDTotal, CalcGrossEmNoFDTotalArgs)



  # Gross Removals Total
  CalcGrossRemTotalArgs <- function() {
    return(list(local$RemEstFell, local$RemEstAR, local$RemEstFPTotal))
  }

  ## MGG - UC
  result$GrossRemTotalFinal <- CalcMonteCarlo("GrossRemTotal", EmRems$GrossRemTotal, CalcGrossRemTotal, CalcGrossRemTotalArgs)
  local$GrossRemTotal <- ValueWithUncertainty(
    Value = EmRems$GrossRemTotal,
    LowerCI = result$GrossRemTotalFinal$value[2],
    UpperCI = result$GrossRemTotalFinal$value[3],
    model = create_vwuSampled(result$GrossRemTotalFinal$MCresults), fixed = FALSE
  )
  names(local$GrossRemTotal) <- c("GrossRemTotal")

  # Gross Removals without degradation
  CalcGrossRemNoFDTotalArgs <- function() {
    return(list(local$RemEstAR))
  }

  result$GrossRemovalsNoFDFinal <- CalcMonteCarlo("GrossRemovalsNoFD", EmRems$GrossRemNoFDTotal, CalcGrossRemNoFDTotal, CalcGrossRemNoFDTotalArgs)


  # Forest Degradation Total
  CalcFDEstArgs <- function() {
    return(list(local$EmEstFell, local$RemEstFell, local$EmFire))
  }

  ## MGG - UC
  result$FDFinal <- CalcMonteCarlo("FDFinal", EmRems$FDEst, CalcFDEst, CalcFDEstArgs)


  # Enhancement Total
  CalcECEstArgs <- function() {
    return(list(local$FPTotal, local$RemEstAR))
  }

  ## MGG - UC
  result$ECFinal <- CalcMonteCarlo("ECFinal", EmRems$ECEst, CalcECEst, CalcECEstArgs)


  # Net Emissions Total
  CalcNetEmTotalArgs <- function() {
    return(list(local$GrossEmTotal, local$GrossRemTotal))
  }

  ## MGG - UC
  result$NetEmissionsFinal <- CalcMonteCarlo("NetEmissions", EmRems$NetEmTotal, CalcNetEmTotal, CalcNetEmTotalArgs)

  return(result)
}



createUC_ERValues <- function(UC_EmRems, UC_MV, UC, MRparams) {
  result <- list()

  year1 <- list()
  year2 <- list()



  #############################################
  # Monitoring Period Net EmRems all activities
  year1$NetEm <- ValueWithUncertainty(
    Value = UC_EmRems$year1$NetEmissionsFinal$value[1],
    LowerCI = UC_EmRems$year1$NetEmissionsFinal$value[2],
    UpperCI = UC_EmRems$year1$NetEmissionsFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year1$NetEmissionsFinal$MCresults), fixed = FALSE
  )

  year2$NetEm <- ValueWithUncertainty(
    Value = UC_EmRems$year2$NetEmissionsFinal$value[1],
    LowerCI = UC_EmRems$year2$NetEmissionsFinal$value[2],
    UpperCI = UC_EmRems$year2$NetEmissionsFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year2$NetEmissionsFinal$MCresults), fixed = FALSE
  )

  NetEmValue <- CalcMpNetEmRems(
    ValueWithUncertaintyValue(year1$NetEm),
    ValueWithUncertaintyValue(year2$NetEm)
  )

  CalcMpNetEmRemsArgs <- function() {
    return(list(year1$NetEm, year2$NetEm))
  }

  # TODO: rename to ER$MpNetEmRems
  result$NetEmissionsFinal <- CalcMonteCarlo("NetEmissions", NetEmValue, CalcMpNetEmRems, CalcMpNetEmRemsArgs)

  #####################################################################
  # Monitoring Period ERs Defor and Enhancements (excluding Forest Deg)

  year1$MpEstEmRemsDefor <- ValueWithUncertainty(
    Value = UC_EmRems$year1$EmEstDFTotalFinal$value[1],
    LowerCI = UC_EmRems$year1$EmEstDFTotalFinal$value[2],
    UpperCI = UC_EmRems$year1$EmEstDFTotalFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year1$EmEstDFTotalFinal$MCresults), fixed = FALSE
  )

  year2$MpEstEmRemsDefor <- ValueWithUncertainty(
    Value = UC_EmRems$year2$EmEstDFTotalFinal$value[1],
    LowerCI = UC_EmRems$year2$EmEstDFTotalFinal$value[2],
    UpperCI = UC_EmRems$year2$EmEstDFTotalFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year2$EmEstDFTotalFinal$MCresults), fixed = FALSE
  )

  MpEstEmRemsDeforValue <- CalcMpEstEmRemsDefor(
    ValueWithUncertaintyValue(year1$MpEstEmRemsDefor),
    ValueWithUncertaintyValue(year2$MpEstEmRemsDefor)
  )

  CalcMpEstEmRemsDeforArgs <- function() {
    return(list(year1$MpEstEmRemsDefor, year2$MpEstEmRemsDefor))
  }

  result$MpEstEmRemsDeforFinal <- CalcMonteCarlo("MpEstEmRemsDefor", MpEstEmRemsDeforValue, CalcMpEstEmRemsDefor, CalcMpEstEmRemsDeforArgs)
  MpEstEmRemsDefor <- ValueWithUncertainty(
    Value = result$MpEstEmRemsDeforFinal$value[1],
    LowerCI = result$MpEstEmRemsDeforFinal$value[2],
    UpperCI = result$MpEstEmRemsDeforFinal$value[3],
    model = create_vwuSampled(result$MpEstEmRemsDeforFinal$MCresults), fixed = FALSE
  )

  year1$MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = UC_EmRems$year1$ECFinal$value[1],
    LowerCI = UC_EmRems$year1$ECFinal$value[2],
    UpperCI = UC_EmRems$year1$ECFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year1$ECFinal$MCresults), fixed = FALSE
  )

  year2$MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = UC_EmRems$year2$ECFinal$value[1],
    LowerCI = UC_EmRems$year2$ECFinal$value[2],
    UpperCI = UC_EmRems$year2$ECFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year2$ECFinal$MCresults), fixed = FALSE
  )

  MpEstEmRemsEnhValue <- CalcMpEstEmRemsEnh(
    ValueWithUncertaintyValue(year1$MpEstEmRemsEnh),
    ValueWithUncertaintyValue(year2$MpEstEmRemsEnh)
  )

  CalcMpEstEmRemsEnhArgs <- function() {
    return(list(year1$MpEstEmRemsEnh, year2$MpEstEmRemsEnh))
  }

  result$MpEstEmRemsEnhFinal <- CalcMonteCarlo("MpEstEmRemsEnh", MpEstEmRemsEnhValue, CalcMpEstEmRemsEnh, CalcMpEstEmRemsEnhArgs)
  MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = result$MpEstEmRemsEnhFinal$value[1],
    LowerCI = result$MpEstEmRemsEnhFinal$value[2],
    UpperCI = result$MpEstEmRemsEnhFinal$value[3],
    model = create_vwuSampled(result$MpEstEmRemsEnhFinal$MCresults), fixed = FALSE
  )

  MpEstERsDefEnhValue <- CalcMpEstERsDefEnh(
    ValueWithUncertaintyValue(UC$FRLDeforestation),
    ValueWithUncertaintyValue(UC$FRLRemovalsBySinks),
    ValueWithUncertaintyValue(MpEstEmRemsDefor),
    ValueWithUncertaintyValue(MpEstEmRemsEnh)
  )

  CalcMpEstERsDefEnhArgs <- function() {
    return(list(UC$FRLDeforestation, UC$FRLRemovalsBySinks, MpEstEmRemsDefor, MpEstEmRemsEnh))
  }

  result$MpEstERsDefEnhFinal <- CalcMonteCarlo("MpEstERsDefEnh", MpEstERsDefEnhValue, CalcMpEstERsDefEnh, CalcMpEstERsDefEnhArgs)
  # FRLDef + FRLEnh + EmRemsDefor + EmRemsEnh



  # ERs not including FDeg
  result$MpEstERsDefEnh <- calcUCModel(
    result$MpEstERsDefEnhFinal$value[2],
    median(result$MpEstERsDefEnhFinal$MCresults),
    result$MpEstERsDefEnhFinal$value[3]
  )

  ##################################
  # monitoring Period ERs Forest Deg

  year1$NetFDEm <- ValueWithUncertainty(
    Value = UC_EmRems$year1$FDFinal$value[1],
    LowerCI = UC_EmRems$year1$FDFinal$value[2],
    UpperCI = UC_EmRems$year1$FDFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year1$FDFinal$MCresults), fixed = FALSE
  )
  names(year1$NetFDEm) <- c("NetFDEm")

  year2$NetFDEm <- ValueWithUncertainty(
    Value = UC_EmRems$year2$FDFinal$value[1],
    LowerCI = UC_EmRems$year2$FDFinal$value[2],
    UpperCI = UC_EmRems$year2$FDFinal$value[3],
    model = create_vwuSampled(UC_EmRems$year2$FDFinal$MCresults), fixed = FALSE
  )
  names(year2$NetFDEm) <- c("NetFDEm")

  NetFDEmValue <- CalcMpEstEmRemsFDeg(
    ValueWithUncertaintyValue(year1$NetFDEm),
    ValueWithUncertaintyValue(year2$NetFDEm)
  )

  CalcMpEstEmRemsFDegArgs <- function() {
    return(list(year1$NetFDEm, year2$NetFDEm))
  }

  # TODO: rename to ER$MpEstEmRemsFDeg
  result$NetFDEmissionsFinal <- CalcMonteCarlo("NetFDEmissions", NetFDEmValue, CalcMpEstEmRemsFDeg, CalcMpEstEmRemsFDegArgs)
  NetFDEmissions <- ValueWithUncertainty(
    Value = result$NetFDEmissionsFinal$value[1],
    LowerCI = result$NetFDEmissionsFinal$value[2],
    UpperCI = result$NetFDEmissionsFinal$value[3],
    model = create_vwuSampled(result$NetFDEmissionsFinal$MCresults), fixed = FALSE
  )
  CalcFDeg <- function(FRLFDeg, EmRemsFDeg) {
    return(CalcMpEstERsFDeg(CalcMpEstFRL(FRLFDeg), EmRemsFDeg))
  }
  CalcFDegArgs <- function() {
    return(list(UC$FRLForestDegradation, NetFDEmissions))
  }

  MpEstERsFDegValue <- CalcFDeg(
    ValueWithUncertaintyValue(UC$FRLForestDegradation),
    ValueWithUncertaintyValue(NetFDEmissions)
  )

  result$MpEstERsFDegFinal <- CalcMonteCarlo("MpEstERsFDeg", MpEstERsFDegValue, CalcFDeg, CalcFDegArgs)
  # FRLFDeg + EmRemsFDeg

  # forestDeg ERs
  result$MpEstERsFDeg <- calcUCModel(
    result$MpEstERsFDegFinal$value[2],
    median(result$MpEstERsFDegFinal$MCresults),
    result$MpEstERsFDegFinal$value[3]
  )

  return(result)
}
