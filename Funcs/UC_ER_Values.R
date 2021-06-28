

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
  CalcEstEmDeforUpArgs <- function() {
    return(list(UC_MV$DeforAreaUp, UC$EFDeforUp))
  }

  McEmDeforUp <- CalcMonteCarlo("EstEmDeforUp", EmRems$EstEmDeforUp, CalcEstEmDefor, CalcEstEmDeforUpArgs)
  if (debug_er) McEmDeforUp

  local$EstEmDeforUp <- ValueWithUncertainty(
    Value = EmRems$EstEmDeforUp,
    LowerCI = McEmDeforUp$value[2],
    UpperCI = McEmDeforUp$value[3],
    model = create_vwuSampled(McEmDeforUp$MCresults), fixed = FALSE
  )
  names(local$EstEmDeforUp) <- c("EstEmDeforUp")

  CalcEstEmDeforLowArgs <- function() {
    return(list(UC_MV$DeforAreaLow, UC$EFDeforLow))
  }


  McEmDeforLow <- CalcMonteCarlo("EstEmDeforLow", EmRems$EstEmDeforLow, CalcEstEmDefor, CalcEstEmDeforLowArgs)
  if (debug_er) McEmDeforLow
  local$EstEmDeforLow <- ValueWithUncertainty(
    Value = EmRems$EstEmDeforLow,
    LowerCI = McEmDeforLow$value[2],
    UpperCI = McEmDeforLow$value[3],
    model = create_vwuSampled(McEmDeforLow$MCresults), fixed = FALSE
  )
  names(local$EstEmDeforLow) <- c("EstEmDeforLow")

  # Total emissions from Deforestation (tCO2e)
  CalcGrossEmDeforArgs <- function() {
    return(list(local$EstEmDeforUp, local$EstEmDeforLow))
  }


  ## MGG - UC
  # Final Estimate of emissions with UCI and LCI
  result$McGrossEmDefor <- CalcMonteCarlo("GrossEmDefor", EmRems$GrossEmDefor, CalcGrossEmDefor, CalcGrossEmDeforArgs)
  if (debug_er) result$McGrossEmDefor
  local$GrossEmDefor <- ValueWithUncertainty(
    Value = EmRems$GrossEmDefor,
    LowerCI = result$McGrossEmDefor$value[2],
    UpperCI = result$McGrossEmDefor$value[3],
    model = create_vwuSampled(result$McGrossEmDefor$MCresults), fixed = FALSE
  )
  names(local$GrossEmDefor) <- c("GrossEmDefor")

  #################### 2. Forest Degradation (FD) ########

  ## 2.1 Felling in Natural Forest (Emissions and Removals)

  ## Yearly EMISSIONS from felling in natural forest (tCO2e)

  # Estimate of CO2e emissions from felling
  # Uncertainty associated with the TEF, create arguments for MC simuation i.e. random value for TEF,
  # Felling Volume uncertainty not included in MC, just use actual volume

 CalcEstEmFellArgs <- function() {
    return(list(UC$TEF, MV$FDegFellVol))
  }


  ## MGG - UC
  result$McEstEmFell <- CalcMonteCarlo("EstEmFell", EmRems$EstEmFell,CalcEstEmFell,CalcEstEmFellArgs)
  if (debug_er) result$McEstEmFell
  local$EstEmFell <- ValueWithUncertainty(
    Value = EmRems$EstEmFell,
    LowerCI = result$McEstEmFell$value[2],
    UpperCI = result$McEstEmFell$value[3],
    model = create_vwuSampled(result$McEstEmFell$MCresults), fixed = FALSE
  )
  names(local$EstEmFell) <- c("EstEmFell")

  ## Yearly REMOVALS from felling in natural forest (tCO2e)
  #  no need to (* delta t) as delta t = 1 for 1 year
  # Estimate of CO2e removals from felling
  # Uncertainty associated with the with MAIC and Area Felled
  # Calculate the arguments
  CalcEstRemFellArgs <- function() {
    return(list(UC_MV$FDegFellArea, UC$MAICFell))
  }


  ## MGG - UC
  # Final Estimate for Removals with UCI and LCI
  result$McEstRemFell <- CalcMonteCarlo("EstRemFell", EmRems$EstRemFell, CalcEstRemFell, CalcEstRemFellArgs)
  if (debug_er) result$McEstRemFell
  local$EstRemFell <- ValueWithUncertainty(
    Value = EmRems$EstRemFell,
    LowerCI = result$McEstRemFell$value[2],
    UpperCI = result$McEstRemFell$value[3],
    model = create_vwuSampled(result$McEstRemFell$MCresults), fixed = FALSE
  )
  names(local$EstRemFell) <- c("EstRemFell")

  CalcNetEmRemsFellArgs <- function() {
    return(list(local$EstEmFell, local$EstRemFell))
  }

  # Final Estimate of emissions with UCI and LCI
  McEstEmFell <- CalcMonteCarlo("EstEmFell", EmRems$NetEmRemsFell, CalcNetEmRemsFell, CalcNetEmRemsFellArgs)
  if (debug_er) McEstEmFell

  ##***********************************************************
  ## 2.2 Biomass Burning

  # Uncertainty analysis For Biomass Burning
  # Random inputs for the MC simulation
  CalcEstEmFireArgs <- function() {
    return(list(
      MV$FDegBurnData$age_yrs, UC$MAIBsw, UC$RootToShootDryLandSmall, MV$FDegBurnData$area_ha
    ))
  }

  ## MGG - UC
  # Yearly Emissions from biomass burning  (tCO2e)
  result$McEstEmFire <- CalcMonteCarlo("EstEmFire", EmRems$EstEmFire, CalcEstEmFire, CalcEstEmFireArgs)
  local$EstEmFire <- ValueWithUncertainty(
    Value = EmRems$EstEmFire,
    LowerCI = result$McEstEmFire$value[2],
    UpperCI = result$McEstEmFire$value[3],
    model = create_vwuSampled(result$McEstEmFire$MCresults), fixed = FALSE
  )
  names(local$EstEmFire) <- c("EstEmFire")
  #*********************************************************
  # 2.3 Fuelwood - Excluded from ER calculations
  # **********************************************************

  #*******************************************************************
  ############## 3. Enhancement ##########
  # 3.1 Afforestation

  # Yearly Removals from Afforestation  (tCO2e)
  # Uncertainty with the estimated total MAICAGBar and Root To Shoot Tropical Rain Ratio
  # Calculate arguments
  CalcEstRemAReforArgs <- function() {
    return(list(UC_MV$AReforArea, UC$MAIVar, UC$BiomassConvExpansionARefor, UC$RootToShootTropRain))
  }


  ## MGG - UC
  result$McEstRemARefor <- CalcMonteCarlo("EstRemARefor", EmRems$EstRemARefor, CalcGrossRemARefor, CalcEstRemAReforArgs)
  if (debug_er) result$McEstRemARefor
  local$EstRemARefor <- ValueWithUncertainty(
    Value = EmRems$EstRemARefor,
    LowerCI = result$McEstRemARefor$value[2],
    UpperCI = result$McEstRemARefor$value[3],
    model = create_vwuSampled(result$McEstRemARefor$MCresults), fixed = FALSE
  )
  names(local$EstRemARefor) <- c("EstRemARefor")
  # 3.2 Forest Plantations

  # Emissions Hardwood plantations ####
  # Uncertainty assessment (Monte Carlo simulations) related to Biomass Conversion and Expansion
  # Factor, Root To shoot Tropical Rain
  CalcEmFPHWArgs <- function() {
    return(list(MV$FPlnVolHarvHwd, UC$BiomassConvExpansionHW, UC$RootToShootTropRain))
  }

  # Final Estimate for Emissions with UCI and LCI
  McEstEmFPlnHwd <- CalcMonteCarlo("EstEmFPlnHwd", EmRems$EstEmFPlnHwd, CalcEstEmFPlnHwd, CalcEmFPHWArgs)
  if (debug_er) McEstEmFPlnHwd
  local$EstEmFPlnHwd <- ValueWithUncertainty(
    Value = EmRems$EstEmFPlnHwd,
    LowerCI = McEstEmFPlnHwd$value[2],
    UpperCI = McEstEmFPlnHwd$value[3],
    model = create_vwuSampled(McEstEmFPlnHwd$MCresults), fixed = FALSE
  )
  names(local$EstEmFPlnHwd) <- c("EstEmFPlnHwd")
  # Emissions Softwood plantations ####
  CalcEmFPSWArgs <- function() {
    return(list(MV$FPlnVolHarvSwd, UC$Recovery, UC$WoodDensity, UC$RootToShootDryLandBig))
  }

  # Final Estimate for SW Emissions with UCI and LCI
  McEstEmFPlnSwd <- CalcMonteCarlo("EstEmFPlnSwd", EmRems$EstEmFPlnSwd, CalcEstEmFPlnSwd, CalcEmFPSWArgs)
  if (debug_er) McEstEmFPlnSwd
  local$EstEmFPlnSwd <- ValueWithUncertainty(
    Value = EmRems$EstEmFPlnSwd,
    LowerCI = McEstEmFPlnSwd$value[2],
    UpperCI = McEstEmFPlnSwd$value[3],
    model = create_vwuSampled(McEstEmFPlnSwd$MCresults), fixed = FALSE
  )
  names(local$EstEmFPlnSwd) <- c("EstEmFPlnSwd")


  # Removals Hardwood plantations ####
  # Uncertainty assessment (removals) of MAIV, Biomass Conversion and Expansion Factor,
  # Root To Shoot Tropical Rain, MAIC, Average annual area of forest that just grows
  CalcRemFPHWArgs <- function() {
    return(list(MV$FPlnAreaJustGrowsHwd, MV$FPlnAreaPlantHwd, MV$FPlnAreaHarvHwd, UC$MAIVhw, UC$BiomassConvExpansionIncHW, UC$RootToShootTropRain))
  }


  # Final Estimate for HW Removals with UCI and LCI
  McEstRemFPlnHwd <- CalcMonteCarlo("EstRemFPlnHwd", EmRems$EstRemFPlnHwd, CalcEstRemFPlnHwd, CalcRemFPHWArgs)
  if (debug_er) McEstRemFPlnHwd
  local$EstRemFPlnHwd <- ValueWithUncertainty(
    Value = EmRems$EstRemFPlnHwd,
    LowerCI = McEstRemFPlnHwd$value[2],
    UpperCI = McEstRemFPlnHwd$value[3],
    model = create_vwuSampled(McEstRemFPlnHwd$MCresults), fixed = FALSE
  )
  names(local$EstRemFPlnHwd) <- c("EstRemFPlnHwd")
  # Estimate of softwood removals for yr (tCO2e) ####
  CalcRemFPSWArgs <- function() {
    return(list(UC$MAIBsw, MV$FPlnAreaJustGrowsSwd, MV$FPlnAreaPlantSwd, MV$FPlnAreaHarvSwd))
  }

  # Final Estimate for SW Removals with UCI and LCI
  McEstRemFPlnSwd <- CalcMonteCarlo("EstRemFPlnSwd", EmRems$EstRemFPlnSwd, CalcEstRemFPlnSwd, CalcRemFPSWArgs)
  if (debug_er) McEstRemFPlnSwd
  local$EstRemFPlnSwd <- ValueWithUncertainty(
    Value = EmRems$EstRemFPlnSwd,
    LowerCI = McEstRemFPlnSwd$value[2],
    UpperCI = McEstRemFPlnSwd$value[3],
    model = create_vwuSampled(McEstRemFPlnSwd$MCresults), fixed = FALSE
  )
  names(local$EstRemFPlnSwd) <- c("EstRemFPlnSwd")


  # **************************************************************
  # Gross emissions Forest Plantations (Hard- and Softwood)
  CalcGrossEmFPlnArgs <- function() {
    return(list(local$EstEmFPlnHwd, local$EstEmFPlnSwd))
  }

  ## MGG - UC
  # Gross emissions Forest Plantations (Hard- and Softwood)
  result$McGrossEmFPln <- CalcMonteCarlo("GrossEmFPln", EmRems$GrossEmFPln, CalcGrossEmFPln, CalcGrossEmFPlnArgs)
  if (debug_er) result$McGrossEmFPln
  local$GrossEmFPln  <- ValueWithUncertainty(
    Value = EmRems$GrossEmFPln ,
    LowerCI = result$McGrossEmFPln$value[2],
    UpperCI = result$McGrossEmFPln$value[3],
    model = create_vwuSampled(result$McGrossEmFPln$MCresults), fixed = FALSE
  )
  names(local$GrossEmFPln ) <- c("GrossEmFPln")


  # Gross removals Forest Plantations (Hard- and Softwood)
  CalcGrossRemFPlnArgs <- function() {
    return(list(local$EstRemFPlnHwd, local$EstRemFPlnSwd))
  }

  ## MGG - UC
  # Gross removals Forest Plantations (Hard- and Softwood)
  result$McGrossRemFPln <- CalcMonteCarlo("GrossRemFPln", EmRems$GrossRemFPln,  CalcGrossRemFPln, CalcGrossRemFPlnArgs)
  if (debug_er) result$McGrossRemFPln
  local$GrossRemFPln <- ValueWithUncertainty(
    Value = EmRems$GrossRemFPln,
    LowerCI = result$McGrossRemFPln$value[2],
    UpperCI = result$McGrossRemFPln$value[3],
    model = create_vwuSampled(result$McGrossRemFPln$MCresults), fixed = FALSE
  )
  names(local$GrossRemFPln) <- c("GrossRemFPln")

  # Net Emissions from Forest Plantations (Hard- and Softwood)
  CalcNetEmRemsFPlnArgs <- function() {
    return(list(local$GrossEmFPln, local$GrossRemFPln))
  }

  # Net Emissions from Forest Plantations (Hard- and Softwood)
  McNetEmRemsFPln <- CalcMonteCarlo("NetEmRemsFPln", EmRems$NetEmRemsFPln, CalcNetEmRemsFPln, CalcNetEmRemsFPlnArgs)
  local$NetEmRemsFPln <- ValueWithUncertainty(
    Value = EmRems$NetEmRemsFPln,
    LowerCI = McNetEmRemsFPln$value[2],
    UpperCI = McNetEmRemsFPln$value[3],
    model = create_vwuSampled(McNetEmRemsFPln$MCresults), fixed = FALSE
  )
  names(local$NetEmRemsFPln) <- c("NetEmRemsFPln")

  NetEmRemsFPlnResults <- data.frame(
    stratum = c("Emissions", "Removals", "Total"),
    # Average annual emissions from Forest Degradation
    Estimate = c(result$McGrossEmFPln$value[1], result$McGrossRemFPln$value[1], McNetEmRemsFPln$value[1]),
    # Lower confidence interval bound
    LCI = c(result$McGrossEmFPln$value[2], result$McGrossRemFPln$value[2], McNetEmRemsFPln$value[2]),
    # Upper confidence interval bound
    UCI = c(result$McGrossEmFPln$value[3], result$McGrossRemFPln$value[3], McNetEmRemsFPln$value[3])
  )
  if (debug_er) NetEmRemsFPlnResults


  #*************************************************************
  # 4. Final table of results

  # Gross Emissions Total
  CalcGrossEmArgs <- function() {
    return(list(local$GrossEmDefor, local$EstEmFell, local$EstEmFire, local$GrossEmFPln ))
  }

  ## MGG - UC
  result$McGrossEm <- CalcMonteCarlo("GrossEm", EmRems$GrossEm, CalcGrossEm, CalcGrossEmArgs)
  local$GrossEm <- ValueWithUncertainty(
    Value = EmRems$GrossEm,
    LowerCI = result$McGrossEm$value[2],
    UpperCI = result$McGrossEm$value[3],
    model = create_vwuSampled(result$McGrossEm$MCresults), fixed = FALSE
  )
  names(local$GrossEm) <- c("GrossEm")


  # Gross Removals Total
  CalcGrossRemArgs <- function() {
    return(list(local$EstRemFell, local$EstRemARefor, local$GrossRemFPln))
  }

  ## MGG - UC
  result$McGrossRem <- CalcMonteCarlo("GrossRem", EmRems$GrossRem, CalcGrossRem, CalcGrossRemArgs)
  local$GrossRem <- ValueWithUncertainty(
    Value = EmRems$GrossRem,
    LowerCI = result$McGrossRem$value[2],
    UpperCI = result$McGrossRem$value[3],
    model = create_vwuSampled(result$McGrossRem$MCresults), fixed = FALSE
  )
  names(local$GrossRem) <- c("GrossRem")

  # Forest Degradation Total
  CalcEstEmRemsFDegArgs <- function() {
    return(list(local$EstEmFell, local$EstRemFell, local$EstEmFire))
  }

  ## MGG - UC
  result$McFDeg <- CalcMonteCarlo("McFDeg", EmRems$EstEmRemsFDeg, CalcEstEmRemsFDeg, CalcEstEmRemsFDegArgs)


  # Enhancement Total
  CalcEstEmRemsEnhArgs <- function() {
    return(list(local$NetEmRemsFPln, local$EstRemARefor))
  }

  ## MGG - UC
  result$McEnh <- CalcMonteCarlo("McEnh", EmRems$EstEmRemsEnh, CalcEstEmRemsEnh, CalcEstEmRemsEnhArgs)


  # Net Emissions Total
  CalcNetEmRemsArgs <- function() {
    return(list(local$GrossEm, local$GrossRem))
  }

  ## MGG - UC
  result$McNetEmRems <- CalcMonteCarlo("NetEmissions", EmRems$NetEmRems, CalcNetEmRems, CalcNetEmRemsArgs)

  return(result)
}



createUC_ERValues <- function(UC_EmRems, UC_MV, UC, MRparams) {
  result <- list()

  year1 <- list()
  year2 <- list()



  #############################################
  # Monitoring Period Net EmRems all activities
  year1$NetEm <- ValueWithUncertainty(
    Value = UC_EmRems$year1$McNetEmRems$value[1],
    LowerCI = UC_EmRems$year1$McNetEmRems$value[2],
    UpperCI = UC_EmRems$year1$McNetEmRems$value[3],
    model = create_vwuSampled(UC_EmRems$year1$McNetEmRems$MCresults), fixed = FALSE
  )

  year2$NetEm <- ValueWithUncertainty(
    Value = UC_EmRems$year2$McNetEmRems$value[1],
    LowerCI = UC_EmRems$year2$McNetEmRems$value[2],
    UpperCI = UC_EmRems$year2$McNetEmRems$value[3],
    model = create_vwuSampled(UC_EmRems$year2$McNetEmRems$MCresults), fixed = FALSE
  )

  NetEmValue <- CalcMpNetEmRems(
    ValueWithUncertaintyValue(year1$NetEm),
    ValueWithUncertaintyValue(year2$NetEm)
  )

  CalcMpNetEmRemsArgs <- function() {
    return(list(year1$NetEm, year2$NetEm))
  }

  # TODO: rename to ER$MpNetEmRems
  result$McNetEmRems <- CalcMonteCarlo("NetEmissions", NetEmValue, CalcMpNetEmRems, CalcMpNetEmRemsArgs)

  #####################################################################
  # Monitoring Period ERs Defor and Enhancements (excluding Forest Deg)

  year1$MpGrossEmDefor <- ValueWithUncertainty(
    Value = UC_EmRems$year1$McGrossEmDefor$value[1],
    LowerCI = UC_EmRems$year1$McGrossEmDefor$value[2],
    UpperCI = UC_EmRems$year1$McGrossEmDefor$value[3],
    model = create_vwuSampled(UC_EmRems$year1$McGrossEmDefor$MCresults), fixed = FALSE
  )

  year2$MpGrossEmDefor <- ValueWithUncertainty(
    Value = UC_EmRems$year2$McGrossEmDefor$value[1],
    LowerCI = UC_EmRems$year2$McGrossEmDefor$value[2],
    UpperCI = UC_EmRems$year2$McGrossEmDefor$value[3],
    model = create_vwuSampled(UC_EmRems$year2$McGrossEmDefor$MCresults), fixed = FALSE
  )

  MpGrossEmDeforValue <- CalcMpGrossEmDefor(
    ValueWithUncertaintyValue(year1$MpGrossEmDefor),
    ValueWithUncertaintyValue(year2$MpGrossEmDefor)
  )

  CalcMpGrossEmDeforArgs <- function() {
    return(list(year1$MpGrossEmDefor, year2$MpGrossEmDefor))
  }

  result$McMpGrossEmDefor <- CalcMonteCarlo("MpGrossEmDefor", MpGrossEmDeforValue, CalcMpGrossEmDefor, CalcMpGrossEmDeforArgs)
  MpGrossEmDefor <- ValueWithUncertainty(
    Value = result$McMpGrossEmDefor$value[1],
    LowerCI = result$McMpGrossEmDefor$value[2],
    UpperCI = result$McMpGrossEmDefor$value[3],
    model = create_vwuSampled(result$McMpGrossEmDefor$MCresults), fixed = FALSE
  )

  year1$MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = UC_EmRems$year1$McEnh$value[1],
    LowerCI = UC_EmRems$year1$McEnh$value[2],
    UpperCI = UC_EmRems$year1$McEnh$value[3],
    model = create_vwuSampled(UC_EmRems$year1$McEnh$MCresults), fixed = FALSE
  )

  year2$MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = UC_EmRems$year2$McEnh$value[1],
    LowerCI = UC_EmRems$year2$McEnh$value[2],
    UpperCI = UC_EmRems$year2$McEnh$value[3],
    model = create_vwuSampled(UC_EmRems$year2$McEnh$MCresults), fixed = FALSE
  )

  MpEstEmRemsEnhValue <- CalcMpEstEmRemsEnh(
    ValueWithUncertaintyValue(year1$MpEstEmRemsEnh),
    ValueWithUncertaintyValue(year2$MpEstEmRemsEnh)
  )

  CalcMpEstEmRemsEnhArgs <- function() {
    return(list(year1$MpEstEmRemsEnh, year2$MpEstEmRemsEnh))
  }

  result$McMpEstEmRemsEnh <- CalcMonteCarlo("MpEstEmRemsEnh", MpEstEmRemsEnhValue, CalcMpEstEmRemsEnh, CalcMpEstEmRemsEnhArgs)
  MpEstEmRemsEnh <- ValueWithUncertainty(
    Value = result$McMpEstEmRemsEnh$value[1],
    LowerCI = result$McMpEstEmRemsEnh$value[2],
    UpperCI = result$McMpEstEmRemsEnh$value[3],
    model = create_vwuSampled(result$McMpEstEmRemsEnh$MCresults), fixed = FALSE
  )

  MpEstERsDefEnhValue <- CalcMpEstERsDefEnh(
    ValueWithUncertaintyValue(UC$FRLDeforestation),
    ValueWithUncertaintyValue(UC$FRLRemovalsBySinks),
    ValueWithUncertaintyValue(MpGrossEmDefor),
    ValueWithUncertaintyValue(MpEstEmRemsEnh)
  )

  CalcMpEstERsDefEnhArgs <- function() {
    return(list(UC$FRLDeforestation, UC$FRLRemovalsBySinks, MpGrossEmDefor, MpEstEmRemsEnh))
  }

  result$McMpEstERsDefEnh <- CalcMonteCarlo("MpEstERsDefEnh", MpEstERsDefEnhValue, CalcMpEstERsDefEnh, CalcMpEstERsDefEnhArgs)
  # FRLDef + FRLEnh + EmRemsDefor + EmRemsEnh



  # ERs not including FDeg
  result$MpEstERsDefEnh <- calcUCModel(
    result$McMpEstERsDefEnh$value[2],
    median(result$McMpEstERsDefEnh$MCresults),
    result$McMpEstERsDefEnh$value[3]
  )

  ##################################
  # monitoring Period ERs Forest Deg

  year1$NetEmFDeg <- ValueWithUncertainty(
    Value = UC_EmRems$year1$McFDeg$value[1],
    LowerCI = UC_EmRems$year1$McFDeg$value[2],
    UpperCI = UC_EmRems$year1$McFDeg$value[3],
    model = create_vwuSampled(UC_EmRems$year1$McFDeg$MCresults), fixed = FALSE
  )
  names(year1$NetEmFDeg) <- c("NetEmFDeg")

  year2$NetEmFDeg <- ValueWithUncertainty(
    Value = UC_EmRems$year2$McFDeg$value[1],
    LowerCI = UC_EmRems$year2$McFDeg$value[2],
    UpperCI = UC_EmRems$year2$McFDeg$value[3],
    model = create_vwuSampled(UC_EmRems$year2$McFDeg$MCresults), fixed = FALSE
  )
  names(year2$NetEmFDeg) <- c("NetEmFDeg")

  NetEmFDegValue <- CalcMpEstEmRemsFDeg(
    ValueWithUncertaintyValue(year1$NetEmFDeg),
    ValueWithUncertaintyValue(year2$NetEmFDeg)
  )

  CalcMpEstEmRemsFDegArgs <- function() {
    return(list(year1$NetEmFDeg, year2$NetEmFDeg))
  }


  result$McNetEmRemsFDeg <- CalcMonteCarlo("NetEmRemsFDeg", NetEmFDegValue, CalcMpEstEmRemsFDeg, CalcMpEstEmRemsFDegArgs)
  NetEmRemsFDeg <- ValueWithUncertainty(
    Value = result$McNetEmRemsFDeg$value[1],
    LowerCI = result$McNetEmRemsFDeg$value[2],
    UpperCI = result$McNetEmRemsFDeg$value[3],
    model = create_vwuSampled(result$McNetEmRemsFDeg$MCresults), fixed = FALSE
  )
  CalcFDeg <- function(FRLFDeg, EmRemsFDeg) {
    return(CalcMpEstERsFDeg(CalcMpEstFRL(FRLFDeg), EmRemsFDeg))
  }
  CalcFDegArgs <- function() {
    return(list(UC$FRLForestDegradation, NetEmRemsFDeg))
  }

  MpEstERsFDegValue <- CalcFDeg(
    ValueWithUncertaintyValue(UC$FRLForestDegradation),
    ValueWithUncertaintyValue(NetEmRemsFDeg)
  )

  result$McMpEstERsFDeg <- CalcMonteCarlo("MpEstERsFDeg", MpEstERsFDegValue, CalcFDeg, CalcFDegArgs)
  # FRLFDeg + EmRemsFDeg

  # forestDeg ERs
  result$MpEstERsFDeg <- calcUCModel(
    result$McMpEstERsFDeg$value[2],
    median(result$McMpEstERsFDeg$MCresults),
    result$McMpEstERsFDeg$value[3]
  )

  return(result)
}
