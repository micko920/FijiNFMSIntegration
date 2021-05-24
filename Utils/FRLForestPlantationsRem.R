# FRL functions to match forest plantation removals for HW and SW
# Required source files
# load(file = "./Data/fiji_frl_input.RData")
# Ty <- 2006:2016 # period of study
# Tl <- length(Ty)
# DeltaT <- 2016 - Ty + 0.5 # 10.5, 9.5, 8.5, .....0.5
# RevDeltaT <- rev(DeltaT) # 0.5, 1.5, ....10.5

# Hardwood Plantations ####

# FRLAreaStockHW <- 56950.45 # Existing hardwood stock at start of 2005 (ha)
# FRLAreaPlantHW <- hw_ahp[, 3] # Areas planted during period
# FRLAreaHarvestHW <- hw_ahp[, 2] # Areas harvested during period

# FRLAreaJustGrows <- FRLAreaStockHW - sum(FRLAreaHarvestHW)

CalcFRLRemForPlantHW <- function(AreaJustGrows,
                                 AreaPlanted,
                                 AreaHarvest,
                                 MAIChw, # Mean annual increment C for volume m^3
                                 DeltaT,
                                 RevDeltaT,
                                 CarbonConvRatio) { # i) Area of forest that just grows (existing stock, neither planted nor harvested)
  Rem1 <- AreaJustGrows * MAIChw
  # ii) Mean annual C removals on planted areas
  Rem2 <- mean(AreaPlanted * DeltaT * MAIChw)
  # iii) Carbon accumulation on areas that were harvested in year t
  Rem3 <- mean(AreaHarvest * RevDeltaT * MAIChw)
  # Sum of Carbon from 3 areas
  TotalAnnualCarbon <- Rem1 + Rem2 + Rem3
  TotalCO2e <- ConvCarbonToCO2e(TotalAnnualCarbon) * (-1)
}


# FRLFPRemHW <- CalcFRLRemForPlantHW(
#   FRLAreaJustGrows, hw$AreaPlanted, hw$AreaHarvested, MAIChw,
#   DeltaT, RevDeltaT, CarbonToCO2eRatio
# )


# Softwood ####



CalcFRLRemForPlantSW <- function(MAICsw, # Mean annual biomass increment in Softwood Plantations
                                 AreaJustGrows, # Initial area of forest at start of period
                                 AreaPlanted, # Area planted during the period
                                 AreaHarvested,
                                 DeltaT, # 10.5, 9.5, 8.5, ......0.5
                                 RevDeltaT # 0.5, 1.5, 2.5 .....10.5
) {
  # Calc mean annual increment C for volume m^3
  Rem1 <- AreaJustGrows * MAICsw
  # ii) Carbon from area planted
  Rem2 <- mean(AreaPlanted * DeltaT * MAICsw)
  # iii) Carbon accumulation on areas that were harvested in year t
  Rem3 <- mean(AreaHarvested * RevDeltaT * MAICsw)
  # Total Carbon
  TotalAnnualCarbon <- Rem1 + Rem2 + Rem3
  TotalCO2e <- ConvCarbonToCO2e(TotalAnnualCarbon) * (-1)
}

# TotalCO2e <- TotalAnnualCarbon * CarbonToCO2eRatio * (-1)


# FRLFPRemSW <- CalcFRLRemForPlantSW(
#   MAICsw, FRLAreaJustGrowsSW, sw$AreaPlanted, sw$AreaHarvested,
#   DeltaT, RevDeltaT
# )
