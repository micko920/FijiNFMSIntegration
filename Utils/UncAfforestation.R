# Removals and Uncertainty from Afforestation - upland and lowland
source(file = "../Constants.R")
source(file = "../Fiji_Constants_from_Reference_Level.R")

options(digits = 8)

# Uncertainty attached to the estimated total carbon increment for AReforefor
VMAICar <- rtriangle(
  # Random mean annual carbon increment
  n = MCRuns,
  theta = MAICAGBar,
  lower = MAICAGBar - MAICAGBar * errMAICAGBar,
  upper = MAICAGBar + MAICAGBar * errMAICAGBar
) *
  # Uncertainty attached to root-to-shoot ratio (tropical rainforest)
  (1 + rtriangle(n = MCRuns,
                 theta = RootToShootTropRain,
                 lower = RootToShootTropRain - RootToShootTropRain * errRootToShootTropRain ,
                 upper = RootToShootTropRain + RootToShootTropRain * errRootToShootTropRain ))

VMAICar


# Total Area of Afforestation
AreaTotal <- AReforeforAreaUp + AReforeforAreaLow 
# Carbon gains on areas afforested/reforested in one year
CarbonT <- AreaTotal * MAICar


# Uncertainty analysis
# Create vector
UncAnaRemAReforefor <- vector()

UncAnaRemAReforefor
# MC simulation
for(i in 1:MCRuns){ # i <- 1
  UncAnaRemAReforefor[i] <- (  
      AreaTotal *  # Average annual area of AReforefor
      VMAICar[i])                  # Random increment
}
UncAnaRemAReforefor[1:10]


# Average annual removals from afforestation/reforestation (AReforefor)
EstimateRemAReforefor <- CarbonT * CarbonToCO2eRatio                              # Estimate
LCIRemAr <- quantile(UncAnaRemAReforefor * CarbonToCO2eRatio, probs = QLCI) # Lower confidence limit
UCIRemAReforefor <- quantile(UncAnaRemAReforefor * CarbonToCO2eRatio, probs = QUCI) # Upper confidence limit
MCERemAReforefor <- UncAnaRemAReforefor * CarbonToCO2eRatio                            # MC estimates

# missing (-1)
EstimateRemAReforefor 
LCIRemAr
UCIRemAReforefor 

############################################################################

# MAICar  <- MAICAGBar * (1 + RootToShootTropRain) which is affected by final value for MAICAGBar

CalcEstRemAReforefor <- function(
  AreaUpland,
  AreaLowland,
  MAICar, 
  CarbonToCO2eRatio
){
  # Total Area of Afforestation over the period
  AreaTotal <- AreaUpland + AreaLowland 
  # C gains from afforested area over the yr 
  Carbon <- AreaTotal * MAICar * (-1) 
  # Removals from afforestation/reforestation for the year
  CO2e<-ConvCarbonToCO2e(Carbon, CarbonToCO2eRatio)
}


# Yearly Removals from Afforestation  (tCO2e)
RemAReforefor <- CalcEstRemAReforefor(AReforeforAreaLow, AReforeforAreaUp, MAICar, CarbonToCO2eRatio)
RemAReforefor
