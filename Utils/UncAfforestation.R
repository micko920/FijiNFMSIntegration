# Removals and Uncertainty from Afforestation - upland and lowland
source(file = "../Constants.R")
source(file = "../Fiji_Constants_from_Reference_Level.R")

options(digits = 8)

# Uncertainty attached to the estimated total carbon increment for AR
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
AreaTotal <- ARAreaUp + ARAreaLow 
# Carbon gains on areas afforested/reforested in one year
CarbonT <- AreaTotal * MAICar


# Uncertainty analysis
# Create vector
UncAnaRemAR <- vector()

UncAnaRemAR
# MC simulation
for(i in 1:MCRuns){ # i <- 1
  UncAnaRemAR[i] <- (  
      AreaTotal *  # Average annual area of AR
      VMAICar[i])                  # Random increment
}
UncAnaRemAR[1:10]


# Average annual removals from afforestation/reforestation (AR)
EstimateRemAR <- CarbonT * CarbonToCO2eRatio                              # Estimate
LCIRemAr <- quantile(UncAnaRemAR * CarbonToCO2eRatio, probs = QLCI) # Lower confidence limit
UCIRemAR <- quantile(UncAnaRemAR * CarbonToCO2eRatio, probs = QUCI) # Upper confidence limit
MCERemAR <- UncAnaRemAR * CarbonToCO2eRatio                            # MC estimates

# missing (-1)
EstimateRemAR 
LCIRemAr
UCIRemAR 

############################################################################

# MAICar  <- MAICAGBar * (1 + RootToShootTropRain) which is affected by final value for MAICAGBar

CalcRemAR <- function(
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
RemAR <- CalcRemAR(ARAreaLow, ARAreaUp, MAICar, CarbonToCO2eRatio)
RemAR
