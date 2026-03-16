

FRLParams <- list()

# Generic ..............................................................................
FRLParams$runs          <- 1000     # Number of Monte-Carlo runs
FRLParams$qlci          <- 0.05      # Lower quantile (confidence bounds, 0.05 <- 90% CI)
FRLParams$quci          <- 0.95      # Upper quantile (confidence bounds, 0.95 <- 90% CI)
FRLParams$etacf         <- 0.47      # Biomass to carbon conversion (IPCC default)
FRLParams$etacc         <- 44/12     # Carbon to CO_2e conversion (IPCC default * -1)
FRLParams$Ty            <- 2006:2016 # Years of the FRL Reference Period
FRLParams$Tl            <- length(FRLParams$Ty)    # Number of years in the FRL Reference Period
FRLParams$deltaT        <- c(FRLParams$Tl:1) - 0.5 # Years of growth over the Reference Period
FRLParams$rdeltaT       <- rev(FRLParams$deltaT)   # Reverse: Years of growth over the Reference Period


# Deforestation ........................................................................
FRLParams$srtmthresh    <- 600       # Elevation threshold (Mountain otherwise)
FRLParams$aithresh      <- 2         # Aridity Index threshold (<2 dry, wet otherwise)

FRLParams$merrdbh       <- 0.1       # Measurement error DBH 10% (NFI 2016 MC simulations)

# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4
# Tropical mountain systems
FRLParams$ruk           <- 0.27
FRLParams$uciruk        <- 0.28
FRLParams$lciruk        <- 0.2699999

# Above- and below-ground biomass in compartments
# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4
# Tropical moist deciduous forest (<125 tAGB ha^-1)
FRLParams$rdlk1         <- 0.20
FRLParams$ucirdlk1      <- 0.25
FRLParams$lcirdlk1      <- 0.09
# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4 R for tropical moist
# deciduous with AGB >125 t ha^-1); R for Dry Lowland
FRLParams$rdlk2         <- 0.24
FRLParams$ucirdlk2      <- 0.33
FRLParams$lcirdlk2      <- 0.22
# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4 R for tropical rainforest);
# R for Wet Lowland (0.37)
FRLParams$Rlwk          <- 0.37      # Root-to-shoot ration tropical rainforest (IPCC)
FRLParams$errRlwk       <- 0.25      # Error root-to-shoot ratio tropical rainforest

FRLParams$cgrass        <- 17.11364  # Average carbon stocks in grassland (Rounds, 2013)
FRLParams$errcgrass     <- 0.75      # Percent error in 'FRLParams$cgrass'

# Forest degradation ...................................................................
FRLParams$efell         <- 0.69      # Emission factor felling (component of TEF)
FRLParams$edam          <- 0.15      # Emission factor damage (component of TEF)
FRLParams$einfr         <- 0.21      # Emission factor infrastructure (component of TEF)
# To 'translate' volumes extracted to carbon loss, the 'Total Emissions Factor' from
# Haas (2015) was used. The TEF has three components:
#   - EMFELL        C loss caused by the log itself (including logging residuals)
#   - EMDAM         C loss due to damage to the remaining stand
#   - EMINFR        C loss caused by logging infrastructure establishment
# Total Emission Factor (TEF)
FRLParams$tef           <- FRLParams$efell + FRLParams$edam + FRLParams$einfr
FRLParams$errtef        <- 0.25      # Relative error of TEF
# Mean annual increment (MAI) of total C (above- and below-ground carbon) for
# conventional logging (Mussong, unpublished)
FRLParams$maiclnf       <- 0.99      # Mean annual (total carbon increment in Natural Forest)
FRLParams$errmaiclnf    <- 0.5       # Relative error in 'maiclnf'
FRLParams$erralnf       <- 0.25      # Relative error in areas logged in Natural Forest



# Mean annual (total) biomass increment in Softwood Plantations (Waterloo, 1994)
# Mean annual increment (total tree biomass, i.e., AGB + BGB).  Table 11.19 in
# Waterloo (1994)
FRLParams$maibp         <- 10       # Mean annual biomass increment in Softwood Plantations
FRLParams$errmaibp      <- 0.25     # Relative error in 'maibp'
# Mean annual carbon increment in Softwood Plantations (C ha^-1 yr^-1)
FRLParams$maicp <- FRLParams$maibp * FRLParams$etacf
FRLParams$errmaicp      <- 0.25     # Relative error in 'maicp'


FRLParams$errghg        <- 0.5     # Relatice Error for GHGs, combustion factor, GWP
FRLParams$sdCO2EF       <- 90      # standard diviation CO2 Emission Factor
# Enhancement of carbon stocks .........................................................
FRLParams$maicar        <- 1.918455  # Mean annual total carbon increment (AR)
FRLParams$errmaicar     <- 0.5       # Relative error in maicar

# Biomass conversion and expansion factor (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.5 value
# for humid tropical natural forest >200 m^3)
FRLParams$bcefrhw       <- 1.05     # Biomass conversion and expansion factor (Hardwood)
FRLParams$errbcefrhw    <- 0.25     # Relative error in 'bcefrhw'

# Mean annual increment (MAI) for volume [m^3]. Estimated from data in the last table in
# the document provided by Fiji Hardwood Corporation Limited (FHCL)
# Calculation: sum(hw_species[,2] * (hw_species[,5] / sum(hw_species[,5])))
FRLParams$maivhww       <- 5.85     # Weighted mean annual volume increment (Hardwood)
FRLParams$errmaivhw     <- 0.25     # Relative error in 'maivhw' and 'maivhww'
# Biomass conversion and expansion factor (BCEF) for MAI volume (IPCC, 2006; Vol. 4,
# Chap. 4, Tab. 4.5, humid tropical rainforest, growing stock 21-40 m^3)
FRLParams$bcefihw       <- 1.1      # Biomass conversion and exp. factor (increment Hardwood)
FRLParams$errbcefihw    <- 0.25     # Relative error in 'bcefihw'

# Wood density of pine (softwoods); see Cown (1981). Pine plantations are mostly located
# below 300 m a.s.l.
FRLParams$wdsw          <- 0.47     # Wood density Softwood
# Standard deviation wood density pine (Cown, 1981; Tab. 2)
FRLParams$sdwdsw        <- 0.0509   # Standard deviation of 'wdsw'
# The value of FRLParams$volTovol = .76 is the ratio between utilizable volume and total tree
# volume The value was taken from Waterloo (1994), Fig. 11.7, page 221.
FRLParams$volTovol      <- 0.76     # Harvested vol. to total vol. (recovered volume prop.)
FRLParams$errvolTovol   <- 0.05     # Relative error in 'volTovol'
# Cutting cycle length softwood (data provided by FPL)
FRLParams$cuttingc      <- 20       # Rotation length in year in Softwood Plantations
FRLParams$errcuttingc   <- 5        # Error in 'cuttingc'


FRLParams$errHwPlantations <- 0.5  # Error in Hardwood Plantation data

# This should be removed for production
set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI
