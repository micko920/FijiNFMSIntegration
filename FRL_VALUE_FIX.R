
# TODO:

# FRL_VALUE_FIX
# This file is a hack to allow the command line tools to work.
# It needs to be removed in favour of using a RData file with a pdf based on
# sample of samples from the FRL Monte Carlo set.

# Updated 30 Sep 2021, Carly and Michael Green
# Update included changes to original FRL for Software harvesting only


# Updated 25 Sep 2021, Carly and Michael Green
#  Update included changes for: Software harvesting and a correction to the
#  Upland/Lowland boundry from 400m to 600m

# Original data from published FRL values in the ER-PD, Dated June 14 2019

############### Forest Reference Level
#
# Updated 30 Sep 2021
# Updated 25 Sep 2021
#
# FRL <- 1636800 # (tCO2e/yr) Sum of Net Emissions from Fiji Baseline (FRL) to 6 significant figures
# FRL <- 1778421.58 # Update 25 Sep 2021
FRL <- 1874373.34 # Update 30 Sep 2021

# FRL_UCI <- 2444030
# FRL_UCI <- 2492427.05 # Update 25 Sep 2021
FRL_UCI <- 2548813.33 # Update 30 Sep 2021

# FRL_LCI <- 953460
# FRL_LCI <- 997407.34 # Update 25 Sep 2021
FRL_LCI <- 1101723.67 # Update 30 Sep 2021


############### Defororestation
#
# Updated 30 Sep 2021
# Updated 25 Sep 2021
#
# FRLDeforestation <- 2696831 # from Table12.5 of ER-PD
# FRLDeforestation <- 2650382.22 # Update 25 Sep 2021
FRLDeforestation <- 2696830.64 # Update 30 Sep 2021

# FRLDeforestation_UCI <- 3373850
# FRLDeforestation_UCI <- 3248409.49 # Update 25 Sep 2021
FRLDeforestation_UCI <- 3252371.26 # Update 30 Sep 2021

# FRLDeforestation_LCI <- 2143830
# FRLDeforestation_LCI <- 1987568.49 # Update 25 Sep 2021
FRLDeforestation_LCI <- 2048438.97 # Update 30 Sep 2021


############### Forest Degradation
#
# Updated 30 Sep 2021
#
# Original Values:
### There is a mistake in the value reported in the ER-PD The value (310442) is outside (less than) the LCI(321925).
# This was recalculated and checked.
# From Table12.5 of ER-PD  - Net emissions forest degradation aanefd frltab[11,2]
# FRLForestDegradation <- 310442
FRLForestDegradation <- 310442.36 # Update 30 Sep 2021

# FRLForestDegradation_UCI <- 358537
FRLForestDegradation_UCI <- 381776.61 # Update 30 Sep 2021

# This value was replaced via recalculation. See Note above.
# FRLForestDegradation_LCI <- 321925
# FRLForestDegradation_LCI <- 274025
FRLForestDegradation_LCI <- 241686.83 # Update 30 Sep 2021


############### Removals by Sink
# Net emissions enhancement of forest carbon stocks (EC). Includes Aforestation
# and Hardword and Softwood Plantations aaneec frltab[12,2]
#
#
# Updated 30 Sep 2021
# Updated 25 Sep 2021
#
# Original Values:
### Note: UCI and LCI incorrectly swapped in ER-PD
# from Table12.5 of ER-PD

# FRLRemovalsBySinks <- -1370469
# FRLRemovalsBySinks <- -1182402.99 # Update 25 Sep 2021
FRLRemovalsBySinks <- -1132899.65 # Update 30 Sep 2021

# FRLRemovalsBySinks_UCI <- -975054
# FRLRemovalsBySinks_UCI <- -804802.25 # Update 25 Sep 2021
FRLRemovalsBySinks_UCI <- -751897.61 # Update 30 Sep 2021

# FRLRemovalsBySinks_LCI <- -1661630
# FRLRemovalsBySinks_LCI <- -1555230.62 # Update 25 Sep 2021
FRLRemovalsBySinks_LCI <- -1538544.58 # Update 30 Sep 2021


############### FRL Adjustments
FRLAdjustments <- 0 # Always zero
