


#Updated 25 Sep 2021, Carly and Michael Green
#  Update included changes for: Software harvesting and a correction to the
#  Upland/Lowland boundry from 400m to 600m

#Original data from published FRL values in the ER-PD, Dated June 14 2019

############### Forest Reference Level
#
# Updated 25 Sep 2021
#
#FRL <- 1636800 # (tCO2e/yr) Sum of Net Emissions from Fiji Baseline (FRL) to 6 significant figures
FRL <- 1778421.58

#FRL_UCI <- 2444030
FRL_UCI <- 2492427.05

#FRL_LCI <- 953460
FRL_LCI <- 997407.34



############### Defororestation
#
# Updated 25 Sep 2021
#
#FRLDeforestation <- 2696831 # from Table12.5 of ER-PD
FRLDeforestation <- 2650382.22

#FRLDeforestation_UCI <- 3373850
FRLDeforestation_UCI <- 3248409.49

#FRLDeforestation_LCI <- 2143830
FRLDeforestation_LCI <- 1987568.49


############### Forest Degradation
#
#
# Original Values:
### There is a mistake in the value reported in the ER-PD The value (310442) is outside (less than) the LCI(321925).
# This was recalculated and checked.
# From Table12.5 of ER-PD  - Net emissions forest degradation aanefd frltab[11,2]
FRLForestDegradation <- 310442 #

FRLForestDegradation_UCI <- 358537

# This value was replaced via recalculation. See Note above.
#FRLForestDegradation_LCI <- 321925
FRLForestDegradation_LCI <- 274025



############### Removals by Sink
# Net emissions enhancement of forest carbon stocks (EC). Includes Aforestation
# and Hardword and Softwood Plantations aaneec frltab[12,2]
#
#
# Updated 25 Sep 2021
#
# Original Values:
### Note: UCI and LCI incorrectly swapped in ER-PD
# from Table12.5 of ER-PD

#FRLRemovalsBySinks <- -1370469
FRLRemovalsBySinks <- -1182402.99

#FRLRemovalsBySinks_UCI <- -975054
FRLRemovalsBySinks_UCI <- -804802.25

#FRLRemovalsBySinks_LCI <- -1661630
FRLRemovalsBySinks_LCI <- -1555230.62


############### FRL Adjustments
FRLAdjustments <- 0 # Always zero
