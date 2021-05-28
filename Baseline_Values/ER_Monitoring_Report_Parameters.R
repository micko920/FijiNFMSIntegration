# ER Monitoring Report Parameters

MonitoringReportParams <- list()
MonitoringReportParams$MpDays <- 730 # number of days in the Monitoring Period # Always 2 years (365*2) Don't change this...
MonitoringReportParams$RpDays <- 540 # number of days in the Reporting Period
MonitoringReportParams$IsRpEqualToMp <- (MonitoringReportParams$MpDays == MonitoringReportParams$RpDays)
MonitoringReportParams$RpMpRatio <- (MonitoringReportParams$RpDays / MonitoringReportParams$MpDays)
MonitoringReportParams$RpMpProrataYears <- 2 * MonitoringReportParams$RpMpRatio
MonitoringReportParams$ErpaYearlyFRL <-  FRL
MonitoringReportParams$ErpaYearlyFRLDefor <-  FRLDeforestation
MonitoringReportParams$ErpaYearlyFRLFDeg <-  FRLForestDegradation
MonitoringReportParams$ErpaYearlyFRLEnh <-  FRLRemovalsBySinks


MonitoringReportParams$ErpaTransferredERs <- 0 # Amount of ERs that have been previously transferred to the
# Carbon Fund, as Contract ERs and Additional ERs
MonitoringReportParams$ErpaContestedERs <- 0 # Number of ERs for which the ability to transfer Title to ERs
# is still unclear or contested at the time of transfer of ERs

MonitoringReportParams$ErpaSoldERs <- 0 # ERs sold, assigned or otherwise used by any other entity

MonitoringReportParams$ErpaRiskSetaside <- 0.16 # Total reversal risk set-aside percentage applied to the ER program.

MonitoringReportParams$ErpaPreviousFRL <- 0 # Todo: It is the sum of FRL from previous reporting periods

MonitoringReportParams$ErpaPreviousEmRems <- 0 # Todo: It is the sum of Emission
# Removals from previous reporting periods

MonitoringReportParams$ErpaPreviousERs <- 0 # Todo: It is the sum of Emission
# Reductions from previous reporting periods

MonitoringReportParams$FDegUncertaintyDiscount <- 0.15 #Defined Forest Degradation Uncertainty Discount for Table 5.2.2
