
# Updated 14 Feb 2022 - Changes for RS data, pixel buffer and other method changes

# Updated 30 Sep 2021, Carly and Michael Green
# Update included changes to original FRL for Software harvesting only


# Updated 25 Sep 2021, Carly and Michael Green
#  Update included changes for: Software harvesting and a correction to the
#  Upland/Lowland boundry from 400m to 600m

# Original data from published FRL values in the ER-PD, Dated June 14 2019

burn_data_2019 <- read.table( "./Data/mrUpdate14Feb22/Burn2019.txt", sep="\t", header=TRUE)
burn_data_2020 <- read.table( "./Data/mrUpdate14Feb22/Burn2020.txt", sep="\t", header=TRUE)

MonitoringReportParams$MpDays<- 730

MonitoringReportParams$RpDays<- 540

MonitoringReportParams$IsRpEqualToMp<- (MonitoringReportParams$MpDays == MonitoringReportParams$RpDays)

MonitoringReportParams$RpMpRatio<- (MonitoringReportParams$RpDays / MonitoringReportParams$MpDays)

MonitoringReportParams$RpMpProrataYears<- (2 * MonitoringReportParams$RpMpRatio)


#MonitoringReportParams$ErpaYearlyFRL<- 1874373
MonitoringReportParams$ErpaYearlyFRL<- -200708.4

# MonitoringReportParams$ErpaYearlyFRLUCI<- 2548813
MonitoringReportParams$ErpaYearlyFRLUCI<- 74751.6

# MonitoringReportParams$ErpaYearlyFRLLCI<- 1101724
MonitoringReportParams$ErpaYearlyFRLLCI<- -494362.8

# MonitoringReportParams$ErpaYearlyFRLDefor<- 2696830
MonitoringReportParams$ErpaYearlyFRLDefor<- 369186.7

# MonitoringReportParams$ErpaYearlyFRLDeforUCI<- 3252371
MonitoringReportParams$ErpaYearlyFRLDeforUCI<- 471427.9

# MonitoringReportParams$ErpaYearlyFRLDeforLCI<- 2048439
MonitoringReportParams$ErpaYearlyFRLDeforLCI<- 253519.3

# MonitoringReportParams$ErpaYearlyFRLFDeg<- 310442
MonitoringReportParams$ErpaYearlyFRLFDeg<- 310442.4

# MonitoringReportParams$ErpaYearlyFRLFDegUCI<- 381777
MonitoringReportParams$ErpaYearlyFRLFDegUCI<- 355432.8

# MonitoringReportParams$ErpaYearlyFRLFDegLCI<- 241687
MonitoringReportParams$ErpaYearlyFRLFDegLCI<- 263812.9

# MonitoringReportParams$ErpaYearlyFRLEnh<- -1132900
MonitoringReportParams$ErpaYearlyFRLEnh<- -880337.4

# MonitoringReportParams$ErpaYearlyFRLEnhUCI<- -751898
MonitoringReportParams$ErpaYearlyFRLEnhUCI<- -623452.1

# MonitoringReportParams$ErpaYearlyFRLEnhLCI<- -1538545
MonitoringReportParams$ErpaYearlyFRLEnhLCI<- -1136858.4

MonitoringReportParams$ErpaTransferredERs<- 0

MonitoringReportParams$ErpaContestedERs<- 0

MonitoringReportParams$ErpaSoldERs<- 0

MonitoringReportParams$ErpaRiskSetaside<- 0.16

MonitoringReportParams$ErpaPreviousFRL<- 0

MonitoringReportParams$ErpaPreviousEmRems<- 0

MonitoringReportParams$ErpaPreviousERs<- 0

MonitoringReportParams$FDegUncertaintyDiscount<- 0.15
