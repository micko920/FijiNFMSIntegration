
# Updated 12 Feb 2024 - Changes from Audit and FRL Feb 2024

# Updated 14 Feb 2022 - Changes for RS data, pixel buffer and other method changes

# Updated 30 Sep 2021, Carly and Michael Green
# Update included changes to original FRL for Software harvesting only


# Updated 25 Sep 2021, Carly and Michael Green
#  Update included changes for: Software harvesting and a correction to the
#  Upland/Lowland boundry from 400m to 600m

# Original data from published FRL values in the ER-PD, Dated June 14 2019

load("./Data/mrAuditJuly24/Fiji_ER_Estimate_Params.RData")

MonitoredValues$year1$FDegBurnData <- read.table( "./Data/mrAuditJuly24/Burn2019.txt", sep="\t", header=TRUE)[, c("year", "area_ha", "age_yrs")]
MonitoredValues$year2$FDegBurnData <- read.table( "./Data/mrAuditJuly24/Burn2020.txt", sep="\t", header=TRUE)[, c("year", "area_ha", "age_yrs")]


# Published values from the Fiji Bureau of Stats. Ann. 2021 Table 3.8 Timber Production
MonitoredValues$year1$FPlnVolHarvHwd <- 21253
MonitoredValues$year2$FPlnVolHarvHwd <- 22254

MonitoredValues$year1$FPlnVolHarvSwd <- 419088
MonitoredValues$year2$FPlnVolHarvSwd <- 385314


load("./Data/frlAuditJuly24/fiji_frl_overall_years.RData", envir=(NE <- new.env()))
ErpaYearlyFRL <- NE$ErpaYearlyFRL
rm(NE)

MonitoringReportParams$MpDays<- 730

MonitoringReportParams$RpDays<- 539

MonitoringReportParams$IsRpEqualToMp<- (MonitoringReportParams$MpDays == MonitoringReportParams$RpDays)

MonitoringReportParams$RpMpRatio<- (MonitoringReportParams$RpDays / MonitoringReportParams$MpDays)

MonitoringReportParams$RpMpProrataYears<- (2 * MonitoringReportParams$RpMpRatio)

MonitoringReportParams$ErpaYearlyFRL <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","MP_FRL"]
MonitoringReportParams$ErpaYearlyFRLUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","UCI"]
MonitoringReportParams$ErpaYearlyFRLLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["NetFRL","LCI"]
MonitoringReportParams$ErpaYearlyFRLFDeg <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","MP_FRL"]
MonitoringReportParams$ErpaYearlyFRLFDegUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","UCI"]
MonitoringReportParams$ErpaYearlyFRLFDegLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDeg","LCI"]
MonitoringReportParams$ErpaYearlyFRLDefor <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","MP_FRL"]
MonitoringReportParams$ErpaYearlyFRLDeforUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","UCI"]
MonitoringReportParams$ErpaYearlyFRLDeforLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Defor","LCI"]
MonitoringReportParams$ErpaYearlyFRLEnh <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","MP_FRL"]
MonitoringReportParams$ErpaYearlyFRLEnhUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","UCI"]
MonitoringReportParams$ErpaYearlyFRLEnhLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["Enh","LCI"]
MonitoringReportParams$ErpaYearlyFRLFDegNonProxy <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","MP_FRL"]
MonitoringReportParams$ErpaYearlyFRLFDegNonProxyUCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","UCI"]
MonitoringReportParams$ErpaYearlyFRLFDegNonProxyLCI <- ErpaYearlyFRL$erpa_yearly$mp_frl["FDegNonProxy","LCI"]


MonitoringReportParams$ErpaTransferredERs<- 0

MonitoringReportParams$ErpaContestedERs<- 0

MonitoringReportParams$ErpaSoldERs<- 0

MonitoringReportParams$ErpaRiskSetaside<- 0.21

MonitoringReportParams$ErpaPreviousFRL<- 0

MonitoringReportParams$ErpaPreviousEmRems<- 0

MonitoringReportParams$ErpaPreviousERs<- 0

MonitoringReportParams$FDegUncertaintyDiscount<- 0.15


save.image("./Data/mrAuditJuly24/Fiji_ER_Estimate_Params.RData")
