

source("./getDataPath.R")

load(file = getDataPath("Fiji_ER_Estimate_Params.RData"))

ReportSettings <- list()
ReportSettings$params <- list()
ReportSettings$params[[1]] <-
  list(
    period = list( 
      start= 2019, 
      end = 2020, 
      description="2019-2020",
      MpDays = 730, # number of days in the Monitoring Period 
                    # Always 2 years (365*2) Don't change this...
      RpDays = 539, # number of days in the Reporting Period
      IsRpEqualToMp = FALSE,
      RpMpRatio = 539/730,
      RpMpProrataYears = (539/730) * 2
    ),
    year1 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    year2 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                             # Carbon Fund, as Contract ERs and Additional ERs
    ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                             # is still unclear or contested at the time of transfer of ERs
    ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
    ErpaRiskSetaside = 0.21, # Total reversal risk set-aside percentage applied to the ER program.
    ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
    ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                             # Removals from previous reporting periods
    ErpaPreviousERs = 0,     # Todo: It is the sum of Emission
                             # Reductions from previous reporting periods
    FDegUncertaintyDiscount = 0.15 #Defined Forest Degradation Uncertainty Discount for Table 5.2.2
  )
ReportSettings$params[[2]] <-
  list(
    period = list( 
      start= 2021, 
      end = 2022, 
      description="2021-2022",
      MpDays = 730,
      RpDays = 730,
      IsRpEqualToMp = TRUE,
      RpMpRatio = 1,
      RpMpProrataYears = 2
    ),
    year1 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    year2 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    ErpaTransferredERs = 0,
    ErpaContestedERs = 0,
    ErpaSoldERs = 0,
    ErpaRiskSetaside = 0.21,
    ErpaPreviousFRL = 0,
    ErpaPreviousEmRems = 0,
    ErpaPreviousERs = 0,
    FDegUncertaintyDiscount = 0.15
  )

ReportSettings$params[[3]] <-
  list(
    period = list( 
      start= 2023, 
      end = 2024, 
      description="2023-2024",
      MpDays = 730,
      RpDays = 730,
      IsRpEqualToMp = TRUE,
      RpMpRatio = 1,
      RpMpProrataYears = 2
    ),
    year1 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    year2 = list(
      ErpaTransferredERs = 0,  # Amount of ERs that have been previously transferred to the
                               # Carbon Fund, as Contract ERs and Additional ERs
      ErpaContestedERs = 0,    # Number of ERs for which the ability to transfer Title to ERs
                               # is still unclear or contested at the time of transfer of ERs
      ErpaSoldERs = 0,         # ERs sold, assigned or otherwise used by any other entity
      ErpaPreviousFRL = 0,     # Todo: It is the sum of FRL from previous reporting periods
      ErpaPreviousEmRems = 0,  # Todo: It is the sum of Emission
                               # Removals from previous reporting periods
      ErpaPreviousERs = 0      # Todo: It is the sum of Emission
                               # Reductions from previous reporting periods
    ),
    ErpaTransferredERs = 0,
    ErpaContestedERs = 0,
    ErpaSoldERs = 0,
    ErpaRiskSetaside = 0.21,
    ErpaPreviousFRL = 0,
    ErpaPreviousEmRems = 0,
    ErpaPreviousERs = 0,
    FDegUncertaintyDiscount = 0.15
  )

MonitoringReportParams <- ReportSettings$params[[3]]

save(
  list = c("ErpaYearlyFRL",
           "MonitoredValues",
           "ReportSettings",
           "MonitoringReportParams"),
  file = paste(getDataPath("Fiji_ER_Estimate_Params.RData"))
)