# Tables Creation Functions for ER Monitoring Report. 4.2, 4.3, 5.2.2, 7 and 8

formatPercent <- function(x) {
  return(paste(format(round(x * 100, 2), nsmall = 2), "%"))
}

formatNumber <- function(x) {
  return(format(round(x, 0), nsmall = 0))
}

formatDecimal <- function(x) {
  return(format(round(x, 2), nsmall = 2))
}

createTable_4_2 <- function(MR) {
  Table4_2 <- data.frame(
    Year = c(MR$year1$year, MR$year2$year, "Total"),
    Deforestation = c(
      formatNumber(MR$year1$EstEmRemsDefor),
      formatNumber(MR$year2$EstEmRemsDefor),
      formatNumber(MR$MpEstEmRemsDefor)
    ),
    Degradation = c(
      formatNumber(MR$year1$EstEmRemsFDeg),
      formatNumber(MR$year2$EstEmRemsFDeg),
      formatNumber(MR$MpEstEmRemsFDeg)
    ),
    Removals = c(
      formatNumber(MR$year1$EstEmRemsEnh),
      formatNumber(MR$year2$EstEmRemsEnh),
      formatNumber(MR$MpEstEmRemsEnh)
    ),
    NetEmissionRemovals = c(
      formatNumber(MR$year1$NetEmRems),
      formatNumber(MR$year2$NetEmRems),
      formatNumber(MR$MpNetEmRems)
    )
  )
  names(Table4_2) <- c(
    "Year",
    "Emissions from deforestation (tCO2e/yr)",
    "Emissions from forest degradation (tCO2e/yr)",
    "Removals by sinks (tCO2e/yr)",
    "Net emissions and removals (tCO2e/yr)"
  )

  return(Table4_2)
}


createTable_4_3 <- function(MR, MRparams) {
  if (MRparams$IsRpEqualToMp) {
    Table4_3 <- data.frame(
      row.names = c(
        "Total Reference Level emissions during the Reporting Period (tCO2e)",
        "Net emissions and removals under the ER Program during the Reporting Period (tCO2e)",
        "Emission Reductions during the Reporting Period (tCO2e)"
      ),
      Equal = c(
        formatNumber(MR$RpEstFRL),
        formatNumber(MR$RpNetEmRems),
        formatNumber(MR$RpEstERs) #  Monitoring Period length == Reporting Period Length
      )
    )
  } else {
    Table4_3 <- data.frame(
      row.names = c(
        "Total Reference Level emissions during the Monitoring Period (tCO2e)",
        "Net emissions and removals under the ER Program during the Monitoring Period (tCO2e)",
        "Emission Reductions during the Monitoring Period (tCO2e)",
        "Length of the Reporting Period / Length of the Monitoring Period (#days/# days)",
        "Emission Reductions during the Reporting Period (tCO2e)"
      ),
      NotEqual = c(
        formatNumber(MR$MpEstFRL),
        formatNumber(MR$MpNetEmRems),
        formatNumber(MR$MpEstERs),
        formatDecimal(MRparams$RpMpRatio), #  Monitoring Period length != Reporting Period Length
        formatNumber(MR$RpEstERs)
      )
    )
  }
  names(Table4_3) <- c(
    "Value"
  )

  return(Table4_3)
}

# Table in section 5.2 Quantification of the uncertainty of the estimate of Emission Reductions


createTable_5_2_2 <- function(MR) {
  Table5_2_2 <- data.frame(
    row.names = c(
      "Median",
      "Upper Bound 90% CI",
      "Lower Bound 90% CI",
      "Half Width Confidence Interval at 90%",
      "Relative margin",
      "Uncertainty discount"
    ),
    totalEmissions = c(
      formatNumber(MR$MpEstERsDefEnh$median),
      formatNumber(MR$MpEstERsDefEnh$UCI),
      formatNumber(MR$MpEstERsDefEnh$LCI),
      formatNumber(MR$MpEstERsDefEnh$halfWidth),
      formatPercent(MR$MpEstERsDefEnh$relativeMargin),
      formatPercent(MR$MpEstERsDefEnh$conserFactor)
    ),
    forestDeg = c(
      formatNumber(MR$MpEstERsFDeg$median),
      formatNumber(MR$MpEstERsFDeg$UCI),
      formatNumber(MR$MpEstERsFDeg$LCI),
      formatNumber(MR$MpEstERsFDeg$halfWidth),
      formatPercent(MR$MpEstERsFDeg$relativeMargin),
      formatPercent(MR$MpEstERsFDeg$conserFactor)
    )
  )
  names(Table5_2_2) <- c(
    "Total Emissions Reductions",
    "Forest Degradation"
  )
  return(Table5_2_2)
}


createTable_7_2 <- function(MR, MRparams) {
  Table7_2 <- data.frame(
    row.names = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K"
    ),
    Titles = c(
      "ER Program Refrence Level for this Reporting Period",
      "ER Program Reference Level for all previous Reporting Periods in the ERPA",
      "Cumulative Reference Level Emissions for all Reporting Periods",
      "Estimation of emissions by sources and removals by sinks for this Reporting Period",
      "Estimation of emissions by sources and removals by sinks for all previous Reporting Periods in the ERPA",
      "Cumulative emissions by sources and removals by sinks including the current reporting period",
      "Cumulative quantity of Total ERs estimated including the current reporting period",
      "Cumulative quantity of Total ERs estimated for prior reporting periods",
      "Available ERs this Reporting Period",
      "Amount of ERs that have been previously transfered to the Carbon Fund as Contract ERs and Additional ERs",
      "Quantity of Buffer ERs to be cancelled from the REversal Buffer account"
    ),
    Values = c(
      # A
      formatNumber(MR$RpEstFRL),
      # B
      formatNumber(MRparams$ErpaPreviousFRL),
      # C
      formatNumber(MR$ErpaCurrentFRL), #   A + B
      # D
      formatNumber(MR$RpNetEmRems),
      # E
      formatNumber(MRparams$ErpaPreviousEmRems),
      # F
      formatNumber(MR$ErpaCurrentEmRems), # D + E
      # G
      formatNumber(MR$ErpaCurrentERs), # C - F
      # H
      formatNumber(MRparams$ErpaPreviousERs),
      # I
      formatNumber(MR$ErpaCurrentBalance), # G - H
      # J
      ifelse(MR$IsReversal, formatNumber(MRparams$ErpaTransferredERs), "NA"),
      # k
      ifelse(MR$IsReversal, formatNumber(MR$RpCanceledERs), "NA") # J / (H * (H - G))
    )
  )
}

createTable_8 <- function(MR, MRparams) {
  Table <- data.frame(
    row.names = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K",
      "L"
    ),
    Titles = c(
      "Emission Reductions during the Reporting Period",
      "If applicable, number of Emission Reductions from reducing forest degradation that have been estimated using proxy-based estimation approaches (use zero if not applicable)",
      "Number of Emission Reductions estimated using measurement approaches",
      "Conservativeness Factor to reflect the level of uncertainty from non-proxy based approaches associated with the estimation of ERs during the Crediting Period",
      "Calculate Uncertainty set-aside",
      "Emission Reductions after uncertianty set-aside",
      "Number of ERs for which the ability to transfer Title to ERs is still unclear or contested at the time of transfer of ERs",
      "ERs sold, assigned or otherwise used by any other entity for sale, public relations, compliance or any other purpose including ERS that have been set-side to meet Reversal management requirements under other GHG accounting schemes",
      "Potential ERs that can be transfered to the Carbon Fund before reversal risk set-aside",
      "Total reversal risk set-aside percentage applied to the ER program",
      "Quantity of ERs allocated to the Reversal Buffer and the Pooled Reversal Buffer",
      "Number of FCPF ERs"
    ),
    Values = c(
      # A
      formatNumber(MR$RpEstERs),
      # B
      formatNumber(MR$RpEstERsFDeg),
      # C
      formatNumber(MR$RpEstERsDefEnh), #  A - B
      # D  from Table5_2 conservativeness factor.
      formatPercent(MR$MpEstERsDefEnh$conserFactor),
      # E
      formatNumber(MR$RpSetaside), # (0.15 * B) + (C * D), 0.15 is default ConserFactor for FDeg
      # F
      formatNumber(MR$RpAdjERs), # A - E
      # G
      formatNumber(MRparams$ErpaContestedERs),
      # H
      formatNumber(MRparams$ErpaSoldERs),
      # I
      formatNumber(MR$RpPotentialERs), # F - G - H
      # J
      formatPercent(MRparams$RISKsetaside),
      # K
      formatNumber(MR$RpBufferedERs), # (I * J)
      # L,  there is a mistake in the Table. (I - L) should be (I - K)
      formatNumber(MR$RpERs)
    )
  )
}
