# Tables Creation Functions for ER Monitoring Report. 4.2, 4.3, 5.2.2, 7 and 8

formatPercent <- function(x) {
  return(ifelse(is.na(x),"--", paste(format(round(x * 100, 2), nsmall = 2), "%")))
}

formatMaxPercent <- function(x) {
  return(ifelse(x>1,">100%",formatPercent(x)))
}

formatNumber <- function(x) {
  return(format(round(x, 0), nsmall = 0))
}

formatDecimal <- function(x) {
  return(format(round(x, 2), nsmall = 2))
}


createTable_MPEmRems <- function(MR) {
  TableMPEmRems <- data.frame(
    Year = c(MR$year1$year, MR$year2$year, "Total"),
    Deforestation = c(
      formatNumber(MR$year1$GrossEmDefor),
      formatNumber(MR$year2$GrossEmDefor),
      formatNumber(MR$MpGrossEmDefor)
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
  names(TableMPEmRems) <- c(
    "Year",
    "Emissions from deforestation (tCO2e/yr)",
    "Emissions from forest degradation (tCO2e/yr)",
    "Removals by sinks (tCO2e/yr)",
    "Net emissions and removals (tCO2e/yr)"
  )

  return(TableMPEmRems)
}






