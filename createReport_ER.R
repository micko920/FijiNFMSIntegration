

library(FijiNFMSCalculations)

source("./getDataPath.R")
load(file = getDataPath("Fiji_ER_Estimate_Params.RData"))
load(file = getDataPath("fiji_frl_overall_years.RData"))




load(file = getPeriodDataPath("Fiji_ER_Estimate_AccuracyAssessment", MonitoringReportParams$period$description, "RData"))
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_AccuracyAssessment.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_Estimate_AccuracyAssessment", MonitoringReportParams$period$description, "html")))



load(file = getPeriodDataPath("Fiji_ER_Estimate_Values", MonitoringReportParams$period$description, "RData"))
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Values.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_Estimate_Values", MonitoringReportParams$period$description, "html")))

load(file = getPeriodDataPath("Fiji_ER_Estimate_UC", MonitoringReportParams$period$description, "RData"))
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_UC.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_Estimate_UC", MonitoringReportParams$period$description, "html")))

load(file = getPeriodDataPath("Fiji_ER_Estimate_Sensitivity", MonitoringReportParams$period$description, "RData"))
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Sensitivity.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_Estimate_Sensitivity", MonitoringReportParams$period$description, "html")))


source("./MonitoringReportExtraTableCreationFunctions.R")







source("./Data/frlTrainingMar2026/FRL_Parameters.R")
load("./Data/frlTrainingMar2026/fiji_frl_tables.RData")
load("./Data/frlTrainingMar2026/fiji_frl_input.RData")
load("./Data/frlTrainingMar2026/fiji_frl_estimate_values.RData")



MultiYear_EmRems_Values <- list()
MultiYear_EmRems_Values$year <- list()

load(file = getPeriodDataPath("Fiji_ER_Estimate_Values", "2019-2020", "RData"))
MultiYear_EmRems_Values$year[["2019"]] <- EmRems_Values$year1
MultiYear_EmRems_Values$year[["2020"]] <- EmRems_Values$year2

ReportYear = "2019"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2019", "html")))

ReportYear = "2020"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2020", "html")))


load(file = getPeriodDataPath("Fiji_ER_Estimate_Values", "2021-2022", "RData"))
MultiYear_EmRems_Values$year[["2021"]] <- EmRems_Values$year1
MultiYear_EmRems_Values$year[["2022"]] <- EmRems_Values$year2

ReportYear = "2021"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2021", "html")))

ReportYear = "2022"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2022", "html")))

load(file = getPeriodDataPath("Fiji_ER_Estimate_Values", "2023-2024", "RData"))
MultiYear_EmRems_Values$year[["2023"]] <- EmRems_Values$year1
MultiYear_EmRems_Values$year[["2024"]] <- EmRems_Values$year2

ReportYear = "2023"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2023", "html")))

ReportYear = "2024"
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-year.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getPeriodDataPath("Fiji_ER_MonitoringReportExtraTables", "2024", "html")))



MultiYear_ResultsTables <- list()
MultiYear_ResultsTables$year <- list()

load(file = getPeriodDataPath("Fiji_ER_Estimate_UC", "2019-2020", "RData"))
MultiYear_ResultsTables$year[["2019"]] <- ResultsTables$year1
MultiYear_ResultsTables$year[["2020"]] <- ResultsTables$year2

load(file = getPeriodDataPath("Fiji_ER_Estimate_UC", "2021-2022", "RData"))
MultiYear_ResultsTables$year[["2021"]] <- ResultsTables$year1
MultiYear_ResultsTables$year[["2022"]] <- ResultsTables$year2


load(file = getPeriodDataPath("Fiji_ER_Estimate_UC", "2023-2024", "RData"))
MultiYear_ResultsTables$year[["2023"]] <- ResultsTables$year1
MultiYear_ResultsTables$year[["2024"]] <- ResultsTables$year2



rmarkdown::render(
  "./reports/Fiji_ER_Report.Rmd",
  output_format = "html_document",
  output_file = paste0("../", getDataPath("Fiji_ER_Report.html")))


