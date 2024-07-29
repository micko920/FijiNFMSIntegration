


load("./Data/mrAuditJuly24/Fiji_ER_Estimate_Params.RData")
load("./Data/mrAuditJuly24/fiji_frl_overall_years.RData")


load("./Data/mrAuditJuly24/Fiji_ER_Estimate_AccuracyAssessment.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_AccuracyAssessment.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrAuditJuly24/Fiji_ER_Estimate_Values.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Values.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrAuditJuly24/Fiji_ER_Estimate_UC.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_UC.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrAuditJuly24/Fiji_ER_Estimate_Sensitivity.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Sensitivity.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

source("./Data/frlAuditJuly24/FRL_Parameters.R")
load("./Data/frlAuditJuly24/fiji_frl_tables.RData")
load("./Data/frlAuditJuly24/fiji_frl_input.RData")
load("./Data/frlAuditJuly24/fiji_frl_estimate_values.RData")
load("./Data/mrAuditJuly24/fiji_frl_overall_years.RData")

rmarkdown::render(
  "./reports/Fiji_ER_Report.Rmd",
  output_dir = "./reports",
  output_format = "html_document")


source("./MonitoringReportExtraTableCreationFunctions.R")
rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables.Rmd",
  output_dir = "./reports",
  output_format = "html_document")


rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-2019.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

rmarkdown::render(
  "./reports/Fiji_ER_MonitoringReportExtraTables-2020.Rmd",
  output_dir = "./reports",
  output_format = "html_document")
