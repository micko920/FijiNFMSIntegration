


load("./Data/mrUpdateOct22/Fiji_ER_Estimate_Params.RData")
load("./Data/mrUpdateOct22/fiji_frl_overall_years.RData")


load("./Data/mrUpdateOct22/Fiji_ER_Estimate_AccuracyAssessment.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_AccuracyAssessment.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrUpdateOct22/Fiji_ER_Estimate_Values.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Values.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrUpdateOct22/Fiji_ER_Estimate_UC.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_UC.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

load("./Data/mrUpdateOct22/Fiji_ER_Estimate_Sensitivity.RData")
rmarkdown::render(
  "./reports/Fiji_ER_Estimate_Sensitivity.Rmd",
  output_dir = "./reports",
  output_format = "html_document")

source("./Data/frlUpdateOct22/FRL_Parameters.R")
load("./Data/frlUpdateOct22/fiji_frl_tables.RData")
load("./Data/frlUpdateOct22/fiji_frl_input.RData")
load("./Data/frlUpdateOct22/fiji_frl_estimate_values.RData")
load("./Data/mrUpdateOct22/fiji_frl_overall_years.RData")

rmarkdown::render(
  "./reports/Fiji_ER_Report.Rmd",
  output_dir = "./reports",
  output_format = "html_document")





