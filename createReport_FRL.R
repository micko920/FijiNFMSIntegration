


source("./Data/frlAuditJuly24/FRL_Parameters.R")
load("./Data/frlAuditJuly24/fiji_frl_tables.RData")
load("./Data/frlAuditJuly24/fiji_frl_input.RData")
load("./Data/frlAuditJuly24/fiji_frl_estimate_values.RData")
load("./Data/frlAuditJuly24/fiji_frl_overall_years.RData")
load("./Data/frlAuditJuly24/fiji_frl_adjusted_areas.RData")
load("./Data/frlAuditJuly24/fiji_frl_emission_factors.RData")
rmarkdown::render("./reports/Fiji_FRL_Report.Rmd", output_dir = "./reports", output_format = "html_document")