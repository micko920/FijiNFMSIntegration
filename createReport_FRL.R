


source("./Data/frlAuditFeb24/FRL_Parameters.R")
load("./Data/frlAuditFeb24/fiji_frl_tables.RData")
load("./Data/frlAuditFeb24/fiji_frl_input.RData")
load("./Data/frlAuditFeb24/fiji_frl_estimate_values.RData")
load("./Data/frlAuditFeb24/fiji_frl_overall_years.RData")
rmarkdown::render("./reports/Fiji_FRL_Report.Rmd", output_dir = "./reports", output_format = "html_document")