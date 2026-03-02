


source("./Data/frlPostAuditOct25/FRL_Parameters.R")
load("./Data/frlPostAuditOct25/fiji_frl_tables.RData")
load("./Data/frlPostAuditOct25/fiji_frl_input.RData")
load("./Data/frlPostAuditOct25/fiji_frl_estimate_values.RData")
load("./Data/frlPostAuditOct25/fiji_frl_overall_years.RData")
load("./Data/frlPostAuditOct25/fiji_frl_adjusted_areas.RData")
load("./Data/frlPostAuditOct25/fiji_frl_emission_factors.RData")

rmarkdown::render(
  "./reports/Fiji_FRL_Report.Rmd",
  output_format = "html_document",
  output_file = paste0("../Data/frlPostAuditOct25/", "Fiji_FRL_Report", ".html"))

