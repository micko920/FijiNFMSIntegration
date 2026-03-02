
all: run-Fiji-FRL run-create-reports-FRL run-Fiji-ER_EST run-create-reports-ER

run-Fiji-FRL:
	Rscript -e 'devtools::install("../ValueWithUncertainty")'
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_FRL_AccuracyAssessment.R")'
	Rscript -e 'source("./Fiji_FRL_NFIEmissionFactors.R")'
	Rscript -e 'source("./Fiji_FRL_Estimate_Values.R")'
	Rscript -e 'source("./Fiji_FRL_Tables.R")'
	Rscript -e 'source("./Fiji_FRL_Report.R")'
	cp Data/frlPostAuditOct25/fiji_frl_overall_years.RData Data/mrPostAuditOct25/


update-chks:
	cp ./chks/Fiji_FRL_Results_AccuracyAssessment.txt ./chks/Fiji_FRL_Results_AccuracyAssessment.chk
	cp ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt ./chks/Fiji_FRL_Results_NFIEmissionFactors.chk
	cp ./chks/Fiji_FRL_Results_Values.txt ./chks/Fiji_FRL_Results_Values.chk
	cp ./chks/Fiji_FRL_Results_Tables.txt ./chks/Fiji_FRL_Results_Tables.chk
	cp ./chks/Fiji_ER_EstimateResults_UC.txt ./chks/Fiji_ER_EstimateResults_UC.chk
	cp ./chks/Fiji_ER_EstimateResults_Sensitivity.txt ./chks/Fiji_ER_EstimateResults_Sensitivity.chk
	cp ./chks/Fiji_ER_EstimateResults_AccuracyAssessment.txt ./chks/Fiji_ER_EstimateResults_AccuracyAssessment.chk  
	cp ./chks/Fiji_ER_EstimateResults_Values.txt ./chks/Fiji_ER_EstimateResults_Values.chk


run-test-FRL:
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_FRL_AccuracyAssessment.R")' 2>&1 > ./chks/Fiji_FRL_Results_AccuracyAssessment.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_AccuracyAssessment.chk ./chks/Fiji_FRL_Results_AccuracyAssessment.txt
	Rscript -e 'source("./Fiji_FRL_NFIEmissionFactors.R")' 2>&1 > ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_NFIEmissionFactors.chk ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt
	Rscript -e 'source("./Fiji_FRL_Estimate_Values.R")' 2>&1 > ./chks/Fiji_FRL_Results_Values.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_Values.chk ./chks/Fiji_FRL_Results_Values.txt
	Rscript -e 'source("./Fiji_FRL_Tables.R")' 2>&1 > ./chks/Fiji_FRL_Results_Tables.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_Tables.chk ./chks/Fiji_FRL_Results_Tables.txt
	Rscript -e 'source("./Fiji_FRL_Report.R")' 2>&1 > ./chks/Fiji_FRL_Results_Report.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_Report.chk ./chks/Fiji_FRL_Results_Report.txt
	cp Data/frlPostAuditOct25/fiji_frl_overall_years.RData Data/mrPostAuditOct25/

run-Fiji-ER_aaboot:
	Rscript -e 'devtools::install("../ValueWithUncertainty")'
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_ER_Estimate_AccuracyAssessment.R")'


run-Fiji-ER_EST:
	Rscript -e 'devtools::install("../ValueWithUncertainty")'
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_ER_Estimate_AccuracyAssessment.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_Values.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_UC.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_Sensitivity.R")'
	
run-create-reports-ER:
	-rm -rf reports/*_cache
	-rm reports/Fiji_ER_*.html 
	Rscript -e 'source("./createReport_ER.R")'
	-rm -rf reports/*_cache


run-create-reports-FRL:
	-rm -rf reports/*_cache
	-rm reports/Fiji_FRL_Report.html 
	Rscript -e 'source("./createReport_FRL.R")'
	-rm -rf reports/*_cache


run-test-ER_EST:
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_ER_Estimate_AccuracyAssessment.R")'
	-diff -U 1  ./chks/Fiji_ER_EstimateResults_AccuracyAssessment.chk ./chks/Fiji_ER_EstimateResults_AccuracyAssessment.txt
	Rscript -e 'source("./Fiji_ER_Estimate_Values.R")'
	-diff -U 1  ./chks/Fiji_ER_EstimateResults_Values.chk ./chks/Fiji_ER_EstimateResults_Values.txt
	Rscript -e 'source("./Fiji_ER_Estimate_UC.R")'
	-diff -U 1  ./chks/Fiji_ER_EstimateResults_UC.chk ./chks/Fiji_ER_EstimateResults_UC.txt
	Rscript -e 'source("./Fiji_ER_Estimate_Sensitivity.R")'
	-diff -U 1  ./chks/Fiji_ER_EstimateResults_Sensitivity.chk ./chks/Fiji_ER_EstimateResults_Sensitivity.txt

run-install-requirements:
	Rscript -e 'source("./requirements.R")'
	
	
