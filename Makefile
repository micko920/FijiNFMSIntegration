
all: run-Fiji-FRL run-FijiER_EST run-test

run-Fiji-FRL:
	Rscript -e 'source("./Fiji_FRL_AccuracyAssessment.R")'
	Rscript -e 'source("./Fiji_FRL_NFIEmissionFactors.R")'
	Rscript -e 'source("./Fiji_FRL_Estimate_Values.R")'
	Rscript -e 'source("./Fiji_FRL_Tables.R")'
	Rscript -e 'source("./Fiji_FRL_Report.R")'


update-chks:
	cp ./chks/Fiji_FRL_Results_AccuracyAssessment.txt ./chks/Fiji_FRL_Results_AccuracyAssessment.chk
	cp ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt ./chks/Fiji_FRL_Results_NFIEmissionFactors.chk
	cp ./chks/Fiji_FRL_Results_Values.txt ./chks/Fiji_FRL_Results_Values.chk
	cp ./chks/Fiji_FRL_Results_Tables.txt ./chks/Fiji_FRL_Results_Tables.chk
	cp ./chks/Fiji_ER_EstimateResults_UC.txt ./chks/Fiji_ER_EstimateResults_UC.chk
	cp ./chks/Fiji_ER_EstimateResults_Sensitivity.txt ./chks/Fiji_ER_EstimateResults_Sensitivity.chk


run-test-FRL:
	Rscript -e 'source("./Fiji_FRL_AccuracyAssessment.R")' 2>&1 > ./chks/Fiji_FRL_Results_AccuracyAssessment.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_AccuracyAssessment.chk ./chks/Fiji_FRL_Results_AccuracyAssessment.txt
	Rscript -e 'source("./Fiji_FRL_NFIEmissionFactors.R")' 2>&1 > ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_NFIEmissionFactors.chk ./chks/Fiji_FRL_Results_NFIEmissionFactors.txt
	Rscript -e 'source("./Fiji_FRL_Estimate_Values.R")' 2>&1 > ./chks/Fiji_FRL_Results_Values.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_Values.chk ./chks/Fiji_FRL_Results_Values.txt
	Rscript -e 'source("./Fiji_FRL_Tables.R")' 2>&1 > ./chks/Fiji_FRL_Results_Tables.txt
	-diff -U 1  ./chks/Fiji_FRL_Results_Tables.chk ./chks/Fiji_FRL_Results_Tables.txt

run-Fiji-ER_EST:
	Rscript -e 'devtools::install("../ValueWithUncertainty")'
	Rscript -e 'devtools::install("../FijiNFMSCalculations")'
	Rscript -e 'source("./Fiji_ER_Estimate_AccuracyAssessment.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_Values.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_UC.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_Sensitivity.R")'

run-create-reports:
	-rm -rf reports/*_cache
	-rm \
		reports/Fiji_FRL_Report.html \
		reports/Fiji_ER_Report.html \
		reports/Fiji_ER_Estimate_UC.html \
		reports/Fiji_ER_Estimate_Values.html \
		reports/Fiji_ER_Estimate_AccuracyAssesment.html \
		reports/Fiji_MonitoringReportExtraTables.html
	Rscript -e 'source("./createReport_FRL.R")'
	Rscript -e 'source("./createReport_ER.R")'


run-test-ER_EST:
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
	
	
