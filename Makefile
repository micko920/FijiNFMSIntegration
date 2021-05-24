
all: run-Fiji-FRL run-FijiER_EST run-test

run-Fiji-FRL:
	Rscript -e 'source("./fiji_frl_all_R_code.R")'

run-Fiji-ER_EST:
	Rscript -e 'source("./Fiji_ER_Estimate_Values.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_UC.R")'
	Rscript -e 'source("./Fiji_ER_Estimate_Sensitivity.R")'

run-tests:
	Rscript -e 'source("./fiji_frl_all_R_code.R")'
	-diff -U 1  ./Fiji_FRL_Results.txt ./Fiji_FRL_Results.chk
	Rscript -e 'source("./Fiji_ER_Estimate_Values.R")'
	-diff -U 1  ./Fiji_ER_EstimateResults_Values.chk ./Fiji_ER_EstimateResults_Values.txt
	Rscript -e 'source("./Fiji_ER_Estimate_UC.R")'
	-diff -U 1  ./Fiji_ER_EstimateResults_UC.chk ./Fiji_ER_EstimateResults_UC.txt
	Rscript -e 'source("./Fiji_ER_Estimate_Sensitivity.R")'
	-diff -U 1  ./Fiji_ER_EstimateResults_Sensitivity.chk ./Fiji_ER_EstimateResults_Sensitivity.txt
	Rscript -e 'source("./run_tests.R")'

