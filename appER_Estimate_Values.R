library(shiny)
library(shinyjs)
library(dplyr)
library(kableExtra)
library(promises)
library(future)
library(htmltools)
library(shinyvalidate)




options(show.error.locations = TRUE)
pdf.options(paper = "a4r", reset = FALSE)
par(mfrow = c(2, 1))

debug_er <<- TRUE # Turn printed output on
show_output <<- TRUE # Turn final table printed output on
plot_mc_output <<- TRUE # Turn on plots for MC samples


## Change
source("./calcER_Estimate_Values.R")


######

ui <- fluidPage(
        theme = shinythemes::shinytheme("cerulean"),
        useShinyjs(),


        ############### Instructions, Consistent and visible on all pages ################


        ############ Main Panel #####################

        fluidRow(
                column(
                        6,
                        h1("Monitoring Period Values, Emmission and Removals")
                ),
                column(
                        2,
                        h5("Save"),
                        disabled(
                                downloadButton(
                                        "downloadData",
                                        "RData"
                                )
                        )
                ),
                column(
                        4,
                        h5("Export"),
                        radioButtons("format", NULL, c("PDF", "HTML"),
                                inline = TRUE
                        ),
                        disabled(
                                downloadButton(
                                        "downloadReport",
                                        "Tables"
                                )
                        )
                )
        ),
        tabsetPanel(
                id = "tabs",

                ############## Welcome Page ##########################


                ########### Data Input Page ##################

                tabPanel(
                        "DataUpload",
                        h3("AdjustedAreas From Accuracy Assessment"),
                        fileInput(
                                "PreviousData",
                                  "Adjusted Areas From Accuracy Assessment",
                                multiple = FALSE,
                                accept = ".RData"
                        ),
                        tableOutput("rdataNames"),
                        h3("Reload previous monitored values"),
                        fileInput(
                                "PreviousMV",
                                "Previous Monitored Values",
                                multiple = FALSE,
                                accept = ".RData"
                        ),
                        h3("Monitored Values"),
                        p("Year 1"),
                        numericInput(
                                "year1year", "Year1 Year",
                                0 # default 2019
                        ),
                        numericInput(
                                "year1AReforSurveyArea",
                                "Year1 Afforestation Survey Area",
                                0 # default 2800
                        ),
                        numericInput(
                                "year1FPlnVolHarvHwd",
                                "Year1 Hardwood Harvested Volume",
                                0 # default 19801.647
                        ),
                        numericInput(
                                "year1FPlnAreaStockHwd",
                                "Year1 Opening Stocked Area Hardwood",
                                0 # default 40909.4
                        ),
                        numericInput(
                                "year1FPlnAreaPlantHwd", "Year1 Hardwood Planted Area",
                                0 # default 4007.9
                        ),
                        numericInput(
                                "year1FPlnAreaHarvHwd", "Year1 Hardwood Harvested Area",
                                0 # default 103.84
                        ),
                        numericInput(
                                "year1FPlnVolHarvSwd",
                                "Year1 Softwood Harvested Volume",
                                0 # default 386985
                        ),
                        numericInput(
                                "year1FPlnAreaStockSwd",
                                "Year1 Opening Stocked Area Softwood",
                                0 # default 24698
                        ),
                        numericInput(
                                "year1FPlnAreaPlantSwd", "Year1 Softwood Planted Area",
                                0 # default 2008
                        ),
                        numericInput(
                                "year1FPlnAreaHarvSwd", "Year1 Softwood Harvested Area",
                                0 # default 909
                        ),
                        numericInput(
                                "year1FDegFellVol", "Year1 Native Harvested Volume",
                                0 # default 27582.754
                        ),
                        numericInput(
                                "year1FDegFellArea", "Year1 Native Felled Area",
                                0 # default 1349.88
                        ),
                        p("Import Burn Data from text file"),
                        fileInput("BurnDataYear1", "Import Burn Data for Year1"),
                        hr(),
                        p("Year 2"),
                        numericInput(
                                "year2year", "Year2 Year",
                                0 # default 2020
                        ),
                        numericInput(
                                "year2AReforSurveyArea",
                                "Year2 Afforestation Survey Area",
                                0 # default 2800
                        ),
                        numericInput(
                                "year2FPlnVolHarvHwd",
                                "Year2 Hardwood Harvested Volume",
                                0 # default 21441.157
                        ),
                        numericInput(
                                "year2FPlnAreaStockHwd",
                                "Year2 Opening Stocked Area Hardwood",
                                0 # default 44813.46
                        ),
                        numericInput(
                                "year2FPlnAreaPlantHwd", "Year2 Hardwood Planted Area",
                                0 # default 0
                        ),
                        numericInput(
                                "year2FPlnAreaHarvHwd", "Year2 Hardwood Harvested Area",
                                0 # default 142.94
                        ),
                        numericInput(
                                "year2FPlnVolHarvSwd",
                                "Year2 Softwood Harvested Volume",
                                0 # default 479959
                        ),
                        numericInput(
                                "year2FPlnAreaStockSwd",
                                "Year2 Opening Stocked Area Softwood",
                                0 # default 26094
                        ),
                        numericInput(
                                "year2FPlnAreaPlantSwd", "Year2 Softwood Planted Area",
                                0 # default 1910
                        ),
                        numericInput(
                                "year2FPlnAreaHarvSwd", "Year2 Softwood Harvested Area",
                                0 # default 1377
                        ),
                        numericInput(
                                "year2FDegFellVol", "Year2 Native Harvested Volume",
                                0 # default 22088.296
                        ),
                        numericInput(
                                "year2FDegFellArea", "Year2 Native Felled Area",
                                0 # default 1082.63
                        ),
                        p("Import Burn Data from text file"),
                        fileInput("BurnDataYear2", "Import Burn Data for Year 2"),
                        br(),

                        h3("Reload previous report params"),
                        fileInput(
                                "PreviousMRP",
                                "Previous Report Params",
                                multiple = FALSE,
                                accept = ".RData"
                        ),
                        # Monitoring Report Params
                        h3("Monitoring Report Params"),
                        numericInput(
                                "mrpMpDays", "Monitoring Period Days",
                                0 # default 730
                        ),
                        numericInput(
                                "mrpRpDays", "Reporting Period Days",
                                0 # default 540
                        ),
                        numericInput(
                                "mrpErpaYearlyFRL", "Forest Reference Level",
                                0 # default values_FRL
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLUCI",
                                "Forest Reference Level UCI",
                                0 # default values_FRL
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLLCI",
                                "Forest Reference Level LCI",
                                0 # default values_FRL
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLDefor",
                                "FRL Deforestation",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLDeforUCI",
                                "FRL Deforestation UCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLDeforLCI",
                                "FRL Deforestation LCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDeg",
                                "FRL Forest Degradation",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDegUCI",
                                "FRL Forest Degradation UCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDegLCI",
                                "FRL Forest Degradation LCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDegNonProxy",
                                "FRL Forest Degradation Non Proxy",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDegNonProxyUCI",
                                "FRL Forest Degradation Non Proxy UCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLFDegNonProxyLCI",
                                "FRL Forest Degradation Non Proxy LCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLEnh",
                                "FRL Removals by Sinks",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLEnhUCI",
                                "FRL Removals by Sinks UCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaYearlyFRLEnhLCI",
                                "FRL Removals by Sinks LCI",
                                0
                        ),
                        numericInput(
                                "mrpErpaTransferredERs", "Transferred Emission Reductions",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpErpaContestedERs", "Contested Emission Reductions",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpErpaSoldERs", "Sold Emission Reductions",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpErpaRiskSetaside", "Risk Set Aside",
                                0 # default 0.16
                        ),
                        numericInput(
                                "mrpErpaPreviousFRL", "Cumulative Previous Reference Level",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpErpaPreviousEmRems",
                                "Cumulative Previous Emissions and Removals",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpErpaPreviousERs",
                                "Cumulative Previous Emission Reductions",
                                0 # default 0
                        ),
                        numericInput(
                                "mrpFDegUncertaintyDiscount",
                                "Proxy Degradation Uncertainty Discount",
                                0 # default 0.15
                        ),
                        hr(),
                        h3("Next"),
                        disabled(
                                actionButton("ProceedtoRunPage", "Proceed to Run Calculations")
                        ),
                        textOutput("reviewvaluesvalid"),
                        textOutput("reviewvaluesinvalid"),
                        br(),
                ),


                ##### Calculate Page ####
                tabPanel(
                        "Calculate",
                        h3("Calculate"),
                        br(),
                        actionButton("run", "Run"),
                        actionButton("cancel", "Cancel"),
                        actionButton("status", "Check Status"),
                        hr(),
                        h3("Results"),
                        tableOutput("Table4_2"),
                        hr(),
                        tableOutput("Table4_3")
                )
                ##### Page End ####
        )
)

##### Server
server <- function(input, output, session) {
        hideTab(inputId = "tabs", target = "Calculate")


        ###### Import Previous Data Set #################

        sessionData <- reactiveValues()
        LoadToEnvironment <-
                function(RData, localEnv = new.env()) {
                        load(RData, localEnv)
                        return(localEnv)
                }

        observeEvent(input$PreviousData$datapath, {
                if (!is.null(input$PreviousData$datapath)) {
                        # Use a reactiveFileReader to read the file on change, and load the content into a new environment
                        rdataEnv <-
                                reactiveFileReader(
                                        1000,
                                        session,
                                        input$PreviousData$datapath,
                                        LoadToEnvironment
                                )

                        # Convert the env into a list to send to the calc Function
                        sessionData$calcEnv <- as.list(rdataEnv())

                        # What names are in the file.
                        sessionData$rdataNames <-
                                data.frame(rdataNames = names(rdataEnv()))
                        colnames(sessionData$rdataNames) <-
                                c("RData content names")

                        output$rdataNames <- renderTable({
                                sessionData$rdataNames
                        })
                        enable("ProceedtoRunPage")
                }
        })

        observeEvent(input$PreviousMV$datapath, {
                if (!is.null(input$PreviousMV$datapath)) {
                        # Use a reactiveFileReader to read the file on change, and load the content into a new environment
                        pmvdataEnv <-
                                reactiveFileReader(
                                        1000,
                                        session,
                                        input$PreviousMV$datapath,
                                        LoadToEnvironment
                                )
                        pmvdata <- as.list(pmvdataEnv())
                        updateTextInput(session, "year1year", value = pmvdata$MonitoredValues$year1$year)
                        updateTextInput(session, "year1AReforSurveyArea", value = pmvdata$MonitoredValues$year1$AReforSurveyArea)
                        updateTextInput(session, "year1FPlnVolHarvHwd", value = pmvdata$MonitoredValues$year1$FPlnVolHarvHwd)
                        updateTextInput(session, "year1FPlnAreaStockHwd", value = pmvdata$MonitoredValues$year1$FPlnAreaStockHwd)
                        updateTextInput(session, "year1FPlnAreaPlantHwd", value = pmvdata$MonitoredValues$year1$FPlnAreaPlantHwd)
                        updateTextInput(session, "year1FPlnAreaHarvHwd", value = pmvdata$MonitoredValues$year1$FPlnAreaHarvHwd)
                        updateTextInput(session, "year1FPlnVolHarvSwd", value = pmvdata$MonitoredValues$year1$FPlnVolHarvSwd)
                        updateTextInput(session, "year1FPlnAreaStockSwd", value = pmvdata$MonitoredValues$year1$FPlnAreaStockSwd)
                        updateTextInput(session, "year1FPlnAreaPlantSwd", value = pmvdata$MonitoredValues$year1$FPlnAreaPlantSwd)
                        updateTextInput(session, "year1FPlnAreaHarvSwd", value = pmvdata$MonitoredValues$year1$FPlnAreaHarvSwd)
                        updateTextInput(session, "year1FDegFellVol", value = pmvdata$MonitoredValues$year1$FDegFellVol)
                        updateTextInput(session, "year1FDegFellArea", value = pmvdata$MonitoredValues$year1$FDegFellArea)

                        updateTextInput(session, "year2year", value = pmvdata$MonitoredValues$year2$year)
                        updateTextInput(session, "year2AReforSurveyArea", value = pmvdata$MonitoredValues$year2$AReforSurveyArea)
                        updateTextInput(session, "year2FPlnVolHarvHwd", value = pmvdata$MonitoredValues$year2$FPlnVolHarvHwd)
                        updateTextInput(session, "year2FPlnAreaStockHwd", value = pmvdata$MonitoredValues$year2$FPlnAreaStockHwd)
                        updateTextInput(session, "year2FPlnAreaPlantHwd", value = pmvdata$MonitoredValues$year2$FPlnAreaPlantHwd)
                        updateTextInput(session, "year2FPlnAreaHarvHwd", value = pmvdata$MonitoredValues$year2$FPlnAreaHarvHwd)
                        updateTextInput(session, "year2FPlnVolHarvSwd", value = pmvdata$MonitoredValues$year2$FPlnVolHarvSwd)
                        updateTextInput(session, "year2FPlnAreaStockSwd", value = pmvdata$MonitoredValues$year2$FPlnAreaStockSwd)
                        updateTextInput(session, "year2FPlnAreaPlantSwd", value = pmvdata$MonitoredValues$year2$FPlnAreaPlantSwd)
                        updateTextInput(session, "year2FPlnAreaHarvSwd", value = pmvdata$MonitoredValues$year2$FPlnAreaHarvSwd)
                        updateTextInput(session, "year2FDegFellVol", value = pmvdata$MonitoredValues$year2$FDegFellVol)
                        updateTextInput(session, "year2FDegFellArea", value = pmvdata$MonitoredValues$year2$FDegFellArea)
                }
        })

        observeEvent(input$PreviousMRP$datapath, {
                if (!is.null(input$PreviousMRP$datapath)) {
                        # Use a reactiveFileReader to read the file on change, and load the content into a new environment
                        pmrpdataEnv <-
                                reactiveFileReader(
                                        1000,
                                        session,
                                        input$PreviousMV$datapath,
                                        LoadToEnvironment
                                )
                        pmrpdata <- as.list(pmrpdataEnv())$MonitoringReportParams

                        updateTextInput(session, "mrpMpDays", value = pmrpdata$MpDays)
                        updateTextInput(session, "mrpRpDays", value = pmrpdata$RpDays)
                        updateTextInput(session, "mrpErpaYearlyFRL", value = pmrpdata$ErpaYearlyFRL)
                        updateTextInput(session, "mrpErpaYearlyFRLUCI", value = pmrpdata$ErpaYearlyFRLUCI)
                        updateTextInput(session, "mrpErpaYearlyFRLLCI", value = pmrpdata$ErpaYearlyFRLLCI)
                        updateTextInput(session, "mrpErpaYearlyFRLDefor", value = pmrpdata$ErpaYearlyFRLDefor)
                        updateTextInput(session, "mrpErpaYearlyFRLDeforUCI", value = pmrpdata$ErpaYearlyFRLDeforUCI)
                        updateTextInput(session, "mrpErpaYearlyFRLDeforLCI", value = pmrpdata$ErpaYearlyFRLDeforLCI)
                        updateTextInput(session, "mrpErpaYearlyFRLFDeg", value = pmrpdata$ErpaYearlyFRLFDeg)
                        updateTextInput(session, "mrpErpaYearlyFRLFDegUCI", value = pmrpdata$ErpaYearlyFRLFDegUCI)
                        updateTextInput(session, "mrpErpaYearlyFRLFDegLCI", value = pmrpdata$ErpaYearlyFRLFDegLCI)
                        updateTextInput(session, "mrpErpaYearlyFRLFDegNonProxy", value = pmrpdata$ErpaYearlyFRLFDegNonProxy)
                        updateTextInput(session, "mrpErpaYearlyFRLFDegNonProxyUCI", value = pmrpdata$ErpaYearlyFRLFDegNonProxyUCI)
                        updateTextInput(session, "mrpErpaYearlyFRLFDegNonProxyLCI", value = pmrpdata$ErpaYearlyFRLFDegNonProxyLCI)
                        updateTextInput(session, "mrpErpaYearlyFRLEnh", value = pmrpdata$ErpaYearlyFRLEnh)
                        updateTextInput(session, "mrpErpaYearlyFRLEnhUCI", value = pmrpdata$ErpaYearlyFRLEnhUCI)
                        updateTextInput(session, "mrpErpaYearlyFRLEnhLCI", value = pmrpdata$ErpaYearlyFRLEnhLCI)
                        updateTextInput(session, "mrpErpaTransferredERs", value = pmrpdata$ErpaTransferredERs)
                        updateTextInput(session, "mrpErpaContestedERs", value = pmrpdata$ErpaContestedERs)
                        updateTextInput(session, "mrpErpaSoldERs", value = pmrpdata$ErpaSoldERs)
                        updateTextInput(session, "mrpErpaRiskSetaside", value = pmrpdata$ErpaRiskSetaside)
                        updateTextInput(session, "mrpErpaPreviousFRL", value = pmrpdata$ErpaPreviousFRL)
                        updateTextInput(session, "mrpErpaPreviousEmRems", value = pmrpdata$ErpaPreviousEmRems)
                        updateTextInput(session, "mrpErpaPreviousERs", value = pmrpdata$ErpaPreviousERs)
                        updateTextInput(session, "mrpFDegUncertaintyDiscount", value = pmrpdata$FDegUncertaintyDiscount)
                }
        })

        calcEnv <- reactive({
                return(sessionData$calcEnv)
        })


        ########## Import Burn Data ############

        Y1BD <- reactive({
                data <- input$BurnDataYear1
                tbl <- read.table(data$datapath, header = T)[, c("year", "area_ha", "age_yrs")]
                return(tbl)
        })

        Y2BD <- reactive({
                data <- input$BurnDataYear2
                tbl <- read.table(data$datapath, header = T)[, c("year", "area_ha", "age_yrs")]
                return(tbl)
        })

        ########### Input Monitored Values or Monitoring Report Parameters ###

        # Save numeric Inputs into a Reactive List so they can be displayed
        MonitoredValues <- reactive({
                mv <- list()
                mv$year1$year <- input$year1year
                # including these in this function causes the output table to have duplicate rows for each other other mv variables - tried to split into separate lists but can't combine them
                mv$year1$FDegBurnData <- Y1BD()
                mv$year1$AReforSurveyArea <-
                        input$year1AReforSurveyArea
                mv$year1$FPlnVolHarvHwd <-
                        input$year1FPlnVolHarvHwd
                mv$year1$FPlnAreaStockHwd <-
                        input$year1FPlnAreaStockHwd
                mv$year1$FPlnAreaPlantHwd <-
                        input$year1FPlnAreaPlantHwd
                mv$year1$FPlnAreaHarvHwd <-
                        input$year1FPlnAreaHarvHwd
                mv$year1$FPlnAreaJustGrowsHwd <-
                        input$year1FPlnAreaStockHwd - input$year1FPlnAreaHarvHwd
                mv$year1$FPlnVolHarvSwd <-
                        input$year1FPlnVolHarvSwd
                mv$year1$FPlnAreaStockSwd <-
                        input$year1FPlnAreaStockSwd
                mv$year1$FPlnAreaPlantSwd <-
                        input$year1FPlnAreaPlantSwd
                mv$year1$FPlnAreaHarvSwd <-
                        input$year1FPlnAreaHarvSwd
                mv$year1$FPlnAreaJustGrowsSwd <-
                        input$year1FPlnAreaStockSwd - input$year1FPlnAreaHarvSwd
                mv$year1$FDegFellVol <- input$year1FDegFellVol
                mv$year1$FDegFellArea <- input$year1FDegFellArea

                # Year2

                mv$year2$year <- input$year2year
                mv$year2$FDegBurnData <- Y2BD()
                mv$year2$AReforSurveyArea <-
                        input$year2AReforSurveyArea
                mv$year2$FPlnVolHarvHwd <-
                        input$year2FPlnVolHarvHwd
                mv$year2$FPlnAreaStockHwd <-
                        input$year2FPlnAreaStockHwd
                mv$year2$FPlnAreaPlantHwd <-
                        input$year2FPlnAreaPlantHwd
                mv$year2$FPlnAreaHarvHwd <-
                        input$year2FPlnAreaHarvHwd
                mv$year2$FPlnAreaJustGrowsHwd <-
                        input$year2FPlnAreaStockHwd - input$year2FPlnAreaHarvHwd
                mv$year2$FPlnVolHarvSwd <-
                        input$year2FPlnVolHarvSwd
                mv$year2$FPlnAreaStockSwd <-
                        input$year2FPlnAreaStockSwd
                mv$year2$FPlnAreaPlantSwd <-
                        input$year2FPlnAreaPlantSwd
                mv$year2$FPlnAreaHarvSwd <-
                        input$year2FPlnAreaHarvSwd
                mv$year2$FPlnAreaJustGrowsSwd <-
                        input$year2FPlnAreaStockSwd - input$year2FPlnAreaHarvSwd
                mv$year2$FDegFellVol <- input$year2FDegFellVol
                mv$year2$FDegFellArea <- input$year2FDegFellArea

                return(mv)
        })

        MonitoringReportParams <- reactive({
                mrp <- list()

                mrp$MpDays <- input$mrpMpDays
                mrp$RpDays <- input$mrpRpDays
                mrp$IsRpEqualToMp <-
                        (input$mrpMpDays == input$mrpRpDays)
                mrp$RpMpRatio <-
                        (input$mrpRpDays / input$mrpMpDays)
                mrp$RpMpProrataYears <- 2 * mrp$RpMpRatio
                mrp$ErpaYearlyFRL <- input$mrpErpaYearlyFRL
                mrp$ErpaYearlyFRLUCI <- input$mrpErpaYearlyFRLUCI
                mrp$ErpaYearlyFRLLCI <- input$mrpErpaYearlyFRLLCI
                mrp$ErpaYearlyFRLDefor <- input$mrpErpaYearlyFRLDefor
                mrp$ErpaYearlyFRLDeforUCI <- input$mrpErpaYearlyFRLDeforUCI
                mrp$ErpaYearlyFRLDeforLCI <- input$mrpErpaYearlyFRLDeforLCI
                mrp$ErpaYearlyFRLFDeg <- input$mrpErpaYearlyFRLFDeg
                mrp$ErpaYearlyFRLFDegUCI <- input$mrpErpaYearlyFRLFDegUCI
                mrp$ErpaYearlyFRLFDegLCI <- input$mrpErpaYearlyFRLFDegLCI
                mrp$ErpaYearlyFRLFDegNonProxy <- input$mrpErpaYearlyFRLFDegNonProxy
                mrp$ErpaYearlyFRLFDegNonProxyUCI <- input$mrpErpaYearlyFRLFDegNonProxyUCI
                mrp$ErpaYearlyFRLFDegNonProxyLCI <- input$mrpErpaYearlyFRLFDegNonProxyLCI
                mrp$ErpaYearlyFRLEnh <- input$mrpErpaYearlyFRLEnh
                mrp$ErpaYearlyFRLEnhUCI <- input$mrpErpaYearlyFRLEnhUCI
                mrp$ErpaYearlyFRLEnhLCI <- input$mrpErpaYearlyFRLEnhLCI
                mrp$ErpaTransferredERs <-
                        input$mrpErpaTransferredERs
                mrp$ErpaContestedERs <-
                        input$mrpErpaContestedERs
                mrp$ErpaSoldERs <- input$mrpErpaSoldERs
                mrp$ErpaRiskSetaside <-
                        input$mrpErpaRiskSetaside
                mrp$ErpaPreviousFRL <- input$mrpErpaPreviousFRL
                mrp$ErpaPreviousEmRems <-
                        input$mrpErpaPreviousEmRems
                mrp$ErpaPreviousERs <- input$mrpErpaPreviousERs
                mrp$FDegUncertaintyDiscount <-
                        input$mrpFDegUncertaintyDiscount

                return(mrp)
        })

        ############ Validation ################


        iv <- InputValidator$new()
        # 2. Add validation rules
        iv$add_rule("PreviousData", sv_required())

        iv$add_rule("BurnDataYear1", sv_required())
        iv$add_rule("BurnDataYear2", sv_required())
        iv$add_rule("year1year", sv_required())
        iv$add_rule("year1AReforSurveyArea", sv_required())
        iv$add_rule("year1FPlnVolHarvHwd", sv_required())
        iv$add_rule("year1FPlnAreaStockHwd", sv_required())
        iv$add_rule("year1FPlnAreaPlantHwd", sv_required())
        iv$add_rule("year1FPlnAreaHarvHwd", sv_required())

        iv$add_rule("year1FPlnVolHarvSwd", sv_required())
        iv$add_rule("year1FPlnAreaStockSwd", sv_required())
        iv$add_rule("year1FPlnAreaPlantSwd", sv_required())
        iv$add_rule("year1FPlnAreaHarvSwd", sv_required())
        iv$add_rule("year1FDegFellVol", sv_required())
        iv$add_rule("year1FDegFellArea", sv_required())
        iv$add_rule("year2AReforSurveyArea", sv_required())
        iv$add_rule("year2FPlnVolHarvHwd", sv_required())
        iv$add_rule("year2FPlnAreaStockHwd", sv_required())
        iv$add_rule("year2FPlnAreaPlantHwd", sv_required())
        iv$add_rule("year2FPlnAreaHarvHwd", sv_required())
        iv$add_rule("year2FPlnVolHarvSwd", sv_required())
        iv$add_rule("year2FPlnAreaStockSwd", sv_required())
        iv$add_rule("year2FPlnAreaPlantSwd", sv_required())
        iv$add_rule("year2FPlnAreaHarvSwd", sv_required())
        iv$add_rule("year2FDegFellVol", sv_required())
        iv$add_rule("year2FDegFellArea", sv_required())
        iv$add_rule("mrpMpDays", sv_required())
        iv$add_rule("mrpRpDays", sv_required())
        iv$add_rule("mrpErpaYearlyFRL", sv_required())
        iv$add_rule("mrpErpaYearlyFRLUCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLLCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLDefor", sv_required())
        iv$add_rule("mrpErpaYearlyFRLDeforUCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLDeforLCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDeg", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDegUCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDegLCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDegNonProxy", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDegNonProxyUCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLFDegNonProxyLCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLEnh", sv_required())
        iv$add_rule("mrpErpaYearlyFRLEnhUCI", sv_required())
        iv$add_rule("mrpErpaYearlyFRLEnhLCI", sv_required())
        iv$add_rule("mrpErpaTransferredERs", sv_required())
        iv$add_rule("mrpErpaContestedERs", sv_required())
        iv$add_rule("mrpErpaSoldERs", sv_required())
        iv$add_rule("mrpErpaRiskSetaside", sv_required())
        iv$add_rule("mrpErpaPreviousFRL", sv_required())
        iv$add_rule("mrpErpaPreviousEmRems", sv_required())
        iv$add_rule("mrpErpaPreviousERs", sv_required())
        iv$add_rule("mrpFDegUncertaintyDiscount", sv_required())


        # 3. Start displaying errors in the UI
        iv$enable()

        # Enable proceed if validation met
        output$reviewvaluesvalid <- renderText({
                req(iv$is_valid())
                enable("ProceedtoRunPage")
                paste0("")
        })

        # Error Message if validation not met.
        output$reviewvaluesinvalid <- renderText({
                req(!iv$is_valid())
                disable("ProceedtoRunPage")
                paste0("All inputs are required before you can proceed.")
        })



        ############ Proceed to the Run Page #############

        observeEvent(input$ProceedtoRunPage, {
                hideTab(inputId = "tabs", target = "DataUpload")
                showTab(inputId = "tabs", target = "Calculate")
                updateTabsetPanel(session, "tabs", selected = "Calculate")
        })

        ############# Calculation #################################

        # Status File
        status_file <- tempfile()

        get_status <- function() {
                scan(status_file, what = "character", sep = "\n")
        }

        set_status <- function(msg) {
                write(msg, status_file)
        }

        fire_interrupt <- function() {
                set_status("interrupt")
        }

        fire_ready <- function() {
                set_status("Ready")
        }

        fire_running <-
                function(perc_complete, notification) {
                        if (missing(notification)) {
                                msg <- "Running ...."
                        } else {
                                msg <- notification
                        }
                        if (!missing(perc_complete)) {
                                msg <-
                                        paste0(msg, " [", perc_complete, "% Complete]")
                        }
                        set_status(msg)
                }

        interrupted <- function() {
                get_status() == "interrupt"
        }

        # Delete file at end of session
        onStop(function() {
                print(status_file)
                if (file.exists(status_file)) {
                        unlink(status_file)
                }
        })

        # Create Status File
        fire_ready()

        nclicks <- reactiveVal(0)
        result_val <- reactiveVal()

        observeEvent(input$run, {
                # Don't do anything if analysis is already being run
                if (nclicks() != 0) {
                        showNotification("Already running analysis")
                        return(NULL)
                }

                if ("env" %in% names(result_val())) {
                        result_val(NULL)
                        disable("downloadReport")
                        disable("downloadData")
                }


                # Increment clicks and prevent concurrent analyses
                nclicks(nclicks() + 1)

                result_val(tagList(div("Running....")))

                fire_running()

                # Put all inputs into one list to get passed into function
                calcEnv <- calcEnv()
                calcEnv$MonitoredValues <- MonitoredValues()
                calcEnv$MonitoringReportParams <-
                        MonitoringReportParams()

                result <- future({
                        r <- CalcER_Estimate_Values(fire_running, interrupted, calcEnv)
                        enable("downloadReport")
                        enable("downloadData")
                        return(r)
                }) %...>% result_val()

                # Catch inturrupt (or any other error) and notify user
                result <- catch(
                        result,
                        function(e) {
                                result_val(NULL)
                                print(e$message)
                                showNotification(e$message)
                        }
                )

                # After the promise has been evaluated set nclicks to 0 to allow for another Run
                result <- finally(
                        result,
                        function() {
                                fire_ready()
                                nclicks(0)
                        }
                )

                # Return something other than the promise so shiny remains responsive
                NULL
        })

        # Register user interrupt
        observeEvent(input$cancel, {
                print("Cancel")
                fire_interrupt()
        })

        # Let user get analysis progress
        observeEvent(input$status, {
                print("Status")
                showNotification(get_status())
        })

        ################## Outputs to Display on Screen ###########

        output$Table4_2 <- function() {
                if (!is.null(result_val()$env)) {
                        return(
                                result_val()$env$Table4_2 %>%
                                        kable("html", caption = "Monitoring Report Table 4.2") %>%
                                        kable_styling(
                                                bootstrap_options = c(
                                                        "striped",
                                                        "condensed",
                                                        "hover",
                                                        full_width = F,
                                                        position = "left"
                                                )
                                        )
                        )
                } else {
                        return(div(""))
                }
        }

        output$Table4_3 <- function() {
                if (!is.null(result_val()$env)) {
                        return(
                                result_val()$env$Table4_3 %>%
                                        kable("html", caption = "Monitoring Report Table 4.3") %>%
                                        kable_styling(
                                                bootstrap_options = c(
                                                        "striped",
                                                        "condensed",
                                                        "hover",
                                                        full_width = F,
                                                        position = "left"
                                                )
                                        )
                        )
                } else {
                        return(div(""))
                }
        }



        ######## Download Data #############################
        output$downloadReport <- downloadHandler(
                filename = function() {
                        fname <- paste(outputFilename, sep = ".", switch(input$format,
                                PDF = "pdf",
                                HTML = "html"
                        ))
                        return(fname)
                },
                content = function(file) {
                        list2env(result_val()$env, environment())
                        fileFormat <- switch(input$format,
                                PDF = "latex",
                                HTML = "html"
                        )
                        reportName <- paste(outputFilename, "Rmd", sep = ".")
                        src <- normalizePath(paste("reports", reportName, sep = "/"))

                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, reportName, overwrite = TRUE)
                        library(rmarkdown)
                        out <- render(reportName, switch(input$format,
                                PDF = pdf_document(),
                                HTML = html_document()
                        ))

                        file.rename(out, file)
                }
        )


        output$downloadData <- downloadHandler(
                filename = function() {
                        return(paste(outputFilename, "RData", sep = "."))
                },
                content = function(file) {
                        list2env(result_val()$env, environment())

                        save(
                                list = outputSaveNames,
                                file = file
                        )
                }
        )
}
shinyApp(ui = ui, server = server)
