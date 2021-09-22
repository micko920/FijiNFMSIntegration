library(shiny)
library(shinyjs)
library(dplyr)
library(kableExtra)
library(promises)
library(future)
library(htmltools)
library(shinyvalidate)

## Change
source("./calcER_Estimate_Values.R")

outputFilename <-"Fiji_ER_Estimate_Values.RData"
outputSaveNames <- c(
"Table4_2",
"Table4_3",
"MonitoredValues",
"MonitoringReportParams",
"EmRems_Values",
"ER_Values",
# "UncertaintyParams",
"AdjustedAreas")



######

ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  useShinyjs(),
  
  
  ############### Instructions, Consistent and visible on all pages ################    
  
  
  ############ Main Panel #####################    
  
  mainPanel(
    h1("ER Estimate Values"),
    tabsetPanel(
    id = "tabs",
    
    ##############Welcome Page ##########################
    
    
    ########### Data Input Page ##################
    
    tabPanel(
      "DataUpload",
      h3("AdjustedAreas From Accuracy Assessment"),
      fileInput(
        "PreviousData",
        "Adjusted Areas From Accuracy Assessment",
        multiple = FALSE,
        accept = '.Rdata'
      ),
      
     tableOutput('rdataNames'),
      
      h3("Monitored Values"),
      p("Year 1"),
      
      numericInput("year1year","Year1 Year", 2018),
      numericInput("year1FPlnVolHarvHwd","Year1 FPlnVolHarvHwd",62199.6),
      numericInput("year1FPlnAreaStockHwd","Year1 FPlnAreaStockHwd",56950.5),
      numericInput("year1FPlnAreaPlantHwd","Year1 FPlnAreaPlantHwd",3050.30),
      numericInput("year1FPlnAreaHarvHwd","Year1 FPlnAreaHarvHwd",3316.60),
      
      numericInput("year1FPlnVolHarvSwd","Year1 FPlnVolHarvSwd",334463),
      numericInput("year1FPlnAreaStockSwd","Year1 FPlnAreaStockSwd",49106.0),
      numericInput("year1FPlnAreaPlantSwd","Year1 FPlnAreaPlantSwd",370.820),
      numericInput("year1FPlnAreaHarvSwd","Year1 FPlnAreaHarvSwd",1282.40),
      
      numericInput("year1FDegFellVol","Year1 FDegFellVol",50731.5),
      numericInput("year1FDegFellArea","Year1 FDegFellArea",11669.9),
      
      p("Import Burn Data from text file"),
      fileInput("BurnDataYear1","Import Burn Data for Year1"),
      
      
      hr(),
      
      p("Year 2"),
      
      numericInput("year2year","Year2 Year",2019),
      numericInput("year2FPlnVolHarvHwd","Year2 FPlnVolHarvHwd",62199.6),
      numericInput("year2FPlnAreaStockHwd","Year2 FPlnAreaStockHwd",56950.5),
      numericInput("year2FPlnAreaPlantHwd","Year2 FPlnAreaPlantHwd",3050.30),
      numericInput("year2FPlnAreaHarvHwd","Year2 FPlnAreaHarvHwd",3316.60),
      
      numericInput("year2FPlnVolHarvSwd","Year2 FPlnVolHarvSwd",334463),
      numericInput("year2FPlnAreaStockSwd","Year2 FPlnAreaStockSwd",49106.0),
      numericInput("year2FPlnAreaPlantSwd","Year2 FPlnAreaPlantSwd",370.820),
      numericInput("year2FPlnAreaHarvSwd","Year2 FPlnAreaHarvSwd",1282.40),
      
      numericInput("year2FDegFellVol","Year2 FDegFellVol",50731.5),
      numericInput("year2FDegFellArea","Year2 FDegFellArea",11669.9),
      
      p("Import Burn Data from text file"),
      fileInput("BurnDataYear2","Import Burn Data for Year 2"),
      
      br(),
      
      # Monitoring Report Params 
      p("Monitoring Report Params"),
      
      numericInput("mrpMpDays","Monitoring Period Days",730),
      numericInput("mrpRpDays","Reporting Period Days",540),
      numericInput("mrpErpaYearlyFRL","FRL",FRL),
      numericInput("mrpErpaYearlyFRLDefor","FRLDeforestation",FRLDeforestation),
      numericInput("mrpErpaYearlyFRLFDeg","FRLForestDegradation", FRLForestDegradation),
      numericInput("mrpErpaYearlyFRLEnh","FRLRemovalsBySinks",0, FRLRemovalsBySinks),
      numericInput("mrpErpaTransferredERs","TransferredERs",0),
      numericInput("mrpErpaContestedERs","ContestedERs",0),
      numericInput("mrpErpaSoldERs","SoldERs",0),
      numericInput("mrpErpaRiskSetaside","RiskSetaside",0.16),
      numericInput("mrpErpaPreviousFRL","PreviousFRL",0),
      numericInput("mrpErpaPreviousEmRems","PreviousEmRems",0),
      numericInput("mrpErpaPreviousERs","PreviousERs",0),
      numericInput("mrpFDegUncertaintyDiscount","FDegUncertaintyDiscount",0.15),
      
      hr(),
      
     # h3("Uncertainty Params"),
     # numericInput("MCRuns","MCRuns", 1.5e6, min= 0),
     # numericInput("MCTolerance","MCTolerance", 0.0115),
     # numericInput("seed","seed", 08121976)
     # 
     h3('Next'),
     disabled(actionButton("ProceedtoRunPage", "Proceed to Run Calculations")),
     textOutput("reviewvaluesvalid"),
     textOutput("reviewvaluesinvalid"),
     br(),
    ),
    
    
    ##### Calculate Page #### 
    
   
    tabPanel(
      "Calculate",
      h3('Calculate'),
      br(),
      actionButton('run', 'Run'),
      actionButton('cancel', 'Cancel'),
      actionButton('status', 'Check Status'),
      hr(),
      h3('Results'),
      tableOutput("Table4_2"),
      hr(),
      tableOutput("Table4_3"),
      
      hr(),
      h3('Export'),
      disabled(downloadButton('downloadData',
                     'Download Data (.Rdata file)')),
     
    )
    
  ))
)


server <- function(input, output, session) {
  hideTab(inputId = "tabs", target = "Calculate")
  
  
###### Import Previous Data Set #################
  
  sessionData <- reactiveValues()
  LoadToEnvironment <- function(RData, localEnv = new.env()) {
    load(RData, localEnv)
    return(localEnv)
  }
  
  observeEvent(input$PreviousData$datapath, {
    if (!is.null(input$PreviousData$datapath)) {
      # Use a reactiveFileReader to read the file on change, and load the content into a new environment
      rdataEnv <-
        reactiveFileReader(1000,
                           session,
                           input$PreviousData$datapath,
                           LoadToEnvironment)
      
      # Convert the env into a list to send to the calc Function      
      sessionData$calcEnv <- as.list(rdataEnv())
      
      # What names are in the file.
      sessionData$rdataNames <- data.frame(rdataNames=names(rdataEnv()))
      colnames(sessionData$rdataNames) <- c("RData content names")
      
      output$rdataNames <- renderTable({
        sessionData$rdataNames
      })
      enable('ProceedtoRunPage')
    }
  })
  
  
  calcEnv <- reactive({
    return(sessionData$calcEnv)
  })
  
  # UncertaintyParams <- reactive({
  #   UCP <- list()
  #   UCP$MCRuns <-input$MCRuns
  #   UCP$MCTolerance <- input$MCTolerance
  #   UCP$seed <- input$seed
  #   return(UCP)
  # })
  
########## Import Burn Data ############
  
   Y1BD <- reactive({
    
    data <- input$BurnDataYear1
    assign('year1FDegBurnData', data, envir = globalenv()) 
    if(is.null(data)){return()}
    tbl <- read.table(data$datapath, sep="")
    return(tbl)
  })
  
  Y2BD <- reactive({
    
    data <- input$BurnDataYear2
    assign('year2FDegBurnData', data, envir = globalenv()) 
    if(is.null(data)){return()}
    tbl <- read.table(data$datapath, sep="")
    return(tbl)
  })

########### Input Monitored Values or Monitoring Report Parameters ### 
    
  # Save numeric Inputs into a Reactive List so they can be displayed
  MonitoredValues <- reactive({
    mv <- list()
    mv$year1$year <- input$year1year
    #including these in this function causes the output table to have duplicate rows for each other other mv variables - tried to split into separate lists but can't combine them
    mv$year1$FDegBurnData <- Y1BD()
    mv$year1$FPlnVolHarvHwd <- input$year1FPlnVolHarvHwd
    mv$year1$FPlnAreaStockHwd <- input$year1FPlnAreaStockHwd
    mv$year1$FPlnAreaPlantHwd <- input$year1FPlnAreaPlantHwd
    mv$year1$FPlnAreaHarvHwd <- input$year1FPlnAreaHarvHwd
    mv$year1$FPlnAreaJustGrowsHwd <- input$year1FPlnAreaStockHwd - input$year1FPlnAreaHarvHwd 
    mv$year1$FPlnVolHarvSwd  <- input$year1FPlnVolHarvSwd
    mv$year1$FPlnAreaStockSwd <- input$year1FPlnAreaStockSwd
    mv$year1$FPlnAreaPlantSwd <- input$year1FPlnAreaPlantSwd
    mv$year1$FPlnAreaHarvSwd  <- input$year1FPlnAreaHarvSwd
    mv$year1$FPlnAreaJustGrowsSwd <- input$year1FPlnAreaStockSwd - input$year1FPlnAreaHarvSwd
    mv$year1$FDegFellVol <- input$year1FDegFellVol
    mv$year1$FDegFellArea <- input$year1FDegFellArea
    
    #Year2
    
    mv$year2$year <- input$year2year
    mv$year2$FDegBurnData <- Y2BD()
    mv$year2$FPlnVolHarvHwd <- input$year2FPlnVolHarvHwd
    mv$year2$FPlnAreaStockHwd <- input$year2FPlnAreaStockHwd
    mv$year2$FPlnAreaPlantHwd <- input$year2FPlnAreaPlantHwd
    mv$year2$FPlnAreaHarvHwd <- input$year2FPlnAreaHarvHwd
    mv$year2$FPlnAreaJustGrowsHwd <- input$year2FPlnAreaStockHwd - input$year2FPlnAreaHarvHwd 
    mv$year2$FPlnVolHarvSwd  <- input$year2FPlnVolHarvSwd
    mv$year2$FPlnAreaStockSwd <- input$year2FPlnAreaStockSwd
    mv$year2$FPlnAreaPlantSwd <- input$year2FPlnAreaPlantSwd
    mv$year2$FPlnAreaHarvSwd  <- input$year2FPlnAreaHarvSwd
    mv$year2$FPlnAreaJustGrowsSwd <- input$year2FPlnAreaStockSwd - input$year2FPlnAreaHarvSwd
    mv$year2$FDegFellVol <- input$year2FDegFellVol
    mv$year2$FDegFellArea <- input$year2FDegFellArea

    return(mv)
  })

  MonitoringReportParams <- reactive({
    
    mrp <- list()
    
    mrp$MpDays <- input$mrpMpDays
    mrp$RpDays <- input$mrpRpDays 
    mrp$IsRpEqualToMp <- (input$mrpMpDays == input$mrpRpDays)
    mrp$RpMpRatio <- (input$mrpRpDays / input$mrpMpDays)
    mrp$RpMpProrataYears <- 2 * mrp$RpMpRatio
    mrp$ErpaYearlyFRL <- input$mrpErpaYearlyFRL  
    mrp$ErpaYearlyFRLDefor <- input$mrpErpaYearlyFRLDefor  
    mrp$ErpaYearlyFRLFDeg <- input$mrpErpaYearlyFRLFDeg  
    mrp$ErpaYearlyFRLEnh <- input$mrpErpaYearlyFRLEnh
    mrp$ErpaTransferredERs <- input$mrpErpaTransferredERs 
    mrp$ErpaContestedERs <- input$mrpErpaContestedERs 
    mrp$ErpaSoldERs <- input$mrpErpaSoldERs 
    mrp$ErpaRiskSetaside <- input$mrpErpaRiskSetaside 
    mrp$ErpaPreviousFRL <- input$mrpErpaPreviousFRL 
    mrp$ErpaPreviousEmRems <- input$mrpErpaPreviousEmRems 
    mrp$ErpaPreviousERs <- input$mrpErpaPreviousERs 
    mrp$FDegUncertaintyDiscount <- input$mrpFDegUncertaintyDiscount 
    
    return(mrp)
  })
  
############ Validation ################  
  
  
  iv <- InputValidator$new()
  # 2. Add validation rules
  iv$add_rule("PreviousData", sv_required())
  # iv$add_rule("MCRuns", sv_required(message = 'Enter the value or 0 if unused'))
  # iv$add_rule("MCRuns", ~ if (input$MCRuns< 0)
  #   "Enter a positive number")
  # iv$add_rule("MCTolerance", sv_required())
  # iv$add_rule("seed", sv_required())
  
  iv$add_rule("BurnDataYear1", sv_required())
  iv$add_rule("BurnDataYear2", sv_required())
  iv$add_rule("year1year", sv_required())
  iv$add_rule("year1FPlnVolHarvHwd", sv_required())
  iv$add_rule("year1FPlnAreaStockHwd", sv_required())
  iv$add_rule("year1FPlnAreaPlantHwd", sv_required())
  iv$add_rule("year1FPlnAreaHarvHwd", sv_required())
  iv$add_rule("year1FPlnAreaHarvHwd", sv_required())
  iv$add_rule("year1FPlnVolHarvSwd", sv_required())
  iv$add_rule("year1FPlnAreaStockSwd", sv_required())
  iv$add_rule("year1FPlnAreaPlantSwd", sv_required())
  iv$add_rule("year1FPlnAreaHarvSwd", sv_required())
  iv$add_rule("year1FDegFellVol", sv_required())
  iv$add_rule("year1FDegFellArea", sv_required())
  iv$add_rule("year2FPlnVolHarvHwd", sv_required())
  iv$add_rule("year2FPlnAreaStockHwd", sv_required())
  iv$add_rule("year2FPlnAreaPlantHwd", sv_required())
  iv$add_rule("year2FPlnAreaHarvHwd", sv_required())
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
  iv$add_rule("mrpErpaYearlyFRLDefor", sv_required())
  iv$add_rule("mrpErpaYearlyFRLFDeg", sv_required())
  iv$add_rule("mrpErpaYearlyFRLEnh", sv_required())
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
  
  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }
  
  set_status <- function(msg){
    write(msg, status_file)
  }
  
  fire_interrupt <- function(){
    set_status("interrupt")
  }
  
  fire_ready <- function(){
    set_status("Ready")
  }
  
  fire_running <- function(perc_complete){
    if(missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0("Running... ", perc_complete, "% Complete")
    set_status(msg)
  }
  
  interrupted <- function(){
    get_status() == "interrupt"
  }
  
  # Delete file at end of session
  onStop(function(){
    print(status_file)
    if(file.exists(status_file))
      unlink(status_file)
  })
  
  # Create Status File
  fire_ready()
  
  nclicks <- reactiveVal(0)
  result_val <- reactiveVal()
  
  observeEvent(input$run,{
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    
    if ("env" %in% names(result_val())) {
      result_val(NULL)
      disable("downloadData")
    }
    
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(tagList(div("Running....")))
    
    fire_running()

####### Put all inputs into one list to get passed into function #############    
    
    calcEnv <- calcEnv()
    calcEnv$MonitoredValues <- MonitoredValues()
    calcEnv$MonitoringReportParams <- MonitoringReportParams()
    # calcEnv$MCRuns <- UncertaintyParams()$MCRuns
    # calcEnv$MCTolerance <- UncertaintyParams()$MCTolerance
    # calcEnv$seed <- UncertaintyParams()$seed
    
    result <- future({
      
      r <- CalcER_Estimate_Values(fire_running, interrupted, calcEnv)
      enable("downloadData")
      return(r)
    }) %...>% result_val()
    
    # Catch inturrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message)
                    })
    
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  # Register user interrupt
  observeEvent(input$cancel, {
    print("Cancel")
    fire_interrupt()
  })
  
  # Let user get analysis progress
  observeEvent(input$status,{
    print("Status")
    showNotification(get_status())
  })
  
################## Outputs to Display on Screen ###########  

  output$Table4_2 <- function() {
    if(!is.null(result_val()$env))
      return(
    result_val()$env$Table4_2 %>%
      kable("html", caption = 'Monitoring Report Table 4.2') %>%
      kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")))
    else{
      return(div(""))
    }
  }
  
  output$Table4_3 <- function() {
    if(!is.null(result_val()$env))
      return(
        result_val()$env$Table4_3 %>%
          kable("html", caption = 'Monitoring Report Table 4.3') %>%
          kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")))
    else{
      return(div(""))
    }
  }
  

 
######## Download Data #############################  
  output$downloadData <- downloadHandler(
    filename = function() {
      return(outputFilename)
    },
    content = function(file) {
      list2env(result_val()$env,environment())
    
      save(list=outputSaveNames,
          file = file)
    }
  )


}
shinyApp(ui = ui, server = server)
