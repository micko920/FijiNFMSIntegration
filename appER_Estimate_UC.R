library(shiny)
library(rlang)
library(shinyjs)
library(dplyr)
library(kableExtra)
library(promises)
library(future)
library(htmltools)

## Change
source("./calcER_Estimate_UC.R")

calcFunc <- CalcERUC
outputFilename <- "Fiji_ER_Estimate_UC.RData"
outputSaveNames <- c(
  "ResultsTables",
  "EmRems_Values",
  "ER_Values",
  "MR_Values",
  "UC_Values",
  "UC_MV_Values",
  "UC_EmRems_Values",
  "Table4_2",
  "Table4_3",
  "Table7_2",
  "Table8"
)


######


ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                useShinyjs(),
                
                ############ Main Panel #####################
                
                mainPanel(
                  h1("ER Estimate Uncertainty"),
                  tabsetPanel(
                  id = "tabs",
                  
                  ########### Data Input Page ##################
                  
                  tabPanel(
                    "DataUpload",
                    fileInput(
                      "PreviousData",
                      "Import ER_Estimate_Values",
                      multiple = FALSE,
                      accept = '.Rdata'
                    ),
                    
                    tableOutput('rdataNames'),
                    disabled(actionButton("ProceedtoRunPage", "Proceed to Run Calculations"))
                  ),
                  tabPanel(
                    "Calculate",
                    actionButton('run', 'Run'),
                    actionButton('cancel', 'Cancel'),
                    actionButton('status', 'Check Status'),
                    hr(),
                    h3('Results'),
                    uiOutput("result"),
                    hr(),
                    h3('Export'),
                    disabled(downloadButton('downloadData',
                                   'Download Data (.Rdata file)')),
                    br()
          
                  )
                  
                )))


server <- function(input, output, session) {
  sessionData <- reactiveValues()
  hideTab(inputId = "tabs", target = "Calculate")
  
  observeEvent(input$ProceedtoRunPage, {
    hideTab(inputId = "tabs", target = "DataUpload")
    showTab(inputId = "tabs", target = "Calculate")
    updateTabsetPanel(session, "tabs", selected = "Calculate")
    
  })
  

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

  fire_running <- function(perc_complete, notification) {
    if (missing(notification))
      msg <- "Running ...."
    else
      msg <- notification
    if (!missing(perc_complete))
      msg <- paste0(msg, " [", perc_complete, "% Complete]")
    set_status(msg)
  }

  interrupted <- function() {
    get_status() == "interrupt"
  }

  # Delete file at end of session
  onStop(function() {
    print(status_file)
    if (file.exists(status_file))
      unlink(status_file)
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
    
    
    if ("html" %in% names(result_val())) {
      result_val(NULL)
      disable("downloadData")
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(tagList(div("Running....")))
    
    fire_running()
    
    result <- future({
      
      Sys.sleep(2)
      r <- do.call(calcFunc, list(fire_running, interrupted, calcEnv()))
      enable("downloadData")
      return(r)
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e) {
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message)
                    })
    
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function() {
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
  observeEvent(input$status, {
    print("Status")
    showNotification(get_status())
  })
  
  # output$result <- renderUI({
  #   req(tagList(result_val()$html))
  # })

  
  output$result <- renderUI({
    req(tagList(result_val()$html))
    if (!is.null(result_val())) {
      return(tagList(result_val()$html))
    }
    else{
      return(div(""))
    }
  })
  
  
  output$downloadData <- downloadHandler(
  
    filename = function() {

      return(outputFilename)
    },
    content = function(file) {
      list2env(result_val()$env,environment())
      
      save(list = outputSaveNames,
          file=file)
    }
  )
  
}
shinyApp(ui = ui, server = server)
