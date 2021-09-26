library(shiny)
library(rlang)
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

debug_er <<- FALSE # Turn printed output on
show_output <<- FALSE # Turn final table printed output on
plot_mc_output <<- FALSE # Turn on plots for MC samples


## Change
source("./calcER_Estimate_UC.R")

calcFunc <- CalcER_Estimate_UC


######


ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  useShinyjs(),

  ############ Main Panel #####################

  fluidRow(
    column(
      6,
      h1("Monitoring Period Uncertainty Analysis"),
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

    ########### Data Input Page ##################

    tabPanel(
      "DataInput",
      h3("Data Input"),
      fileInput(
        "PreviousData",
        "Input ER_Estimate_Values",
        multiple = FALSE,
        accept = ".Rdata"
      ),
      tableOutput("rdataNames"),
      hr(),
      h3("Uncertainty Params"),
      numericInput("MCRuns", "MCRuns", 1.5e6, min = 0),
      numericInput("MCTolerance", "MCTolerance", 0.0115),
      numericInput("seed", "seed", 08121976),
      hr(),
      h3("Next"),
      disabled(actionButton("ProceedtoRunPage", "Proceed to Run Calculations")),
      textOutput("reviewvaluesvalid"),
      textOutput("reviewvaluesinvalid"),
      br(),
    ),

    ##### Calculate Page ####


    tabPanel(
      "Calculate",
      br(),
      actionButton("run", "Run"),
      actionButton("cancel", "Cancel"),
      actionButton("status", "Check Status"),
      hr(),
      h3("Results"),
      tableOutput("Table5_2_2"),
      tableOutput("Table7"),
      tableOutput("Table8")
    )
  )
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
        reactiveFileReader(
          1000,
          session,
          input$PreviousData$datapath,
          LoadToEnvironment
        )

      # Convert the env into a list to send to the calc Function
      sessionData$calcEnv <- as.list(rdataEnv())

      # What names are in the file.
      sessionData$rdataNames <- data.frame(rdataNames = names(rdataEnv()))
      colnames(sessionData$rdataNames) <- c("RData content names")

      output$rdataNames <- renderTable({
        sessionData$rdataNames
      })
      # enable('ProceedtoRunPage')
    }
  })

  calcEnv <- reactive({
    return(sessionData$calcEnv)
  })

  UncertaintyParams <- reactive({
    UCP <- list()
    UCP$MCRuns <- input$MCRuns
    UCP$MCTolerance <- input$MCTolerance
    UCP$seed <- input$seed
    return(UCP)
  })


  ############ Validation ################

  iv <- InputValidator$new()
  # 2. Add validation rules
  iv$add_rule("PreviousData", sv_required())
  iv$add_rule("MCRuns", sv_required(message = "Enter the value or 0 if unused"))
  iv$add_rule("MCRuns", ~ if (input$MCRuns < 0) {
    "Enter a positive number"
  })
  iv$add_rule("MCTolerance", sv_required())
  iv$add_rule("seed", sv_required())

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
    hideTab(inputId = "tabs", target = "DataInput")
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

  fire_running <- function(perc_complete, notification) {
    if (missing(notification)) {
      msg <- "Running ...."
    } else {
      msg <- notification
    }
    if (!missing(perc_complete)) {
      msg <- paste0(msg, " [", perc_complete, "% Complete]")
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


    if ("html" %in% names(result_val())) {
      result_val(NULL)
      disable("downloadData")
      disable("downloadReport")
    }

    ### Put all inputs into one list to get passed into function

    calcEnvExtended <- calcEnv()
    calcEnvExtended$MCRuns <- UncertaintyParams()$MCRuns
    calcEnvExtended$MCTolerance <- UncertaintyParams()$MCTolerance
    calcEnvExtended$seed <- UncertaintyParams()$seed



    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)

    result_val(tagList(div("Running....")))

    fire_running()

    result <- future(seed = calcEnvExtended$seed, {
      r <- do.call(calcFunc, list(fire_running, interrupted, calcEnvExtended))
      enable("downloadData")
      enable("downloadReport")
      return(r)
    }) %...>% result_val()

    # Catch interrupt (or any other error) and notify user
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

  output$Table5_2_2 <- function() {
    if (!is.null(result_val()$env)) {
      return(
        result_val()$env$Table5_2_2 %>%
          kable("html", caption = "Monitoring Report Table 5.2.2") %>%
          kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
      )
    } else {
      return(div(""))
    }
  }

  output$Table7 <- function() {
    if (!is.null(result_val()$env)) {
      return(
        result_val()$env$Table7 %>%
          kable("html", caption = "Monitoring Report Table 7") %>%
          kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
      )
    } else {
      return(div(""))
    }
  }

  output$Table8 <- function() {
    if (!is.null(result_val()$env)) {
      return(
        result_val()$env$Table8 %>%
          kable("html", caption = "Monitoring Report Table 8") %>%
          kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left"))
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
      return(outputFilename)
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
