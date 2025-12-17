#' Run shiny app for get and visualize WISP data
#' @description `r lifecycle::badge("experimental")`
#' This function run the Shiny App
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @param ... other params passed to `shiny::runApp`
#' @import shiny
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny dateInput
#' @keywords internal
#' @examples
#' # example code
#' WISP.data::wisp_runApp(launch.browser = rstudioapi::viewer)
#' 
### wisp_runApp
wisp_runApp_test <- function(
    stations = c("WISPstation012", "WISPstation013")
) {
  
  library(shiny)
  library(shinyjs)
  library(plotly)
  library(readr)
  
  shinyApp(
    
    ui = fluidPage(
      useShinyjs(),
      
      titlePanel("WISP station reflectance data"),
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput(
            "station", "Select station:",
            choices = stations, selected = stations[1]
          ),
          
          dateInput("date_from", "Start date:", value = Sys.Date()),
          dateInput("date_to",   "End date:",   value = Sys.Date()),
          
          fluidRow(
            column(
              6,
              selectInput(
                "hour_from", "From (UTC):",
                choices = sprintf("%02d:00", 0:23),
                selected = "09:00"
              )
            ),
            column(
              6,
              selectInput(
                "hour_to", "To (UTC):",
                choices = sprintf("%02d:00", 0:23),
                selected = "17:00"
              )
            )
          ),
          
          hr(),
          
          checkboxInput("do_qc", "Apply Quality Control (QC)", TRUE),
          actionButton("qc_options_btn", "QC options"),
          
          checkboxInput("do_sr", "Apply Sunglint Removal (SR)", TRUE),
          actionButton("sr_options_btn", "SR options"),
          
          hr(),
          
          actionButton("get_data_btn", "Get Reflectance Data"),
          downloadButton("download_csv", "Download CSV"),
          
          hr(),
          actionButton("logout_btn", "Change credentials")
        ),
        
        mainPanel(
          plotlyOutput("plot", height = "520px"),
          hr(),
          verbatimTextOutput("messages")
        )
      )
    ),
    
    server = function(input, output, session) {
      
      # -------------------------
      # Credentials
      # -------------------------
      creds <- reactiveValues(userid = NULL, pwd = NULL, logged = FALSE)
      
      shinyjs::disable("get_data_btn")
      shinyjs::disable("download_csv")
      
      show_login <- function() {
        showModal(
          modalDialog(
            title = "WISP login required",
            textInput("userid_input", "User ID"),
            passwordInput("pwd_input", "Password"),
            footer = actionButton("login_btn", "Login"),
            easyClose = FALSE
          )
        )
      }
      
      observe({
        if (!creds$logged) show_login()
      })
      
      observeEvent(input$login_btn, {
        req(input$userid_input, input$pwd_input)
        creds$userid <- input$userid_input
        creds$pwd    <- input$pwd_input
        creds$logged <- TRUE
        removeModal()
        shinyjs::enable("get_data_btn")
        shinyjs::enable("download_csv")
        user_message("✔️ Successfully authenticated.")
      })
      
      observeEvent(input$logout_btn, {
        creds$userid <- NULL
        creds$pwd    <- NULL
        creds$logged <- FALSE
        shinyjs::disable("get_data_btn")
        shinyjs::disable("download_csv")
        user_message("🔒 Credentials cleared. Please login again.")
      })
      
      # -------------------------
      # Messages
      # -------------------------
      user_message <- reactiveVal("")
      
      # -------------------------
      # QC OPTIONS (preset)
      # -------------------------
      qc_opts <- reactiveValues(
        maxPeak        = 0.05,
        maxPeak_blue   = 0.02,
        qa_threshold   = 0.5,
        qwip_threshold = 0.2,
        calc_scatt     = TRUE,
        calc_SPM       = TRUE,
        calc_TUR       = TRUE,
        calc_TSS       = TRUE,
        calc_gons      = TRUE,
        calc_gons740   = TRUE,
        calc_NDCI      = TRUE,
        calc_mishra    = TRUE
      )
      
      # -------------------------
      # SR OPTIONS (preset)
      # -------------------------
      sr_opts <- reactiveValues(
        calc_scatt   = TRUE,
        calc_SPM     = TRUE,
        calc_TUR     = TRUE,
        calc_TSS     = TRUE,
        calc_gons    = TRUE,
        calc_gons740 = TRUE,
        calc_NDCI    = TRUE,
        calc_mishra  = TRUE
      )
      
      # -------------------------
      # QC OPTIONS POPUP
      # -------------------------
      observeEvent(input$qc_options_btn, {
        showModal(
          modalDialog(
            title = "QC options",
            
            sliderInput("qc_maxPeak", "maxPeak",
                        min = 0.01, max = 0.1, step = 0.005,
                        value = qc_opts$maxPeak),
            
            sliderInput("qc_maxPeak_blue", "maxPeak (blue)",
                        min = 0.005, max = 0.05, step = 0.0025,
                        value = qc_opts$maxPeak_blue),
            
            sliderInput("qc_qa_threshold", "QA threshold",
                        min = 0.1, max = 1, step = 0.05,
                        value = qc_opts$qa_threshold),
            
            sliderInput("qc_qwip_threshold", "QWIP threshold",
                        min = 0.05, max = 0.5, step = 0.05,
                        value = qc_opts$qwip_threshold),
            
            hr(),
            
            checkboxInput("qc_calc_scatt", "calc_scatt", qc_opts$calc_scatt),
            checkboxInput("qc_calc_SPM", "calc_SPM", qc_opts$calc_SPM),
            checkboxInput("qc_calc_TUR", "calc_TUR", qc_opts$calc_TUR),
            checkboxInput("qc_calc_TSS", "calc_TSS", qc_opts$calc_TSS),
            checkboxInput("qc_calc_gons", "calc_gons", qc_opts$calc_gons),
            checkboxInput("qc_calc_gons740", "calc_gons740", qc_opts$calc_gons740),
            checkboxInput("qc_calc_NDCI", "calc_NDCI", qc_opts$calc_NDCI),
            checkboxInput("qc_calc_mishra", "calc_mishra", qc_opts$calc_mishra),
            
            footer = tagList(
              modalButton("Cancel"),
              actionButton("save_qc_opts", "Save")
            )
          )
        )
      })
      
      observeEvent(input$save_qc_opts, {
        qc_opts$maxPeak        <- input$qc_maxPeak
        qc_opts$maxPeak_blue   <- input$qc_maxPeak_blue
        qc_opts$qa_threshold   <- input$qc_qa_threshold
        qc_opts$qwip_threshold <- input$qc_qwip_threshold
        
        qc_opts$calc_scatt     <- input$qc_calc_scatt
        qc_opts$calc_SPM       <- input$qc_calc_SPM
        qc_opts$calc_TUR       <- input$qc_calc_TUR
        qc_opts$calc_TSS       <- input$qc_calc_TSS
        qc_opts$calc_gons      <- input$qc_calc_gons
        qc_opts$calc_gons740   <- input$qc_calc_gons740
        qc_opts$calc_NDCI      <- input$qc_calc_NDCI
        qc_opts$calc_mishra    <- input$qc_calc_mishra
        
        removeModal()
      })
      
      # -------------------------
      # SR OPTIONS POPUP
      # -------------------------
      observeEvent(input$sr_options_btn, {
        showModal(
          modalDialog(
            title = "SR options",
            
            checkboxInput("sr_calc_scatt", "calc_scatt", sr_opts$calc_scatt),
            checkboxInput("sr_calc_SPM", "calc_SPM", sr_opts$calc_SPM),
            checkboxInput("sr_calc_TUR", "calc_TUR", sr_opts$calc_TUR),
            checkboxInput("sr_calc_TSS", "calc_TSS", sr_opts$calc_TSS),
            checkboxInput("sr_calc_gons", "calc_gons", sr_opts$calc_gons),
            checkboxInput("sr_calc_gons740", "calc_gons740", sr_opts$calc_gons740),
            checkboxInput("sr_calc_NDCI", "calc_NDCI", sr_opts$calc_NDCI),
            checkboxInput("sr_calc_mishra", "calc_mishra", sr_opts$calc_mishra),
            
            footer = tagList(
              modalButton("Cancel"),
              actionButton("save_sr_opts", "Save")
            )
          )
        )
      })
      
      observeEvent(input$save_sr_opts, {
        sr_opts$calc_scatt   <- input$sr_calc_scatt
        sr_opts$calc_SPM     <- input$sr_calc_SPM
        sr_opts$calc_TUR     <- input$sr_calc_TUR
        sr_opts$calc_TSS     <- input$sr_calc_TSS
        sr_opts$calc_gons    <- input$sr_calc_gons
        sr_opts$calc_gons740 <- input$sr_calc_gons740
        sr_opts$calc_NDCI    <- input$sr_calc_NDCI
        sr_opts$calc_mishra  <- input$sr_calc_mishra
        
        removeModal()
      })
      
      final_data <- eventReactive(input$get_data_btn, {
        
        req(creds$logged)
        user_message("")
        
        if (as.Date(input$date_to) < as.Date(input$date_from)) {
          shinyjs::alert("End date cannot be earlier than start date.")
          return(NULL)
        }
        
        time_from <- paste0(input$date_from, "T", input$hour_from)
        time_to   <- paste0(input$date_to,   "T", input$hour_to)
        
        # -------------------------
        # RAW
        # -------------------------
        rd <- withCallingHandlers(
          WISP.data::wisp_get_reflectance_multi_data(
            version   = "1.0",
            time_from = time_from,
            time_to   = time_to,
            station   = input$station,
            userid    = creds$userid,
            pwd       = creds$pwd
          ),
          message = function(m) {
            user_message(paste(user_message(), m$message, sep = "\n"))
            invokeRestart("muffleMessage")
          }
        )
        
        if (is.null(rd) || nrow(rd) == 0) return(NULL)
        
        # -------------------------
        # QC (ISOLATED)
        # -------------------------
        if (input$do_qc) {
          
          qc <- isolate(qc_opts)
          
          rd <- withCallingHandlers(
            WISP.data::wisp_qc_reflectance_data(
              data           = rd,
              maxPeak        = qc$maxPeak,
              maxPeak_blue   = qc$maxPeak_blue,
              qa_threshold   = qc$qa_threshold,
              qwip_threshold = qc$qwip_threshold,
              calc_scatt     = qc$calc_scatt,
              calc_SPM       = qc$calc_SPM,
              calc_TUR       = qc$calc_TUR,
              calc_TSS       = qc$calc_TSS,
              calc_gons      = qc$calc_gons,
              calc_gons740   = qc$calc_gons740,
              calc_NDCI      = qc$calc_NDCI,
              calc_mishra    = qc$calc_mishra
            ),
            message = function(m) {
              user_message(paste(user_message(), m$message, sep = "\n"))
              invokeRestart("muffleMessage")
            }
          )
          
          if (is.null(rd) || nrow(rd) == 0) {
            shinyjs::alert("⚠️ QC removed all spectral signatures.")
            return(NULL)
          }
        }
        
        # -------------------------
        # SR (ISOLATED)
        # -------------------------
        if (input$do_sr) {
          
          sr <- isolate(sr_opts)
          
          rd <- withCallingHandlers(
            WISP.data::wisp_sr_reflectance_data(
              qc_data       = rd,
              calc_scatt   = sr$calc_scatt,
              calc_SPM     = sr$calc_SPM,
              calc_TUR     = sr$calc_TUR,
              calc_TSS     = sr$calc_TSS,
              calc_gons    = sr$calc_gons,
              calc_gons740 = sr$calc_gons740,
              calc_NDCI    = sr$calc_NDCI,
              calc_mishra  = sr$calc_mishra
            ),
            message = function(m) {
              user_message(paste(user_message(), m$message, sep = "\n"))
              invokeRestart("muffleMessage")
            }
          )
        }
        
        rd
      })
      
      # -------------------------
      # Plot
      # -------------------------
      output$plot <- renderPlotly({
        fd <- final_data()
        req(fd, nrow(fd) > 0)
        WISP.data::wisp_plot_reflectance_data(fd)
      })
      
      # -------------------------
      # Messages
      # -------------------------
      output$messages <- renderText({
        user_message()
      })
      
      # -------------------------
      # Download CSV
      # -------------------------
      output$download_csv <- downloadHandler(
        filename = function() {
          suffix <- if (input$do_sr) "sr" else if (input$do_qc) "qc" else "raw"
          paste0(
            "wisp_reflectance_",
            input$station, "_",
            input$date_from, "_",
            input$date_to, "_",
            suffix,
            ".csv"
          )
        },
        content = function(file) {
          fd <- final_data()
          req(fd, nrow(fd) > 0)
          readr::write_csv(fd, file)
        }
      )
    }
  )
}

