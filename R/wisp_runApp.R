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
wisp_runApp <- function(...) {
  require(shiny)
  require(shinyjs)  # Aggiungi shinyjs per gestire i pop-up
  
  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Inizializza shinyjs
      titlePanel("WISP station reflectance data"),
      sidebarLayout(
        sidebarPanel(
          dateInput("date_from", "Select start date:", value = Sys.Date()),  # Data di inizio
          dateInput("date_to", "Select end date:", value = Sys.Date()),  # Data di fine
          actionButton("get_data_btn", "Get Reflectance Data")  # Bottone per scaricare i dati
        ),
        mainPanel(
          plotly::plotlyOutput("plot"),
          verbatimTextOutput("messages")  # Aggiungi uno spazio per i messaggi
        )
      )
    ),
    server = function(input, output) {
      reflectance_data <- eventReactive(input$get_data_btn, {
        provided_date_from <- as.character(input$date_from)
        provided_date_to <- as.character(input$date_to)
        
        # Controlla che la data di fine non sia precedente alla data di inizio
        if (as.Date(provided_date_to) < as.Date(provided_date_from)) {
          shinyjs::alert("Error: The end date cannot be earlier than the start date.")
          return(NULL)
        }
        # download data
        reflec_data <- WISP.data::wisp_get_reflectance_multi_data(
          time_from = paste0(provided_date_from, "T09:00"),
          time_to = paste0(provided_date_to, "T17:00"),
          station = "WISPstation012",
          userid = userid,
          pwd = pwd
        )
        
        # Controlla se la risposta contiene il messaggio di errore
        if (is.null(reflec_data)) {
          shinyjs::alert("Thank you for your request, but the instrument does not acquire data on this date. Please close the window and try again with another date.")
          return(NULL)  # Esci dalla funzione senza fare altro
        }
        return(reflec_data)
      })
      
      # Esegui QC
      reflectance_data_qc <- reactive({
        req(reflectance_data())
        WISP.data::wisp_qc_reflectance_data(data = reflectance_data())
      })
      
      # Esegui SR
      reflectance_data_sr <- reactive({
        req(reflectance_data_qc())
        WISP.data::wisp_sr_reflectance_data(qc_data = reflectance_data_qc())
      })
      
      # Renderizza il grafico
      output$plot <- plotly::renderPlotly({
        req(reflectance_data_sr())
        WISP.data::wisp_plot_reflectance_data(data = reflectance_data_sr())
      })
    }
  )
}
