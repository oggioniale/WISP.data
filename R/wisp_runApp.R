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
  shinyApp(
    ui = fluidPage(
      titlePanel("Plot WISP station data of specific date from 7 AM to 7 PM"),
      sidebarLayout(
        sidebarPanel(
          dateInput("date", "Select a date:", value = Sys.Date())#,
          # timeInput("time_start", "Seleziona un'ora di inizio:", value = Sys.time()),
          # timeInput("time_stop", "Seleziona un'ora di fine:", value = Sys.time())
        ),
        mainPanel(
          plotly::plotlyOutput("plot")
        )
      )
    ),
    server = function(input, output) {
      output$plot <- plotly::renderPlotly({
        provided_date <- as.character(input$date)
        # download data
        reflec_data <- WISP.data::wisp_get_reflectance_data(
          time_from = paste0(provided_date, "T07:00"),
          time_to = paste0(provided_date, "T19:00"),
          station = "WISPstation012",
          userid = userid,
          pwd = pwd
        )
        # QC on the data
        reflec_data_qc <- WISP.data::qc_reflectance_data(data = reflec_data)
        # SR on the data
        reflec_data_sr <- WISP.data::sr_reflectance_data(qc_data = reflec_data_qc) 
        # Plot data 
        if (!is.null(reflec_data_sr)) {
          # plot
          WISP.data::plot_reflectance_data(data = reflec_data_sr)
        }
      })
    }
  )
}
