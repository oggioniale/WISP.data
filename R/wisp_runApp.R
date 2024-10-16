#' Run the main app
#' @import shiny
#' @param ... other params passed to `shiny::runApp`
#' @export
#' @examples 
#' WISP.data::wisp_runApp(launch.browser=rstudioapi::viewer)
wisp_runApp <- function(...) {
  require(shiny)
  require(plotly)
  require(shinyWidgets)
  source(file = "R/functions.R")
  
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
          plotlyOutput("plot")
        )
      )
    ),
    server = function(input, output) {
      output$plot <- renderPlotly({
        provided_date <- as.character(input$date)
        # download data
        reflec_data <- wisp_get_reflectance_data(
          time_from = paste0(provided_date, "T07:00"),
          time_to = paste0(provided_date, "T19:00"),
          station = "WISPstation012",
          userid = userid,
          pwd = pwd
        )
        if (!is.null(reflec_data)) {
          # plot
          plot_reflectance_data(data = reflec_data)
        }
      })
    }
  )
}