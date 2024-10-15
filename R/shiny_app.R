library(shiny)
library(plotly)
library(shinyWidgets)

source(file = "wisp_Station.R")

ui <- fluidPage(
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
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    provided_date <- as.character(input$date)
    # download data
    reflec_data <- wisp_get_reflectance_data(
      time_from = paste0(provided_date, "T07:00"),
      time_to = paste0(provided_date, "T19:00"),
      station = "WISPstation012",
      userid = "cnr_irea",
      pwd = "W1spcloud4cnr_irea"
    )
    if (!is.null(reflec_data)) {
      # plot
      plot_reflectance_data(data = reflec_data)
    }
  })
}

shinyApp(ui = ui, server = server)
