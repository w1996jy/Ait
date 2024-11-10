#' Describe UI Module
#' @name describe_ui
#' @description describe UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
describe_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    title = 'Descriptive Statistics',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("data_file"), "Upload CSV data")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_analysis"), "Run")
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("download_table"), "Download Table")
        )
      ),
      mainPanel(
        DTOutput(ns("desc_table"))
      )
    )
  )
}

#' Server for Descriptive Statistics
#' @name describe_server
#' @description Handles the server-side logic for descriptive statistics analysis
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom psych describe
#' @importFrom DT datatable
#' @importFrom utils read.csv
#' @export
describe_server <- function(input, output, session) {
  ns <- session$ns
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, row.names = 1)
  })

  desc_stats <- eventReactive(input$run_analysis, {
    req(data())
    psych::describe(data())
  })

  output$desc_table <- renderDT({
    req(desc_stats())
    DT::datatable(as.data.frame(desc_stats()), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })

  output$download_table <- downloadHandler(
    filename = function() {
      "descriptive_statistics.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(desc_stats()), file)
    },
    contentType = "text/csv"
  )
}
