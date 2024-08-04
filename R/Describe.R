#' UI for Descriptive Statistics
#'
#' @param id Namespace ID
#' @description
#' Creates the UI for descriptive statistics analysis using psych::describe
#' @import shiny
#' @import DT
#' @name describe_ui
describe_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Descriptive Statistics Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("data_file"), "Upload CSV data"),
          actionButton(ns("run_analysis"), "Run Descriptive Analysis"),
          hr(),
          downloadButton(ns("download_table"), "Download Descriptive Statistics")
        ),
        mainPanel(
          DTOutput(ns("desc_table"))
        )
      )
    )
  )
}
#' Server for Descriptive Statistics
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @description
#' Handles the server-side logic for descriptive statistics analysis using psych::describe
#' @name describe_server
#' @importFrom psych describe
#' @importFrom DT datatable
#' @importFrom utils read.csv
#' @noRd
describe_server <- function(input, output, session) {
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, row.names = 1)
  })
  
  desc_stats <- eventReactive(input$run_analysis, {
    req(data())
    describe(data())
  })
  
  output$desc_table <- renderDT({
    req(desc_stats())
    datatable(as.data.frame(desc_stats()), extensions = 'Buttons', options = list(
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
