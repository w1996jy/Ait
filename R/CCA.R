#' CCA UI Module
#' @description CCA UI Module
#' @param id A unique identifier for the Shiny namespace,CCA.
#' @title CCA_ui
#' @name CCA_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
CCA_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'CCA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Upload CSV File1", accept = ".csv"),
          fileInput(ns("file2"), "Upload CSV File2", accept = ".csv")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("CCA_Plot"))  # Display the generated plot
            )
          )
        )
      )
      )
    )
}
#' CCA_server Server Module
#' @description Server logic for CCA_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import CCA
#' @importFrom grDevices pdf dev.off
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils write.table read.csv
#' @export
#'
CCA_server <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$run, {
    req(input$file1, input$file2)  # Ensure that both files are uploaded

    # Read and process uploaded files
    file1 <- read.csv(input$file1$datapath, row.names = 1)
    file2 <- read.csv(input$file2$datapath, row.names = 1)
    file1 <- as.matrix(file1)  # Convert to matrix
    file2 <- as.matrix(file2)  # Convert to matrix

    # Perform Canonical Correlation Analysis (CCA)
    res.cc <- CCA::cc(file1, file2)

    # Render the CCA plot
    output$CCA_Plot <- renderPlot({
      CCA::plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)
    })
  })

  # Download plot as PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("CCA_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # User-defined width for PDF
      height <- input$height # User-defined height for PDF

      pdf(file, width = width, height = height)  # Open PDF device

      req(input$file1, input$file2)  # Ensure that both files are uploaded

      # Read and process uploaded files
      file1 <- read.csv(input$file1$datapath, row.names = 1)
      file2 <- read.csv(input$file2$datapath, row.names = 1)
      file1 <- as.matrix(file1)  # Convert to matrix
      file2 <- as.matrix(file2)  # Convert to matrix

      # Perform Canonical Correlation Analysis (CCA)
      res.cc <- CCA::cc(file1, file2)

      # Create and print the CCA plot
      CCA::plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)

      dev.off()  # Close PDF device
    }
  )
}
