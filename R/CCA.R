#' UI for CCA Module
#'
#' Creates the user interface for the CCA (Canonical Correlation Analysis) module.
#'
#' @name CCA_ui
CCA_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("CCA Analyse"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Upload CSV File1", accept = ".csv"),
        fileInput(ns("file2"), "Upload CSV File2", accept = ".csv"),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("CCA_Plot"))  # Display the generated plot
      )
    )
  )
}

#' Server Function for CCA Module
#'
#' Handles server-side logic for the CCA (Canonical Correlation Analysis) module.
#' @import CCA
#' @name CCA_server
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
    res.cc <- cc(file1, file2)
    
    # Render the CCA plot
    output$CCA_Plot <- renderPlot({
      plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)
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
      res.cc <- cc(file1, file2)
      
      # Create and print the CCA plot
      plt.cc(res.cc, d1 = 1, d2 = 3, type = "v", var.label = TRUE)
      
      dev.off()  # Close PDF device
    }
  )
}
