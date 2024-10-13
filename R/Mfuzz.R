#' Application User Interface for Mfuzz Clustering
#'
#' Defines the user interface for the Mfuzz clustering module in a Shiny application.
#' 
#' @param id A unique identifier for the module.
#' @import shiny
#' @import shinythemes
#' @return A Shiny UI object for the Mfuzz clustering module.
#' @name mfuzz_ui
library(Mfuzz)
library(tidyverse)
library(Biobase)
library(VIM)

# UI function
mfuzz_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Mfuzz Soft Clustering"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        numericInput(ns("clusters"), "Number of Clusters", value = 6, min = 1),
        numericInput(ns("fuzziness"), "Fuzziness (m)", value = 1.25, min = 1, step = 0.01),
        actionButton(ns("run"), "Run Clustering"),
        br(),br(),
        downloadButton(ns("download"), "Download Cluster table"),
        br(),br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("mfuzzPlot")),
        textOutput(ns("clusterSummary"))
      )
    )
  )
}
#' Server logic for Mfuzz Clustering
#'
#' Defines the server logic for the Mfuzz clustering module in a Shiny application.
#' This includes reading the input file, preprocessing the data, running the Mfuzz clustering algorithm,
#' and providing the results for download and visualization.
#'
#' @param id A unique identifier for the module.
#' @import Mfuzz
#' @import tidyverse
#' @import Biobase
#' @import VIM
#' @return A Shiny server function that handles clustering and data processing.
#' @name mfuzz_server
mfuzz_server <- function(input, output, session) {
  ns <- session$ns
  clusteringResult <- reactiveVal(NULL)
  observeEvent(input$run, {
    req(input$file)
    
    # Data pre-processing
    yeast <- read.csv(input$file$datapath, row.names = 1)
    
    yeastF_kNN <- VIM::kNN(yeast, k = 3)
    rownames(yeastF_kNN) <- rownames(yeast)
    yeastF_clean <- yeastF_kNN %>% 
      dplyr::select(!contains("imp")) %>% 
      as.data.frame() %>% 
      dplyr::filter(rowSums(.) != 0)
    
    # 检查数据是否有效
    if (nrow(yeastF_clean) == 0 || ncol(yeastF_clean) == 0) {
      showNotification("The dataset is invalid after preprocessing.", type = "error")
      return(NULL)
    }
    
    # Convert to ExpressionSet
    yeastF_clean <- tryCatch({
      Biobase::ExpressionSet(assayData = data.matrix(yeastF_clean))
    }, error = function(e) {
      showNotification("Failed to create ExpressionSet object.", type = "error")
      return(NULL)
    })
    
    # 检查ExpressionSet对象是否创建成功
    if (is.null(yeastF_clean)) {
      return(NULL)
    }
    
    yeastF <- Mfuzz::standardise(yeastF_clean)
    
    # Fuzziness estimation and clustering
    m <- input$fuzziness
    cl <- tryCatch({
      Mfuzz::mfuzz(yeastF, c = input$clusters, m = m)
    }, error = function(e) {
      showNotification("Clustering failed.", type = "error")
      return(NULL)
    })
    
    # 保存聚类结果
    clusteringResult(list(data = yeastF, cluster = cl))
    
    # Plot clustering result
    output$mfuzzPlot <- renderPlot({
      req(cl)
      Mfuzz::mfuzz.plot(yeastF, cl, mfrow = c(2, 3), new.window = FALSE)
    })
    
    # Show cluster summary
    output$clusterSummary <- renderText({
      req(yeastF_clean)
      paste("Clusters assigned to", nrow(Biobase::exprs(yeastF_clean)), "genes.")
    })
    # Download cluster output
    output$download <- downloadHandler(
      filename = function() {
        paste("clusters_output", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        req(clusteringResult())
        write.table(clusteringResult()$cluster$cluster, file, quote = FALSE, row.names = TRUE, col.names = FALSE, sep = "\t")
      })
    
    
  })
  # Download plot as PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("mfuzz_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(clusteringResult())
      pdf(file, width = 8, height = 6)
      Mfuzz::mfuzz.plot(clusteringResult()$data, clusteringResult()$cluster, mfrow = c(2, 3), new.window = FALSE)
      dev.off()
    }
  )
  
}