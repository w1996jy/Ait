#' mfuzz UI Module
#' @description mfuzz UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @name mfuzz_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import shinythemes
#' @import DT
#' @export
#'
mfuzz_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Mfuzz Clustering',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run"), "Run")
        ),
      ),
      page_fluid(
        fluidRow(  # Use fluidRow for horizontal layout
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Table of Mfuzz analyse",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Download',
                  downloadButton(ns("download"), "Download Table")
                )
              ),
              mainPanel(
                dataTableOutput(ns("clusterSummary"))
              )
            )
          ),
          column(
            width = 6,  # Half of the row
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Mfuzz plot",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  numericInput(ns("clusters"), "Number of Clusters", value = 6, min = 1),
                  numericInput(ns("fuzziness"), "Fuzziness (m)", value = 1.25, min = 1, step = 0.01)
                ),
                accordion_panel(
                  title = 'Download',
                  downloadButton(ns("downloadPlot"), "Download PDF")
                )
              ),
              mainPanel(
                plotOutput(ns("mfuzzPlot"))
              )
            )
          )
        )
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
#' @param input A unique identifier for the module.
#' @param output description
#' @param session description
#' @import Mfuzz
#' @import tidyverse
#' @import Biobase
#' @import VIM
#' @return A Shiny server function that handles clustering and data processing.
#' @name mfuzz_server
#' @importFrom grDevices dev.off pdf
#' @export
#'
utils::globalVariables(c("pdf", "dev.off", "."))
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
    output$clusterSummary <- DT::renderDataTable({
      req(clusteringResult())
      # 创建数据框
      mfuzz_table <- data.frame(
        ID = names(clusteringResult()$cluster$cluster),
        cluster = clusteringResult()$cluster$cluster
      )
      # 返回交互式表格
      DT::datatable(mfuzz_table)
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
