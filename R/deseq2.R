#' Draw a deseq2
#' @description Creates a UI for DESeq2 analysis.
#' @param id A time-series omics matrix.
#' @import shiny
#' @import shinythemes
#' @import dashboardthemes
#' @import shinydashboard
#' @import shinyWidgets
#' @import DT
#' @import DESeq2
#' @import pheatmap
#' @import ggplot2
#' @name deseq2_ui
#' @export
#'
deseq2_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'DESeq2',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("count_file"), "Upload count data (CSV)"),
          fileInput(ns("meta_file"), "Upload metadata (CSV)")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_analysis"), "Run")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 300,
          navset_card_tab(
            height = 300,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              accordion_panel(
                title = "Download",
                downloadButton(ns("download_results"), "Download Table")
                )
              ),
            mainPanel(
              DTOutput(ns("results_table"))
            )
            )
          ),
        fluidRow(  # Use fluidRow for horizontal alignment
          column(
            width = 6,  # Half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Volcano Plot",
              sidebar = accordion(
                accordion_panel(
                  title = "Parameter",
                  radioButtons(ns("download_format"), "Select download format:",
                               choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                  downloadButton(ns("download_volcano_plot"), "Download")
                  )
                ),
              mainPanel(
                plotOutput(ns("volcano_plot"))
              )
            )
          ),
          column(
            width = 6,  # Half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Heatmap plot",
              sidebar = accordion(
                accordion_panel(
                  title = "Parameter",
                  radioButtons(ns("download_format"), "Select download format:",
                               choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                  downloadButton(ns("download_heatmap_plot"), "Download Heatmap")
                )
                ),
              mainPanel(
                plotOutput(ns("heatmap_plot"))
              )
            )
          )
        )
      )
    )
  )
}

#' DESeq2 Server Module
#' @description Server logic for DESeq2 analysis
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @importFrom DESeq2 DESeqDataSetFromMatrix
#' @importFrom DESeq2 DESeq
#' @importFrom DESeq2 results
#' @import pheatmap
#' @import ggplot2
#' @import SummarizedExperiment
#' @importFrom utils head write.csv
#' @importFrom matrixStats rowVars
#' @importFrom stats setNames
#' @name deseq2_server
#' @export
#'
utils::globalVariables(c("log2FoldChange", "pvalue", "color"))
deseq2_server <- function(input, output, session) {
  observeEvent(input$run_analysis, {
    req(input$count_file, input$meta_file)

    count_data <- read.csv(input$count_file$datapath, row.names = 1)
    if (ncol(count_data) < 2) {
      showNotification("Count data must have at least two columns", type = "error")
      return()
    }

    meta_data <- read.csv(input$meta_file$datapath, row.names = 1)
    if (!"Condition" %in% colnames(meta_data)) {
      showNotification("Metadata must contain 'Condition' column", type = "error")
      return()
    }

    dds <- DESeqDataSetFromMatrix(countData = count_data, colData = meta_data, design = ~ Condition)
    dds <- DESeq(dds)
    results <- results(dds)

    output$results_table <- renderDT({
      datatable(
        as.data.frame(results),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })

    volcano_plot <- reactive({
      log2FC_threshold <- 1
      pvalue_threshold <- 0.05

      results$color <- "gray"
      results$color[results$log2FoldChange > log2FC_threshold & results$pvalue < pvalue_threshold] <- "red"
      results$color[results$log2FoldChange < -log2FC_threshold & results$pvalue < pvalue_threshold] <- "blue"

      ggplot(results, aes(x = log2FoldChange, y = -log10(pvalue), color = color)) +
        geom_point() +
        scale_color_identity() +
        theme_bw() +
        labs(title = "Volcano Plot", x = "log2 Fold Change", y = "-log10 p-value") +
        geom_hline(yintercept = -log10(pvalue_threshold), col = "red") +
        geom_vline(xintercept = c(-log2FC_threshold, log2FC_threshold), col = "blue")
    })

    output$volcano_plot <- renderPlot({
      print(volcano_plot())
    })

    heatmap_plot <- reactive({
      topVarGenes <- head(order(rowVars(assay(dds)), decreasing = TRUE), 20)
      mat <- assay(dds)[topVarGenes, ]
      mat <- mat - rowMeans(mat)
      pheatmap(mat, annotation_col = as.data.frame(colData(dds)[, "Condition", drop = FALSE]))
    })

    output$heatmap_plot <- renderPlot({
      print(heatmap_plot())
    })

    output$download_results <- downloadHandler(
      filename = function() {
        "deseq2_results.csv"
      },
      content = function(file) {
        write.csv(as.data.frame(results), file)
      }
    )

    output$download_volcano_plot <- downloadHandler(
      filename = function() {
        paste("volcano_plot.", input$download_format, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = volcano_plot(), device = input$download_format)
      }
    )

    output$download_heatmap_plot <- downloadHandler(
      filename = function() {
        paste("heatmap_plot.", input$download_format, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = heatmap_plot(), device = input$download_format)
      }
    )
  })
}
