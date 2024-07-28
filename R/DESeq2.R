#' Draw a deseq2
#' @name deseq2_ui
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

library(shiny)
library(shinythemes)
library(dashboardthemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(DESeq2)
library(pheatmap)
library(ggplot2)

deseq2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("DESeq2 Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("count_file"), "Upload count data (CSV)"),
          fileInput(ns("meta_file"), "Upload metadata (CSV)"),
          actionButton(ns("run_analysis"), "Run DESeq2 Analysis"),
          hr(),
          radioButtons(ns("download_format"), "Select download format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
          downloadButton(ns("download_volcano_plot"), "Download Volcano Plot"),
          div(style = "margin-top: 20px;", downloadButton(ns("download_heatmap_plot"), "Download Heatmap")),
          div(style = "margin-top: 40px;", downloadButton(ns("download_results"), "Download Results"))
        ),
        mainPanel(
          DTOutput(ns("results_table")),
          plotOutput(ns("volcano_plot")),
          plotOutput(ns("heatmap_plot"))
        )
      )
    )
  )
}

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

shinyApp(ui = deseq2_ui("deseq2"), server = deseq2_server)
