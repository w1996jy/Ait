#' UI for Correlation Analysis
#'
#' @param id Namespace ID
#' @description
#' Creates the UI for correlation analysis
#' @name cor_ui
cor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Correlation Analysis"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("data_file"), "Upload CSV data"),
          actionButton(ns("run_analysis"), "Run Correlation Analysis"),
          hr(),
          sliderInput(ns("plot_width"), "Plot Width (inches):", min = 5, max = 20, value = 10),
          sliderInput(ns("plot_height"), "Plot Height (inches):", min = 5, max = 20, value = 10),
          sliderInput(ns("fontsize"), "Font Size:", min = 10, max = 30, value = 15),
          radioButtons(ns("color_scheme"), "Color Scheme:",
                       choices = list("Red-Blue" = "RdBu", "Green-Blue" = "GnBu", "Heat" = "heat")),
          radioButtons(ns("download_format"), "Select download format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_plot"), "Download Correlation Heatmap")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_table"), "Download Correlation Table")),
          div(style = "margin-bottom: 10px;", downloadButton(ns("download_p_table"), "Download P-Value Table"))
        ),
        mainPanel(
          plotOutput(ns("heatmap_plot")),
          htmlOutput(ns("cor_table_title")),
          DTOutput(ns("cor_table")),
          htmlOutput(ns("pvalue_table_title")),
          DTOutput(ns("pvalue_table"))
        )
      )
    )
  )
}
#' Server for Correlation Analysis
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object.
#' @description
#' Handles the server-side logic for correlation analysis
#' @name cor_server
cor_server <- function(input, output, session) {
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, row.names = 1)
  })
  
  calc_cor_and_pval <- function(data) {
    n <- ncol(data)
    cor_matrix <- matrix(NA, n, n)
    pval_matrix <- matrix(NA, n, n)
    rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)
    rownames(pval_matrix) <- colnames(pval_matrix) <- colnames(data)
    for (i in 1:n) {
      for (j in i:n) {
        test <- cor.test(data[, i], data[, j])
        cor_matrix[i, j] <- test$estimate
        cor_matrix[j, i] <- test$estimate
        pval_matrix[i, j] <- test$p.value
        pval_matrix[j, i] <- test$p.value
      }
    }
    list(cor = cor_matrix, pval = pval_matrix)
  }
  
  cor_and_pval <- eventReactive(input$run_analysis, {
    req(data())
    calc_cor_and_pval(data())
  })
  
  output$heatmap_plot <- renderPlot({
    req(cor_and_pval())
    color_scheme <- switch(input$color_scheme,
                           "RdBu" = colorRampPalette(c("blue", "white", "red"))(100),
                           "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                           "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
    pheatmap(cor_and_pval()$cor, 
             display_numbers = TRUE, 
             cluster_rows = FALSE, 
             cluster_cols = FALSE,
             fontsize = input$fontsize,
             color = color_scheme)
  })
  
  output$cor_table_title <- renderUI({
    req(cor_and_pval())
    HTML(paste("<h4>Correlation Matrix</h4>"))
  })
  
  output$pvalue_table_title <- renderUI({
    req(cor_and_pval())
    HTML(paste("<h4>P-Value Matrix</h4>"))
  })
  
  output$cor_table <- renderDT({
    req(cor_and_pval())
    cor_df <- as.data.frame(cor_and_pval()$cor)
    datatable(cor_df, rownames = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$pvalue_table <- renderDT({
    req(cor_and_pval())
    pval_df <- as.data.frame(cor_and_pval()$pval)
    datatable(pval_df, rownames = TRUE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("correlation_heatmap.", input$download_format, sep = "")
    },
    content = function(file) {
      color_scheme <- switch(input$color_scheme,
                             "RdBu" = colorRampPalette(c("blue", "white", "red"))(100),
                             "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                             "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
      ggsave(file, plot = pheatmap(cor_and_pval()$cor, 
                                   display_numbers = TRUE, 
                                   cluster_rows = FALSE, 
                                   cluster_cols = FALSE, 
                                   fontsize = input$fontsize,
                                   color = color_scheme,
                                   silent = TRUE)$gtable, 
             device = input$download_format,
             width = input$plot_width,
             height = input$plot_height)
    },
    contentType = "image"
  )
  
  output$download_table <- downloadHandler(
    filename = function() {
      "correlation_table.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(cor_and_pval()$cor), file)
    },
    contentType = "text/csv"
  )
  
  output$download_p_table <- downloadHandler(
    filename = function() {
      "pvalue_table.csv"
    },
    content = function(file) {
      write.csv(as.data.frame(cor_and_pval()$pval), file)
    },
    contentType = "text/csv"
  )
}
