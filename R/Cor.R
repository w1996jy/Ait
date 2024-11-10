#' cor UI Module
#' @description cor UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
cor_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Correlation analysis',
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
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                textInput(ns("plot_width"), "Plot Width (inches):", value = "10"),
                textInput(ns("plot_height"), "Plot Height (inches):", value = "10"),
                textInput(ns("fontsize"), "Font Size:", value = "15"),
                radioButtons(ns("color_scheme"), "Color Scheme:",
                             choices = list("Red-Blue" = "RdBu", "Green-Blue" = "GnBu", "Heat" = "heat"))
                ),
              accordion_panel(
                title = 'Download',
                radioButtons(ns("download_format"), "Select download format:",
                             choices = list("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("download_plot"), "Download")
              )
              ),
            mainPanel(
              plotOutput(ns("heatmap_plot")
              ))
            )
          ),
        fluidRow(  # Use fluidRow to arrange elements horizontally
          column(
            width = 6,  # Each column will take half of the row width
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Table of correlation",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = "Download",
                  downloadButton(ns("download_table"), "Download")
                )
                ),
              mainPanel(
                DTOutput(ns("cor_table"))
              )
            )
          ),
          column(
            width = 6,  # Another half of the row
            height = 300,
            navset_card_tab(
              height = 300,
              full_screen = TRUE,
              title = "Table of Pvalue",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = "Download",
                  downloadButton(ns("download_p_table"), "Download")
                )
              ),
              mainPanel(
                DTOutput(ns("pvalue_table"))
              )
            )))
        )
      )
    )
}

#' cor Server Module
#' @description Server logic for cor
#' @name cor_server
#' @param input, output, session Standard shiny server arguments
#' @import shiny
#' @import pheatmap
#' @import dplyr
#' @import writexl
#' @import pheatmap
#' @import ggplot2
#' @import DT
#' @importFrom dplyr left_join
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames cor.test rbinom rnorm rpois runif
#' @importFrom utils read.csv
#' @export
#'
utils::globalVariables(c("calc_cor_and_pval", "colorRampPalette", "runif", "rnorm",
                         "rbinom", "rpois"))
cor_server <- function(input, output, session) {
  ns <- session$ns
  data <- reactive({
    req(input$data_file)
    # Read data and ensure all columns are numeric, removing non-numeric columns
    raw_data <- read.csv(input$data_file$datapath, row.names = 1)
    numeric_data <- raw_data[sapply(raw_data, is.numeric)]  # Keep only numeric columns
    numeric_data
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
    print(cor_and_pval()$cor)
    color_scheme <- switch(input$color_scheme,
                           "RdBu" = colorRampPalette(c("greenyellow", "white", "red"))(100),
                           "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                           "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
    pheatmap(cor_and_pval()$cor,
             display_numbers = TRUE,
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             fontsize = as.numeric(input$fontsize),
             color = color_scheme)
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
                             "RdBu" = colorRampPalette(c("greenyellow", "white", "red"))(100),
                             "GnBu" = colorRampPalette(c("green", "white", "blue"))(100),
                             "heat" = colorRampPalette(c("red", "yellow", "white"))(100))
      ggsave(file, plot = pheatmap(cor_and_pval()$cor,
                                   display_numbers = TRUE,
                                   cluster_rows = FALSE,
                                   cluster_cols = FALSE,
                                   fontsize = as.numeric(input$fontsize),
                                   color = color_scheme,
                                   silent = TRUE)$gtable,
             device = input$download_format,
             width = as.numeric(input$plot_width),
             height = as.numeric(input$plot_height))
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

