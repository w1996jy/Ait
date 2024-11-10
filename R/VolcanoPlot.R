#' VolcanoPlot UI Module
#' @description VolcanoPlot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title VolcanoPlot_ui
#' @name VolcanoPlot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @import DT
#' @export
#'
VolcanoPlot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Volcano Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file_upload"), "Upload CSV file:",
                    accept = c(".csv"))
        )
      ),page_fluid(
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
                title = 'Parameter',
                selectInput(ns("pvalue_col"), "Select P-value column:", choices = NULL),
                selectInput(ns("log2fc_col"), "Select log2 Fold Change column:", choices = NULL),
                selectInput(ns("vip_col"), "Select VIP column:", choices = NULL),  # Select VIP column
                # Threshold inputs
                numericInput(ns("pvalue_threshold"), "P-value threshold:", value = 0.05),
                colourpicker::colourInput(ns("pvalue_line_color"), "P-value Line Color:", value = "black"),  # P-value line color (default black)
                numericInput(ns("log2fc_threshold"), "log2 Fold Change threshold:", value = 1),
                colourpicker::colourInput(ns("log2fc_line_color"), "log2 Fold Change Line Color:", value = "black"),  # log2 Fold Change line color (default black)

                # Point color inputs for upregulated and downregulated points
                colourpicker::colourInput(ns("upregulated_color"), "Upregulated Points Color:", value = "red"),  # Upregulated points color
                colourpicker::colourInput(ns("downregulated_color"), "Downregulated Points Color:", value = "blue"),  # Downregulated points color
                colourpicker::colourInput(ns("not_significant_color"), "Not Significant Points Color:", value = "gray"),  # Not significant points color

                # Axis range toggle and inputs
                checkboxInput(ns("use_x_range"), "Set X-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_x_range"), "']"),
                  numericInput(ns("x_min"), "X-axis minimum:", value = -3),
                  numericInput(ns("x_max"), "X-axis maximum:", value = 3)
                ),
                checkboxInput(ns("use_y_range"), "Set Y-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_y_range"), "']"),
                  numericInput(ns("y_min"), "Y-axis minimum:", value = 0),
                  numericInput(ns("y_max"), "Y-axis maximum:", value = 10)
                )
                ),
              accordion_panel(
                title = "Run",
                actionButton(ns("run_btn"), "Run")
                ),
              accordion_panel(
                title = "Download",
                radioButtons(ns("file_format"), "Choose file format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                             selected = "png"),
                # Download button
                downloadButton(ns("download_plot"), "Download")
              )
              ),
            mainPanel(
              uiOutput(ns("output_ui")
                       )
              )
            )
          )
        )
      )
    )
}
#' VolcanoPlot_server Module
#' @description Server logic for VolcanoPlot_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import ggplot2
#' @name VolcanoPlot_server
#' @importFrom utils write.table data
#' @export
#'
utils::globalVariables(c("data"))
VolcanoPlot_server <- function(input, output, session, data) {
  ns <- session$ns
  # Reactive value to store the data
  data1 <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath)
  })

  observeEvent(input$file_upload, {
    req(data1())
    # Update select input choices based on data columns
    updateSelectInput(session, "pvalue_col", choices = names(data1()))
    updateSelectInput(session, "log2fc_col", choices = names(data1()))
    updateSelectInput(session, "vip_col", choices = names(data1()))
  })

  # Generate the volcano plot based on user inputs
  plot_reactive <- reactive({
    req(input$pvalue_col, input$log2fc_col, input$vip_col)

    # Data cleaning
    data_clean <- data1()
    data_clean[[input$pvalue_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$pvalue_col]])))
    data_clean[[input$log2fc_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$log2fc_col]])))
    data_clean[[input$vip_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$vip_col]])))

    # Check if any selected columns contain NA after conversion
    if (any(is.na(data_clean[[input$pvalue_col]]))) {
      stop("Error: P-value column contains non-numeric values that could not be converted.")
    }
    if (any(is.na(data_clean[[input$log2fc_col]]))) {
      stop("Error: log2 Fold Change column contains non-numeric values that could not be converted.")
    }
    if (any(is.na(data_clean[[input$vip_col]]))) {
      stop("Error: VIP column contains non-numeric values that could not be converted.")
    }

    # Categorize points based on thresholds
    data_clean$category <- ifelse(data_clean[[input$log2fc_col]] > input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Up",
                                  ifelse(data_clean[[input$log2fc_col]] < -input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Down", "Not Significant"))

    # Start building the plot
    plot <- ggplot(data_clean, aes_string(x = input$log2fc_col, y = paste0("-log10(", input$pvalue_col, ")"), size = input$vip_col, color = "category")) +
      geom_point() +
      scale_color_manual(values = c("Up" = input$upregulated_color, "Down" = input$downregulated_color, "Not Significant" = input$not_significant_color)) +
      geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", color = input$pvalue_line_color) +  # P-value threshold line
      geom_vline(xintercept = c(-input$log2fc_threshold, input$log2fc_threshold), linetype = "dashed", color = input$log2fc_line_color) +  # log2 Fold Change threshold lines
      theme_bw() +
      labs(x = "log2 Fold Change", y = "-log10(P-value)",
           title = "Volcano Plot", color = "Category", size = "VIP")

    # Apply X-axis range if toggle is enabled
    if (input$use_x_range) {
      plot <- plot + xlim(input$x_min, input$x_max)
    }

    # Apply Y-axis range if toggle is enabled
    if (input$use_y_range) {
      plot <- plot + ylim(input$y_min, input$y_max)
    }

    plot
  })

  # Render the volcano plot
  output$output_ui <- renderUI({
    req(input$run_btn)
    renderPlot({ plot_reactive() })
  })
  # Download handler for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("volcano_plot", Sys.Date(), ".", input$file_format, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), device = input$file_format, width = 8, height = 6, units = "in")
    }
  )
}
