library(shiny)
library(ggplot2)
if (!require('colourpicker')) install.packages("colourpicker")

#' Volcano Plot User Interface
#' @description Defines the UI for the Volcano Plot module in the Shiny application.
#' @param id The module's ID.
#' @import shiny
#' @import colourpicker
#' @noRd
VolcanoPlot_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "VolcanoPlot",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        # 文件上传控件
        fileInput(ns("file_upload"), "Upload CSV file:",
                  accept = c(".csv")),
        # User input: Select columns for p-value, log2 fold change, and VIP
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
        ),
        
        # Run button
        actionButton(ns("run_btn"), "Run"),
        br(),
        br(),
        # Download format selection
        radioButtons(ns("file_format"), "Choose file format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                     selected = "png"),
        # Download button
        downloadButton(ns("download_plot"), "Download Plot")
      ),
      mainPanel(
        uiOutput(ns("output_ui")))
      )
    )
}

#' Volcano Plot Server Logic
#' @description Defines the server logic for the Volcano Plot module in the Shiny application.
#' @param input, output, session Internal parameters for `{shiny}`. Do not remove.
#' @param data A reactive expression that provides the data for the Volcano Plot.
#' @import shiny
#' @import ggplot2
#' @import colourpicker
#' @noRd
VolcanoPlot_server <- function(input, output, session, data) {
  # Reactive value to store the data
  data <- reactiveVal(NULL)
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    data(read.csv(input$file_upload$datapath))
    
    # Update select input choices based on data columns
    updateSelectInput(session, "pvalue_col", choices = names(data()))
    updateSelectInput(session, "log2fc_col", choices = names(data()))
    updateSelectInput(session, "vip_col", choices = names(data()))
  })
  
  # Generate the volcano plot based on user inputs
  plot_reactive <- reactive({
    req(input$pvalue_col, input$log2fc_col, input$vip_col)
    
    # Data cleaning
    data_clean <- data()
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