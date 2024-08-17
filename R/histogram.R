#' Histogram UI
#'
#' This function creates the user interface for the Histogram module in a Shiny application.
#' It includes input options for file upload, column selection, bin number, and appearance settings.
#'
#' @param id The namespace identifier for UI elements to ensure they are unique.
#' @return A tabPanel containing the histogram plot UI components.
#' @import shiny
#' @import colourpicker
#' @import ggplot2
#' @noRd
histogram_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Histogram with Normal Distribution Curve",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        uiOutput(ns("selectUI")),
        numericInput(ns("bins"), "Number of bins:", 30, min = 1),
        colourpicker::colourInput(ns("lineColor"), "Select Line Color", value = "red"),
        colourpicker::colourInput(ns("fillColor"), "Select Bar Fill Color", value = "lightblue"),
        numericInput(ns("plotWidth"), "Plot Width (inches):", 7, min = 1),
        numericInput(ns("plotHeight"), "Plot Height (inches):", 5, min = 1),
        radioButtons(ns("format"), "Select Download Format:",
                     choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
        actionButton(ns("generate"), "Run"),
        downloadButton(ns("downloadPlot"), "Download")
      ),
      mainPanel(
        plotOutput(ns("histPlot")),
        textOutput(ns("errorMsg"))
      )
    )
  )
}
#' Histogram Server Logic
#'
#' Defines the server logic for the histogram plot. It handles data loading, dynamic UI updates,
#' plot rendering, and plot downloading based on user input.
#'
#' @param input, output, session Objects provided by Shiny server to handle reactive input and output.
#' @import shiny
#' @import ggplot2
#' @noRd
histogram_server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$selectUI <- renderUI({
    req(data())
    selectInput(session$ns("column"), "Select Column for Analysis", choices = names(data()))
  })
  
  # Reactive expression for generating the plot based on user input
  plot_reactive <- reactive({
    req(data(), input$column, input$generate > 0)
    selected_data <- data()[[input$column]]
    valid_data <- na.omit(as.numeric(selected_data))
    
    if (length(valid_data) > 0) {
      ggplot(data = data.frame(x = valid_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = input$bins, color = "black", fill = input$fillColor) +
        stat_function(fun = dnorm, args = list(mean = mean(valid_data, na.rm = TRUE), sd = stats::sd(valid_data, na.rm = TRUE)), 
                      color = input$lineColor, size = 1) +
        labs(title = "Histogram with Normal Distribution Curve",
             x = "Value", y = "Density") +
        theme_bw()
    } else {
      NULL
    }
  })
  
  output$histPlot <- renderPlot({
    plot_reactive()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("histogram-", Sys.Date(), ".", input$format, sep="")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), device = input$format,
             width = input$plotWidth, height = input$plotHeight, units = "in")
    }
  )
}
