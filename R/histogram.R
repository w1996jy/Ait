#' histogram UI Module
#' @description histogram UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title histogram_ui
#' @name histogram_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Histogram',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = ".csv")
        )
      ),
      page_fluid(
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
                uiOutput(ns("selectUI")),
                numericInput(ns("bins"), "Number of bins:", 30, min = 1),
                colourpicker::colourInput(ns("lineColor"), "Select Line Color", value = "red"),
                colourpicker::colourInput(ns("fillColor"), "Select Bar Fill Color", value = "lightblue"),
                numericInput(ns("plotWidth"), "Plot Width (inches):", 7, min = 1),
                numericInput(ns("plotHeight"), "Plot Height (inches):", 5, min = 1)
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("generate"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("downloadPlot"), "Download")
              )
              ),
            mainPanel(
              plotOutput(ns("histPlot")),
              textOutput(ns("errorMsg"))
            )
            )
          )
        )
      )
    )

}

#' histogram UI Module
#' @description histogram_server Module
#' @name histogram_server
#' @title histogram_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.csv
#' @import ggplot2
#' @export
#'
utils::globalVariables(c("x", "..density.."))

histogram_server <- function(input, output, session) {
  ns <- session$ns
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
