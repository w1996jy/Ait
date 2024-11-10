#' boxplot UI Module
#' @description boxplot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title boxplot_ui
#' @name boxplot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @export
#'
boxplot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Boxplot',
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
                uiOutput(ns("xcol_ui")),  # X-axis column selection
                uiOutput(ns("ycol_ui")),  # Y-axis column selection
                colourpicker::colourInput(ns("box_color"), "Box color:", value = "black"),
                colourpicker::colourInput(ns("box_fill"), "Box fill color:", value = "red"),
                textInput(ns("x_label"), "Enter the X-axis label:", value = "Group"),
                textInput(ns("y_label"), "Enter the Y-axis label:", value = "Value")
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 6, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
              ),
            mainPanel(
              plotOutput(ns("boxPlot"))  # Correct plotOutput id
            )
            )
          )
        )
      )
    )
}

#' boxplot_server Module
#' @description Server logic for boxplot_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import ggplot2
#' @import shiny
#' @importFrom utils read.csv
#' @export
#'
boxplot_server <- function(input, output, session) {
  ns <- session$ns

  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Dynamically generate UI for X and Y column selection based on uploaded data
  output$xcol_ui <- renderUI({
    req(data())
    selectInput(ns("xcol"), "Select X-axis (Grouping Column):", choices = names(data()))
  })

  output$ycol_ui <- renderUI({
    req(data())
    selectInput(ns("ycol"), "Select Y-axis (Numeric Column):", choices = names(data()))
  })

  # Create the boxplot when the action button is pressed
  observeEvent(input$run, {
    output$boxPlot <- renderPlot({
      req(input$xcol, input$ycol)
      plot_data <- data()

      ggplot(plot_data, aes_string(x = input$xcol, y = input$ycol)) +
        geom_boxplot(color = input$box_color, fill = input$box_fill) +
        labs(x = input$x_label, y = input$y_label) +
        theme_bw()
    })
  })

  # Allow user to download the plot as a PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("boxplot-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = input$width, height = input$height)
    }
  )
}
