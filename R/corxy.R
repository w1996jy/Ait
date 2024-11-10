#' @title Calculate the correlation between two sets of data
#' @name corxy_ui
#' @description corxy UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @export
#'
corxy_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Correlation Analysis of Two Traits',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = c(".csv"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Fig of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("xcol")),  # Dropdown for selecting x-axis column
                uiOutput(ns("ycol")),  # Dropdown for selecting y-axis column
                textInput(ns("xaxis"), "X Axis Title", "X Axis"),
                textInput(ns("yaxis"), "Y Axis Title", "Y Axis"),
                colourpicker::colourInput(ns("pointColor"), "Choose Point Color", value = "black"),
                colourpicker::colourInput(ns("lineColor"), "Choose Line Color", value = "blue"),
                radioButtons(ns("format"), "Choose Download Format", choices = c("PDF", "PNG", "JPG", "SVG")),
                numericInput(ns("width"), "Width (in inches)", value = 7, min = 1, max = 20),
                numericInput(ns("height"), "Height (in inches)", value = 5, min = 1, max = 20)
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadPlot"), "Download Plot"),
                helpText("Select the columns for x and y axes, adjust titles,
                         colors, size, and format, and download the plot.")
              )
              ),
            mainPanel(
              plotOutput(ns("plot"))
            )
            )
          )
        )
      )
    )
  }
#' @title Calculate the correlation between two sets of data server
#' @name corxy_server
#' @description corxy UI Module
#' @param input A unique identifier for the Shiny namespace.
#' @param output A unique identifier for the Shiny namespace.
#' @param session A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom stats lm
#' @importFrom colourpicker colourInput
#' @export
corxy_server <- function(input, output, session) {
  ns <- session$ns

  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
    df
  })

  # Generate UI for selecting x and y columns
  output$xcol <- renderUI({
    df <- dataInput()
    selectInput(ns("xcol"), "X Axis", names(df), selected = names(df)[1])
  })

  output$ycol <- renderUI({
    df <- dataInput()
    selectInput(ns("ycol"), "Y Axis", names(df), selected = names(df)[2])
  })

  # Generate the plot
  plotInput <- reactive({
    data <- dataInput()

    # Ensure the selected columns are numeric
    x <- data[[input$xcol]]
    y <- data[[input$ycol]]

    validate(
      need(is.numeric(x), "The selected X axis column must be numeric"),
      need(is.numeric(y), "The selected Y axis column must be numeric")
    )

    # Create linear model
    model <- lm(y ~ x)

    # Calculate the position for the annotation (center on x-axis and top of y-axis)
    annotate_x <- mean(range(x))
    annotate_y <- max(y) + 0.05 * (max(y) - min(y))  # Slightly above the top of y range

    # Create plot
    ggplot(data, aes(x = x, y = y)) +
      geom_point(color = input$pointColor) +
      geom_smooth(method = "lm", se = FALSE, color = input$lineColor) +
      annotate("text", x = annotate_x, y = annotate_y,
               label = as.character(as.expression(paste0("y = ", format(model[["coefficients"]][[2]], digits = 2), "x + ",
                                                         format(model[["coefficients"]][[1]], digits = 2),
                                                         ", R2 = ", format(summary(model)$r.squared, digits = 3)))),
               vjust = 1, hjust = 0.5) +
      labs(x = input$xaxis, y = input$yaxis) +
      theme_bw()
  })

  output$plot <- renderPlot({
    plotInput()
  })

  # Downloadable plot handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", switch(input$format, "PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"), sep = ".")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = switch(input$format, "PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
             width = input$width, height = input$height)
    }
  )
}
