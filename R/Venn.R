#' Venn UI Module
#' @description Venn UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title Venn_ui
#' @name Venn_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
Venn_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Venn Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV file", accept = c(".csv"))
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
            layout_sidebar(
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("col_ui")),
                  colourpicker::colourInput(ns("color1"), "Select Color 1", value = "red"),
                  colourpicker::colourInput(ns("color2"), "Select Color 2", value = "blue"),
                  colourpicker::colourInput(ns("color3"), "Select Color 3", value = "green"),
                  colourpicker::colourInput(ns("color4"), "Select Color 4", value = "yellow"),
                  colourpicker::colourInput(ns("color5"), "Select Color 5", value = "purple")
                  ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("plot_venn"), "Run"),
                  ),
                accordion_panel(
                  title = 'Download',
                  numericInput(ns("width"), "Width (inches)", value = 7),
                  numericInput(ns("height"), "Height (inches)", value = 7),
                  selectInput(ns("format"), "Select Format", choices = c("pdf", "png", "jpg", "svg", "eps", "ps", "tex", "jpeg", "bmp")),
                  downloadButton(ns("downloadVenn"), "Download")
                )
                ),
              mainPanel(
                plotOutput(ns("venn_plot"))
              )
              )
            )
          )
        )
      )
    )
}
#' Venn_server Server Module
#' @description Server logic for Venn_server
#' @name Venn_server
#' @title Venn_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @importFrom grid grid.draw
#' @import VennDiagram
#' @import tikzDevice
#' @importFrom grDevices bmp jpeg png postscript svg
#' @export
#'
Venn_server <- function(input, output, session) {
  ns <- session$ns

  dataset <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  output$col_ui <- renderUI({
    req(dataset())
    cols <- colnames(dataset())
    checkboxGroupInput(ns("selected_cols"), "Select columns for Venn Diagram (up to 5)", choices = cols, selected = cols[1:min(5, length(cols))])
  })

  output$venn_plot <- renderPlot({
    req(dataset(), input$selected_cols)
    validate(
      need(length(input$selected_cols) <= 5, "Please select up to 5 columns")
    )

    data <- dataset()[, input$selected_cols, drop = FALSE]

    sets <- lapply(data, function(col) unique(col))
    names(sets) <- colnames(data)

    colors <- c(input$color1, input$color2, input$color3, input$color4, input$color5)[1:length(sets)]

    venn.plot <- venn.diagram(
      x = sets,
      category.names = colnames(data),
      fill = colors,
      filename = NULL, # 设置为NULL，避免保存文件
      output = TRUE
    )
    grid.draw(venn.plot)
  })

  output$downloadVenn <- downloadHandler(
    filename = function() {
      paste("venn_diagram.", input$format, sep = "")
    },
    content = function(file) {
      device <- switch(input$format,
                       pdf = pdf,
                       png = png,
                       jpg = jpeg,
                       svg = svg,
                       eps = postscript,
                       ps = postscript,
                       tex = function(...) tikzDevice::tikz(...),
                       jpeg = jpeg,
                       bmp = bmp
      )

      device(file, width = input$width, height = input$height)
      data <- dataset()[, input$selected_cols, drop = FALSE]

      sets <- lapply(data, function(col) unique(col))
      names(sets) <- colnames(data)

      colors <- c(input$color1, input$color2, input$color3, input$color4, input$color5)[1:length(sets)]

      venn.plot <- venn.diagram(
        x = sets,
        category.names = colnames(data),
        fill = colors,
        filename = NULL, # 设置为NULL，避免保存文件
        output = TRUE
      )
      grid.draw(venn.plot)
      dev.off()
    }
  )

}
