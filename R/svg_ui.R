#' UI for SVG conversion
#'
#' @param id Namespace ID
#' @description
#' Creates the UI for SVG conversion
#' @name svg_ui
#'
svg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      theme = shinytheme("spacelab"),
      titlePanel("SVG File Converter"),
      sidebarLayout(
        sidebarPanel(
          fileInput(ns("file"), "Upload SVG File"),
          radioButtons(ns("format"), "Select Output Format:",
                       choices = list("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg")),
          actionButton(ns("convertBtn"), "Convert File"),
          hr(),
          downloadButton(ns("downloadBtn"), "Download Converted File")
        ),
        mainPanel(
          plotOutput(ns("plotOutput"), height = "600px", width = "100%")
        )
      )
    )
  )
}
#' Server for SVG conversion
#'
#' @param input Input object
#' @param output Output object
#' @param session Shiny session object
#' @description
#' Handles the server-side logic for SVG conversion
#' @name svg_server
#'
svg_server <- function(input, output, session) {
  
  svg_file <- reactive({
    req(input$file)
    input$file$datapath
  })
  
  converted_file <- reactiveVal(NULL)
  
  observeEvent(input$convertBtn, {
    req(svg_file())
    
    output_format <- input$format
    temp_file <- tempfile(fileext = paste0(".", output_format))
    
    if (output_format == "pdf") {
      rsvg_pdf(svg_file(), temp_file)
    } else if (output_format == "png") {
      rsvg_png(svg_file(), temp_file)
    } else if (output_format == "jpeg") {
      # Convert SVG to PNG first
      temp_png <- tempfile(fileext = ".png")
      rsvg_png(svg_file(), temp_png)
      # Convert PNG to JPEG
      img <- readPNG(temp_png)
      writeJPEG(img, temp_file)
    }
    
    converted_file(temp_file)
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste0("converted_file.", input$format)
    },
    content = function(file) {
      file.copy(converted_file(), file)
    }
  )
  
  output$plotOutput <- renderPlot({
    req(svg_file())
    plot.new()
    rasterImage(rsvg::rsvg(svg_file()), 0, 0, 1, 1)
  }, res = 96)
}
