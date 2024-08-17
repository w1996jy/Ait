#' User Interface for Word Cloud Module
#'
#' This function creates a user interface for the Word Cloud module in a Shiny application.
#' It allows users to upload a CSV file and set parameters for generating a word cloud.
#'
#' @param id A unique identifier for the UI elements to handle namespaces in Shiny modules.
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel fileInput tags checkboxInput
#' @importFrom shiny selectInput numericInput downloadButton mainPanel
#' @importFrom wordcloud2 wordcloud2Output
#' @return Returns a tab panel that includes a sidebar for inputs and a main panel for word cloud output.
#' @export
wordcloud2_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Word Cloud with wordcloud2",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("textFile"), "Choose a CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput(ns("removeNumbers"), "Remove Numbers", TRUE),
        selectInput(ns("format"), "Select File Format", choices = c("PNG", "HTML")),
        numericInput(ns("imgWidth"), "Image Width", 800),
        numericInput(ns("imgHeight"), "Image Height", 600),
        downloadButton(ns("downloadImage"), "Download Word Cloud")
      ),
      mainPanel(
        wordcloud2Output(ns("wordcloud"))
      )
    )
  )
}
#' Server Logic for Word Cloud Module
#'
#' This function contains the server-side logic for the Word Cloud module. It processes the uploaded
#' CSV file, generates a word cloud, and handles the download of the word cloud in PNG or HTML format.
#'
#' @param input Shiny server input.
#' @param output Shiny server output.
#' @param session Shiny server session.
#' @importFrom shiny reactive downloadHandler
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @return Does not return anything; it registers reactive values and outputs for a Shiny module.
#' @export
wordcloud2_server <- function(input, output, session) {
  textData <- reactive({
    inFile <- input$textFile
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    if (input$removeNumbers) {
      df$word <- gsub('[0-9]+', '', df$word)
    }
    df <- df[!df$word == "",]
    df
  })
  
  output$wordcloud <- renderWordcloud2({
    df <- textData()
    if (is.null(df))
      return(NULL)
    
    wordcloud2(df, size = 0.5, color = 'random-light', shape = 'circle')
  })
  
  output$downloadImage <- downloadHandler(
    filename = function() {
      paste("wordcloud-", Sys.Date(), switch(input$format, "PNG" = ".png", "HTML" = ".html"))
    },
    content = function(file) {
      df <- textData()
      if (!is.null(df)) {
        tempFile <- tempfile(fileext = ".html")
        widget <- wordcloud2(df, size = 0.5, color = 'random-light', shape = 'circle')
        htmlwidgets::saveWidget(widget, tempFile, selfcontained = TRUE)
        
        if (input$format == "PNG") {
          webshot::webshot(tempFile, file, delay = 5, vwidth = input$imgWidth, vheight = input$imgHeight, zoom = 2)
        } else if (input$format == "HTML") {
          file.copy(tempFile, file)
        }
      }
    }
  )
}
