#' User Interface for UpSetR Visualization
#'
#' This function defines the UI components for the UpSetR visualization tab,
#' including file input, numeric inputs for plot dimensions, and download buttons.
#'
#' @param id A unique identifier for this UI component.
#' @return A `shiny::tabPanel` object containing the UI elements for UpSetR visualization.
#' @name upset_ui
upset_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "UpSetR Visualization",
    sidebarLayout(
      sidebarPanel(
        # File input for CSV file upload
        fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
        helpText("The uploaded CSV file should have at least two columns."),
        
        # Numeric inputs for plot dimensions
        numericInput(ns("width"), "Width (in inches)", value = 12, min = 1),
        numericInput(ns("height"), "Height (in inches)", value = 6, min = 1),
        
        # Download buttons
        downloadButton(ns("downloadPlot"), "Download Plot"),
        downloadButton(ns("downloadData"), "Download Data"),
        tags$hr(),
        helpText("Upload the CSV file and customize the download options.")
      ),
      mainPanel(
        plotOutput(ns("plot"))
      )
    )
  )
}

#' Server logic for UpSetR Visualization
#'
#' This function contains the server logic for rendering the UpSetR plot and 
#' handling file uploads and downloads. It processes the uploaded CSV data, 
#' generates the UpSetR plot, and provides options to download both the plot 
#' and the processed data.
#'
#' @param input A list of input values from the UI.
#' @param output A list of output values to be rendered in the UI.
#' @param session The Shiny session object.
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @return NULL
#' @name upset_server
upset_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
    df
  })
  
  # Reactive expression to process the data for UpSetR
  upset_data <- reactive({
    df <- dataInput()
    long_df <- df %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Set", values_to = "Name")
    
    long_df %>%
      dplyr::distinct(Name, Set) %>%
      tidyr::pivot_wider(names_from = Set, values_from = Set, values_fill = list(Set = "0")) %>%
      dplyr::mutate(across(-Name, ~ifelse(. == "0", 0, 1))) %>% 
      as.data.frame()
  })
  
  # Reactive expression to generate the UpSetR plot
  plot_obj <- reactive({
    upset_data <- upset_data()
    UpSetR::upset(upset_data, order.by = "freq")
  })
  
  # Render the UpSetR plot
  output$plot <- renderPlot({
    plot_obj()
  })
  
  # Download handler for the UpSetR plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("upset_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = input$width, height = input$height)
      print(plot_obj())
      dev.off()
    }
  )
  
  # Download handler for the processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("upset_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(upset_data(), file, row.names = FALSE)
    }
  )
}
