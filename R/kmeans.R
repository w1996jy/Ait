#' kmeans UI Module
#' @description kmeans UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @name kmeans_ui
#' @export
#'
kmeans_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Kmeans analyse',
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
            title = "Kmeans plot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput("dropdown", "Data transformation",
                            choices = c("scale","log2", "log10", "no")),
                textOutput("selected"),
                numericInput(ns("num_input"), "Cluster number:", value = 6, min = 2),
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                actionButton(ns("run"), "Run"),
                ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadPlot"), "Download PDF"),
                br(),
                downloadButton(ns("downloadTable"), "Download Table")
                  )
              ),
            mainPanel(
              plotOutput(ns("KmeansPlot"))  # 显示生成的图表
            )
            )
          )
        )
      )
    )
  }
#' @importFrom stats kmeans
#' @importFrom dplyr mutate
#' @param data A data frame or matrix containing the data to be clustered. Rows represent observations,
#' and columns represent features.
#' @param centers An integer specifying the number of clusters to generate in the K-means analysis.
#' @name KmeansR_Ait
#' @title title
#' @export
#'
utils::globalVariables(c("Cluster"))
KmeansR_Ait <- function(data, centers) {
  data_scale <- data.frame(round(t(apply(data, 1, scale)), 2))
  colnames(data_scale) <- colnames(data)
  cl <- kmeans(data_scale, centers = centers)
  data_new <- data.frame("index" = rownames(data_scale), "Cluster" = cl$cluster, data_scale) %>%
    as_tibble() %>%
    dplyr::mutate("Cluster" = paste0("Cluster", Cluster))
  return(data_new)
}

#' @name kmeans_server
#' @title title kmeans_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.table
#' @import KmeansTrendAnalyzer
#' @export
#'
utils::globalVariables(c("Cluster"))
kmeans_server <- function(input, output, session) {
  ns <- session$ns
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read.csv(input$file$datapath,row.names = 1)  # Read the CSV file
  })
  observeEvent(input$run, {
    output$KmeansPlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      KmeansTrendAnalyzer::KmeansR(df,centers = input$num_input,table = TRUE)
    })
  })
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Kmeans_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width
      height <- input$height
      pdf(file, width = width, height = height)
      req(uploaded_data())
      df <- uploaded_data()
      KmeansTrendAnalyzer::KmeansR(df,centers = input$num_input,table = TRUE)
      dev.off()
    }
  )
  # 下载表格
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("kmeans_result", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(uploaded_data())
      df <- uploaded_data()
      result_table <- KmeansR_Ait(df, centers = input$num_input)
      write.csv(result_table, file)
    }
  )


}

