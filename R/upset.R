#' upset_ui  Module
#' @description upset_ui Module
#' @param id A unique identifier for the Shiny namespace.
#' @title upset_ui
#' @name upset_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
upset_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'UpSetR Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
          helpText("The uploaded CSV file should have at least two columns.")
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
                title = 'Download Plot',
                numericInput(ns("width"), "Width (in inches)", value = 12, min = 1),
                numericInput(ns("height"), "Height (in inches)", value = 6, min = 1),
                downloadButton(ns("downloadPlot"), "Download")
                ),
              accordion_panel(
                title = 'Download Data',
                downloadButton(ns("downloadData"), "Download")
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
#' upset_server Module
#' @description upset_server Module
#' @name upset_server
#' @title upset_server
#' @param input description
#' @param output description
#' @param session description
#' @importFrom utils read.csv write.csv
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom UpSetR upset
#' @import ggplot2
#' @export
#'
utils::globalVariables(c("Name", "Set"))
upset_server <- function(input, output, session) {
  ns <- session$ns

  # Reactive expression to read the uploaded CSV file
  dataInput <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    shiny::validate(need(ncol(df) >= 2, "The CSV file must have at least two columns"))
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
