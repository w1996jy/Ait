#' randomNum UI Module
#' @description randomNum UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
randomNum_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'randomNum',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameter",
          selectInput(ns("distribution"), "Select Distribution:",
                      choices = c("Uniform" = "uniform",
                                  "Normal" = "normal",
                                  "Binomial" = "binomial",
                                  "Poisson" = "poisson")),
          numericInput(ns("n"), "Number of Random Numbers:", value = 10, min = 1),

          conditionalPanel(
            condition = "input.distribution == 'normal'",
            numericInput(ns("mean"), "Mean:", value = 0),
            numericInput(ns("sd"), "Standard Deviation:", value = 1)
          ),

          conditionalPanel(
            condition = "input.distribution == 'binomial'",
            numericInput(ns("size"), "Number of Trials:", value = 10),
            numericInput(ns("prob"), "Probability of Success:", value = 0.5, min = 0, max = 1)
          ),

          conditionalPanel(
            condition = "input.distribution == 'poisson'",
            numericInput(ns("lambda"), "Lambda (Mean):", value = 3)
          )
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("generate"), "Run")
      ),
      accordion_panel(
        title = "Download",
        downloadButton(ns("downloadData"), "Download Table")
      )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            mainPanel(
              DTOutput(ns("resultsTable"))  # 显示结果表格
            )
            )
          )
        )
      )
    )
  }
# Server部分
#' @description
#' A short description...
#' @name randomNum_server
#' @title randomNum_server
#' @param input description
#' @param output description
#' @param session description
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
randomNum_server <- function(input, output, session) {
  ns <- session$ns
  result_data <- reactiveVal(NULL)  # 存储生成的随机数

  observeEvent(input$generate, {
    req(input$n)
    result <- switch(input$distribution,
                     uniform = runif(input$n),
                     normal = rnorm(input$n, mean = input$mean, sd = input$sd),
                     binomial = rbinom(input$n, size = input$size, prob = input$prob),
                     poisson = rpois(input$n, lambda = input$lambda))

    result_data(data.frame(Random_Numbers = result))  # 存储为数据框

    output$resultsTable <- renderDT({
      datatable(result_data(), options = list(pageLength = 5))
    })
  })

  # 下载CSV文件
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("random_numbers_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(result_data(), file)
    }
  )
}
