#' ridgePlot UI Module
#' @description ridgePlot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title ridgePlot_ui
#' @name ridgePlot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
ridgePlot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Ridge Plot',
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
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("xcol_ui")),  # 动态生成X轴选择
                uiOutput(ns("ycol_ui")),  # 动态生成Y轴选择
                uiOutput(ns("fillcol_ui")),  # 动态生成填充选择
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
              ),
            mainPanel(
              plotOutput(ns("RidgePlot"))  # 显示生成的图表
            )
            )
          )
        )
      )
    )
}

#' @import ggridges
#' @param input description
#' @param output description
#' @param session description
#' @name ridgePlot_server
#' @title ridgePlot_server
#' @importFrom utils read.table
#' @import ggplot2
#' @export
#'
ridgePlot_server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    df <- read.csv(input$file$datapath)  # Read the CSV file
    return(df)
  })

  output$xcol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("xcol"), "Select X Column:", choices = names(uploaded_data()))
  })

  output$ycol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("ycol"), "Select Y Column:", choices = names(uploaded_data()))
  })

  output$fillcol_ui <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("fillcol"), "Select Fill Column:", choices = names(uploaded_data()))
  })

  observeEvent(input$run, {
    output$RidgePlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      x_var <- input$xcol
      y_var <- input$ycol
      fill_var <- input$fillcol

      ggplot(data = df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
        geom_density_ridges(scale = 1.6, alpha = 0.3) +
        theme_classic()
    })
  })

  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Ridge_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      x_var <- input$xcol
      y_var <- input$ycol
      fill_var <- input$fillcol

      pdf(file, width = input$width, height = input$height)  # 设置 PDF 尺寸
      print(
        ggplot(data = df, aes_string(x = x_var, y = y_var, fill = fill_var)) +
          geom_density_ridges(scale = 1.6, alpha = 0.3) +
          theme_classic()
      )
      dev.off()  # 关闭PDF设备
    }
  )
}
