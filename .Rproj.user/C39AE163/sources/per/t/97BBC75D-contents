#' Merge File UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title merge_file
#' @name merge_file_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
merge_file_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Merge File',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose first Excel file", accept = c(".xlsx")),
          fileInput(ns("file2"), "Choose second Excel file", accept = c(".xlsx")),
          uiOutput(ns("sheet_ui1")),
          uiOutput(ns("sheet_ui2"))
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
                mainPanel(
                  uiOutput(ns("col_ui1")),
                  uiOutput(ns("col_ui2"))
                )
              ),
              accordion_panel(
                title = "Run",
                actionButton(ns("join_btn"), "Join Files")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download Table")
              )
            ),
            tabPanel(
              "Preview",
              mainPanel(
                DT::DTOutput(ns("preview")) # 使用 DTOutput 来显示交互式数据表
              )
            )

          )
        )
      )
    )
  )
}
#' Merge File Server Module
#' @description Server logic for merging files
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import readxl
#' @import dplyr
#' @import writexl
#' @import shinyWidgets
#' @import shinyFiles
#' @import DT
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr left_join
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#' @importFrom utils write.table
#' @export
#'
merge_file_server <- function(input, output, session) {
  ns <- session$ns

  data1 <- reactive({
    req(input$file1)
    req(input$sheet1)
    read_excel(input$file1$datapath, sheet = input$sheet1)
  })

  data2 <- reactive({
    req(input$file2)
    req(input$sheet2)
    read_excel(input$file2$datapath, sheet = input$sheet2)
  })

  observe({
    req(input$file1)
    sheets <- excel_sheets(input$file1$datapath)
    updateSelectInput(session, "sheet1", choices = sheets)
  })

  observe({
    req(input$file2)
    sheets <- excel_sheets(input$file2$datapath)
    updateSelectInput(session, "sheet2", choices = sheets)
  })

  output$sheet_ui1 <- renderUI({
    req(input$file1)
    selectInput(ns("sheet1"), "Select sheet from first file", choices = NULL)
  })

  output$sheet_ui2 <- renderUI({
    req(input$file2)
    selectInput(ns("sheet2"), "Select sheet from second file", choices = NULL)
  })

  output$col_ui1 <- renderUI({
    req(data1())
    selectInput(ns("col1"), "Select column from first file", choices = colnames(data1()))
  })

  output$col_ui2 <- renderUI({
    req(data2())
    selectInput(ns("col2"), "Select column from second file", choices = colnames(data2()))
  })

  joined_data <- eventReactive(input$join_btn, {
    req(data1(), data2(), input$col1, input$col2)
    left_join(data1(), data2(), by = setNames(input$col2, input$col1))
  })

  # 使用 DT 包的 renderDT
  output$preview <- DT::renderDT({
    req(joined_data())
    DT::datatable(joined_data()) # 生成交互式数据表
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste("joined_data.xlsx")
    },
    content = function(file) {
      write_xlsx(joined_data(), path = file)
    }
  )
}
