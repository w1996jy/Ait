
#' Grouping_sorting UI Module
#' @description Merge File UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
Grouping_sorting_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    title = 'Grouping sorting',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
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
                selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
                selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
                selectInput(ns("summary_func"), "Select summary function:",
                            choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),

                selectInput(ns("sort_order"), "Select sort order:",
                            choices = c("Ascending" = "asc", "Descending" = "desc"))),
              accordion_panel(
                title = "Run",
                actionButton(ns("run"), "Run Summary")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("downloadData"), "Download Table")
              )
              ),
            mainPanel(
              # h3("Grouped and Sorted Summary"),
              DTOutput(ns("summaryTable"))
            )
            )
          )
        )
      )
    )
}

#' @title Grouping sorting Server Function
#' @description This function defines the server logic for grouping statistics in a Shiny application.
#' @param input Shiny input object, used to access input values from the UI.
#' @param output Shiny output object, used to send output values to the UI.
#' @param session Shiny session object, used to manage session-level information.
#' @import readr
#' @import dplyr
#' @export
#'
Grouping_sorting_server <- function(input, output, session) {
  # 读取上传的数据
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, show_col_types = FALSE)  # 隐藏列类型警告
  })

  # 动态更新选择列
  observe({
    req(data())
    updateSelectInput(session, "group_column", choices = names(data()), selected = names(data())[1])
    updateSelectInput(session, "summary_column", choices = names(data()), selected = names(data())[2])
  })

  # 分组统计逻辑
  summaryData <- eventReactive(input$run, {
    req(input$group_column, input$summary_column)  # 确保所需输入存在

    # 根据选择的汇总函数动态生成汇总表达式
    summary_function <- switch(input$summary_func,
                               "mean" = mean,
                               "sum" = sum,
                               "n" = NULL)  # 当选择 n 时，不使用 summary_function

    # 执行分组汇总
    summary_df <- if (input$summary_func == "n") {
      # 对选择的列进行计数
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      # 对选择的列应用 mean 或 sum
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Summary = summary_function(get(input$summary_column), na.rm = TRUE), .groups = "drop")
    }

    # 根据选择的排序顺序进行排序
    if (input$sort_order == "asc") {
      summary_df <- summary_df %>% arrange(across(everything()))
    } else {
      summary_df <- summary_df %>% arrange(desc(across(everything())))
    }

    return(summary_df)
  })

  # 显示汇总表
  output$summaryTable <- renderDT({
    req(summaryData())
    datatable(summaryData(), options = list(pageLength = 10, autoWidth = TRUE))
  })

  # 下载分组统计结果
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("summary_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summaryData(), file, row.names = FALSE)
    }
  )
}
