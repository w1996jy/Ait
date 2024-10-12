# 加载所需的库
library(shiny)
library(readr)
library(dplyr)
library(DT)

# UI 函数
Grouping_statistics_ui <- function(id) {
  ns <- NS(id)  # 使用命名空间
  fluidPage(
    titlePanel("Grouping Statistics"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
        selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
        selectInput(ns("summary_func"), "Select summary function:", 
                    choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
        
        actionButton(ns("run"), "Run Summary")
      ),
      
      mainPanel(
        h3("Grouped Summary"),
        DTOutput(ns("summaryTable"))
      )
    )
  )
}

# 服务器部分
Grouping_statistics_server <- function(input, output, session) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, show_col_types = FALSE)  # 隐藏列类型警告
  })
  
  # 动态更新选择列
  observe({
    req(data())
    
    # 更新选择输入，确保数据可用时才进行更新
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
    if (input$summary_func == "n") {
      # 对选择的列进行计数
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Count = n(), .groups = "drop")  # 添加 .groups = "drop" 来避免警告
    } else {
      # 对选择的列应用 mean 或 sum
      data() %>%
        group_by(across(all_of(input$group_column))) %>%
        summarise(Summary = summary_function(get(input$summary_column), na.rm = TRUE), .groups = "drop")  # 添加 .groups = "drop"
    }
  })
  
  # 显示汇总表
  output$summaryTable <- renderDT({
    req(summaryData())
    datatable(summaryData(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}