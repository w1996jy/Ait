# 加载所需的库
library(shiny)
library(readr)
library(dplyr)
library(DT)

# UI 函数
Grouping_sorting_ui <- function(id) {
  ns <- NS(id)  # 使用命名空间
  fluidPage(
    titlePanel("Grouping and Sorting Statistics"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        
        selectInput(ns("group_column"), "Select column to group by:", choices = NULL),
        selectInput(ns("summary_column"), "Select column to summarize:", choices = NULL),
        selectInput(ns("summary_func"), "Select summary function:", 
                    choices = c("Mean" = "mean", "Sum" = "sum", "Count" = "n")),
        
        selectInput(ns("sort_order"), "Select sort order:", 
                    choices = c("Ascending" = "asc", "Descending" = "desc")),
        
        actionButton(ns("run"), "Run Summary"),
        downloadButton(ns("downloadData"), "Download Summary")  # 添加下载按钮
      ),
      
      mainPanel(
        h3("Grouped and Sorted Summary"),
        DTOutput(ns("summaryTable"))
      )
    )
  )
}

# 服务器部分
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
