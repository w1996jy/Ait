library(shiny)
library(clusterProfiler)
library(dplyr)
library(DT)

# UI部分
KEGG_ann_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("KEGG Pathway Annotation"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        actionButton(ns("run"), "Run"),
        downloadButton(ns("download"), "Download Results"),
        helpText("Please upload a CSV file with a column named 'index' containing KEGG IDs.")
      ),
      mainPanel(
        DT::dataTableOutput(ns("results"))  # 显示结果的DT表格
      )
    )
  )
}

# Server部分
KEGG_ann_server <- function(input, output, session) {
  observeEvent(input$run, {
    req(input$file)  # 确保文件已上传
    
    # 显示进度条
    withProgress(message = "Processing...", value = 0, {
      # 读取CSV文件
      kegg_data <- tryCatch({
        read.csv(input$file$datapath)
      }, error = function(e) {
        showNotification("Error reading CSV file. Please check the file format.", type = "error")
        return(NULL)
      })
      
      if (is.null(kegg_data)) return()  # 如果读取失败，停止后续处理
      
      incProgress(0.2)  # 更新进度条
      
      # 检查是否存在名为"index"的列
      if (!"index" %in% colnames(kegg_data)) {
        showNotification("The uploaded file must contain a column named 'index'.", type = "error")
        return()
      }
      
      # 提取KEGG ID
      k <- kegg_data$index  # 使用上传文件中的"index"列
      
      # 进行KEGG转换并替换
      x <- bitr_kegg(k, "kegg", "Path", "ko") %>%
        mutate(Path = gsub("ko", "map", Path))
      incProgress(0.5)  # 更新进度条
      
      # 获取KO名称
      y <- ko2name(x$Path)
      incProgress(0.2)  # 更新进度条
      
      # 将结果与KO名称合并，去掉第三列
      results <- cbind(x, KO_Name = y) %>%
        select(-KO_Name.ko)  # 去掉不需要的列
      
      # 将结果存储为反应式
      output$results <- DT::renderDataTable({
        datatable(results, options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # 下载结果
      output$download <- downloadHandler(
        filename = function() {
          paste("KEGG_Annotation_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file, row.names = FALSE)
        }
      )
    })
  })
}
