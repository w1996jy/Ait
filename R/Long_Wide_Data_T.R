Long_Wide_Data_T_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Long-Wide Data Transformation"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        radioButtons(ns("transformation"), "Select transformation type:",
                     choices = list("Long format" = "long", "Wide format" = "wide")),
        uiOutput(ns("columns")), # 动态列选择
        actionButton(ns("transform"), "Transform Data"),
        downloadButton(ns("downloadData"), "Download Transformed Data")
      ),
      
      mainPanel(
        h3("Original Data"),
        DTOutput(ns("originalData")),  # 显示上传的原始数据
        h3("Transformed Data"),
        DTOutput(ns("dataTable"))      # 显示转换后的数据
      )
    )
  )
}
Long_Wide_Data_T_server <- function(input, output, session) {
  ns <- session$ns
  
  # 读取上传的文件
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  # 动态生成选择列的 UI
  output$columns <- renderUI({
    req(data())
    
    if(input$transformation == "long") {
      checkboxGroupInput(ns("cols"), "Select columns to pivot to long format:", 
                         choices = names(data()), selected = names(data())[1:2])
    } else {
      selectInput(ns("cols"), "Select the column to use as key:", choices = names(data()), selected = names(data())[1])
    }
  })
  
  # 使用 DT 显示上传的原始数据
  output$originalData <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, autoWidth = TRUE))  # 交互式表格显示原始数据
  })
  
  # 进行数据转换
  transformedData <- eventReactive(input$transform, {
    req(input$cols)
    
    if (input$transformation == "long") {
      pivot_longer(data(), cols = all_of(input$cols), names_to = "variable", values_to = "value")
    } else {
      pivot_wider(data(), names_from = all_of(input$cols), values_from = "value")
    }
  })
  
  # 使用 DT 显示转换后的数据
  output$dataTable <- renderDT({
    req(transformedData())
    datatable(transformedData(), options = list(pageLength = 10, autoWidth = TRUE))  # 交互式表格显示转换后的数据
  })
  
  # 提供下载转换后的数据
  output$downloadData <- downloadHandler(
    filename = function() { paste("transformed_data", ".csv", sep = "") },
    content = function(file) {
      write_csv(transformedData(), file)
    }
  )
}
