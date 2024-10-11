kmeans_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Kmeans analyse"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        selectInput("dropdown", "Data transformation", 
                    choices = c("scale","log2", "log10", "no")),
        textOutput("selected"),
        numericInput("num_input", "Cluster number：", value = 6, min = 2),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("KmeansPlot"))  # 显示生成的图表
      )
    )
  )
}

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
      
      KmeansTrendAnalyzer::KmeansR(df,centers = 6,table = TRUE)
    })
  })
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Kmeans_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      KmeansTrendAnalyzer::KmeansR(result,centers = 6,table = TRUE)
      dev.off()  # 关闭PDF设备
    }
  )
}