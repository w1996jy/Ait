#' GO Bar Plot UI
#'
#' Creates the user interface for the GO Bar Plot application, allowing users to upload a CSV file,
#' select colors for different categories, and customize plot labels and dimensions.
#' @import shiny
#' @name GO_bar_class_ui
GO_bar_class_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("GO Bar Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        colourpicker::colourInput(ns("bp"), "Biological process color", value = "#F27FB2"),
        colourpicker::colourInput(ns("cc"), "Cellular component color", value = "#A3C8F7"),
        colourpicker::colourInput(ns("mf"), "Molecular function color", value = "#ED8000"),
        textInput(ns("xlable"), "X Label:", value = ""),
        textInput(ns("ylable"), "Y Label:", value = "Count"),
        textInput(ns("group"), "Group Label:", value = "Group"),
        numericInput(ns("width"), "Plot Width (inches):", value = 12, min = 3, max = 20),
        numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("downloadPlot"), "Download PDF")
      ),
      mainPanel(
        plotOutput(ns("goBarPlot"))  # 显示生成的图表
      )
    )
  )
}
#' GO Bar Plot Server
#'
#' Handles the server-side logic for the GO Bar Plot application, including reading uploaded data,
#' generating the plot based on user inputs, and enabling PDF downloads with specified dimensions.
#' @import ggplot2
#' @name GO_bar_class_server
GO_bar_class_server <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read.csv(input$file$datapath)  # Read the CSV file
  })
  
  # 监听"Run"按钮的点击事件并生成图表
  observeEvent(input$run, {
    output$goBarPlot <- renderPlot({
      req(uploaded_data())  # Ensure data is available
      df <- uploaded_data()
      
      ggplot(df, aes(x = Description, y = Count, fill = Ontology)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = input$xlable, y = input$ylable) +
        scale_x_discrete(limits = unique(df$Description)) +
        scale_fill_manual(values = c(input$bp, input$cc, input$mf))
    })
  })
  
  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("GO_Bar_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(uploaded_data())  # 确保数据存在
      df <- uploaded_data()
      
      # 绘制与UI一致的图表并保存为PDF
      p <- ggplot(df, aes(x = Description, y = Count, fill = Ontology)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = input$xlable, y = input$ylable) +
        scale_x_discrete(limits = unique(df$Description)) +
        scale_fill_manual(values = c(input$bp, input$cc, input$mf))
      
      print(p)  # 输出图表到PDF文件
      dev.off()  # 关闭PDF设备
    }
  )
}
