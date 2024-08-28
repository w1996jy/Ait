if (!require('colourpicker')) install.packages("colourpicker")
if (!require('DT')) install.packages("DT")
if (!require('ggplot2')) install.packages("ggplot2")
#' Draw a histogram
#' @description Creates a UI for drawing histograms.
#' @param id A time-series omics matrix.
#' @import shiny
#' @importFrom colourpicker colourInput
#' @import ggplot2
#' @import DT
#' @import elliptic
#' @import rlang
#' @import shinyWidgets
#' @import shinyjs
#' @import dashboardthemes
#' @importFrom utils read.csv
#' @importFrom DT dataTableOutput
#' @name Bar_ui
#' @noRd
bar_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Bar",
    sidebarLayout(
      sidebarPanel(
        id = ns("Sidebar"),
        # 文件上传控件
        fileInput(ns("file_upload"), "Upload CSV file",
                  accept = ".csv"),
        # 下拉菜单选择 x 和 y 轴变量
        selectInput(ns("x_var"), "Select X linkage data:",
                    choices = NULL),
        selectInput(ns("y_var"), "Select Y linkage data:", 
                    choices = NULL),
        textInput(ns("title"), "Enter the title:",value = "tittle"),
        textInput(ns("x_label"), "Enter the X linkage label:",
                  value = "xlabel"),
        textInput(ns("y_label"), "Enter the Y linkage label:",
                  value = "ylabel"),
        textInput(ns("bar_width"), "Enter the bar width:",
                  value = 0.5),
        # 自定义标题、标签字体大小和颜色
        numericInput(ns("title_size"), "Title font size:", 
                     value = 20, min = 1, step = 1),
        colourpicker::colourInput(ns("title_color"), "Title color:", 
                                  value = "black"),
        numericInput(ns("label_size"), "Linkage label font size:", 
                     value = 20, min = 1, step = 1),
        colourpicker::colourInput(ns("label_color"), "Linkage label color:",
                                  value = "black"),
        # 柱子的颜色
        colourpicker::colourInput(ns("bar_color"), "Bar color", 
                                  value = "#03A9F4"),
        # 自定义下载尺寸输入框
        numericInput(ns("width"), "Image width (px):", 
                     value = 800, min = 100, step = 10),
        numericInput(ns("height"), "Image height (px):",
                     value = 600, min = 100, step = 10),
        # 添加下载格式选择下拉菜单
        selectInput(ns("format"), "Select download format:", 
                    choices = c("pdf", "png", "jpg", "svg","eps", 
                                "ps", "tex","jpeg","bmp","wmf")),
        # 添加 Run 按钮
        actionButton(ns("run_button"), "Run"),
        # 添加下载按钮
        downloadButton(ns("download_plot"), "download")
      ),
      mainPanel(
        # 使用 DT 渲染上传文件的内容
        DTOutput(ns("file_content")),
        # 显示图形
        plotOutput(ns("plot_output"))
      )
    )
  )
}

#' Bar Server Function
#'
#' @description Server logic for the Bar UI module.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @import shiny
#' @import ggplot2
#' @name Bar_server
#' @noRd
Bar_server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # 监听文件上传事件
  observeEvent(input$file_upload, {
    req(input$file_upload)  # 确保文件上传控件有文件输入
    # 读取上传的 CSV 文件
    data <- read.csv(input$file_upload$datapath)
    # 将数据存储在 reactive value 中
    uploaded_data(data)
    
    # 更新下拉菜单的选择项
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
  })
  
  # 使用 DT 渲染上传文件的内容
  output$file_content <- renderDT({
    req(uploaded_data())  # 确保有数据可用
    datatable(uploaded_data())  # 使用 DT::datatable 显示数据
  })
  
  # 监听 Run 按钮点击事件
  observeEvent(input$run_button, {
    # 确保有数据可用
    req(uploaded_data())
    
    # 获取上传的数据
    data <- uploaded_data()
    
    # 生成图形
    new_plot <- ggplot(data, aes(
      x = !!sym(input$x_var),
      y = !!sym(input$y_var)
    )) +
      geom_bar(stat = "identity", width = as.numeric(input$bar_width), fill = input$bar_color) +
      labs(
        title = input$title,
        x = input$x_label,
        y = input$y_label
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = input$title_size, color = input$title_color),
        axis.title = element_text(size = input$label_size, color = input$label_color),
        axis.text = element_text(size = input$label_size, color = input$label_color)
      )
    
    # 更新输出图形
    output$plot_output <- renderPlot({
      new_plot
    })
    
    # 处理下载图像的逻辑
    output$download_plot <- downloadHandler(
      filename = function() {
        paste(ifelse(input$title != "", input$title, "plot"), Sys.Date(), ".", input$format, sep = "")
      },
      content = function(file) {
        # 保存图像文件
        ggsave(file, plot = new_plot, width = input$width / 100, height = input$height / 100, units = "in", device = input$format)
      }
    )
  })
}
