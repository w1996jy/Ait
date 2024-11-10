#' GO_bar_class UI Module
#' @description GO_bar_class UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title GO_bar_class_ui
#' @name GO_bar_class_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @export
#'
GO_bar_class_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'GO Bar Plot',
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
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("bp"), "Biological process color", value = "#F27FB2"),
                colourpicker::colourInput(ns("cc"), "Cellular component color", value = "#A3C8F7"),
                colourpicker::colourInput(ns("mf"), "Molecular function color", value = "#ED8000"),
                textInput(ns("xlable"), "X Label:", value = ""),
                textInput(ns("ylable"), "Y Label:", value = "Count"),
                textInput(ns("group"), "Group Label:", value = "Group")
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
              plotOutput(ns("goBarPlot"))  # 显示生成的图表
            )
            )
          )
        )
      )
    )
}

#' GO_bar_class_server Module
#' @description GO_bar_class_server Module
#' @param id A unique identifier for the Shiny namespace.
#' @param input description
#' @param output description
#' @param session description
#' @title GO_bar_class_server
#' @name GO_bar_class_server
#' @import ggplot2
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @importFrom utils read.table
#' @export
#'
utils::globalVariables(c("Count"))
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
