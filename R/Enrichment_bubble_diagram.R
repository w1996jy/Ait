#' 定义富集气泡图的用户界面
#'
#' @description 创建用于生成富集气泡图的 Shiny 应用程序界面。
#'
#' @param id 模块ID
#'
#' @return 返回一个 Shiny UI 布局。
#' @import shiny
#' @name enrichment_bubble_ui
#' @noRd
enrichment_bubble_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Enrichment Bubble Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        numericInput(ns("num_entries"), "Number of Entries per Group", 10, min = 1),
        tags$hr(),
        checkboxInput(ns("header"), "Header", TRUE),
        textInput(ns("plot_title"), "Plot Title", "Enrichment Bubble Plot"),
        textInput(ns("x_label"), "X-axis Label", ""),
        textInput(ns("y_label"), "Y-axis Label", ""),
        numericInput(ns("num_colors"), "Number of Colors", 2, min = 2),
        uiOutput(ns("color_inputs")),
        tags$hr(),
        numericInput(ns("plot_width"), "Plot Width (inches)", 10, min = 1),
        numericInput(ns("plot_height"), "Plot Height (inches)", 8, min = 1),
        radioButtons(ns("file_type"), "File Type", choices = c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"), selected = "png"),
        downloadButton(ns("downloadPlot"), "Download Plot")
      ),
      mainPanel(
        plotOutput(ns("bubblePlot"))
      )
    )
  )
}

#' 定义富集气泡图的服务器逻辑
#'
#' @description 实现生成富集气泡图的服务器端逻辑。
#'
#' @param id 模块ID
#'
#' @return 无返回值。
#' @importFrom ggplot2 ggplot geom_point scale_color_gradientn theme_bw labs theme
#' @importFrom shiny NS moduleServer
#' @importFrom colourpicker colourInput
#' @importFrom dplyr group_by arrange slice_head ungroup
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggsave
#' @name enrichment_bubble_server
#' @noRd
if (getRversion() >= "2.15.1") utils::globalVariables(c("Group"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Pvalue"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Description"))
if (getRversion() >= "2.15.1") utils::globalVariables(c("Count_all"))
enrichment_bubble_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 动态生成颜色选择输入框
    output$color_inputs <- renderUI({
      num_colors <- input$num_colors
      color_inputs <- lapply(1:num_colors, function(i) {
        colourInput(ns(paste0("color", i)), paste0("Color ", i), value = ifelse(i == 1, "blue", "red"))
      })
      do.call(tagList, color_inputs)
    })
    
    output$bubblePlot <- renderPlot({
      req(input$file1)
      
      data <- read_csv(input$file1$datapath, col_names = input$header)
      
      plot_data <- data %>%
        group_by(Group) %>%
        arrange(Pvalue) %>%
        slice_head(n = as.numeric(input$num_entries)) %>%
        ungroup()
      
      colors <- sapply(1:input$num_colors, function(i) input[[paste0("color", i)]])
      color_scale <- scale_color_gradientn(colors = colors)
      
      ggplot(plot_data, aes(x = Group, y = Description)) +
        geom_point(aes(size = Count_all, color = Pvalue)) +
        color_scale +
        theme_bw() +
        labs(
          title = input$plot_title,
          x = input$x_label,
          y = input$y_label,
          size = "Count",
          color = "P-value"
        ) +
        theme(axis.text.y = element_text(size = 8))
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("Enrichment_Bubble_Plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$file_type, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = input$file_type, width = input$plot_width, height = input$plot_height)
      }
    )
  })
}
