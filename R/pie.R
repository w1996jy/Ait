#' pie_ UI Module
#' @description pie_ UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title pie_ui
#' @name pie_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
pie_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Pie Plot',
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
                uiOutput(ns("colorSelectors")),
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("generate"), "Run"),
              ),
              accordion_panel(
                title = 'Download Table',
                downloadButton(ns("downloadData"), "Download")
              ),
              accordion_panel(
                title = 'Download Plot',
                numericInput(ns("width"), "Download Width (inches):", value = 8, min = 1),
                numericInput(ns("height"), "Download Height (inches):", value = 6, min = 1),
                downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            mainPanel(
              # 创建一个新的面板，包含DT表格和绘图
              tabsetPanel(
                tabPanel(
                  title = "Plot",
                  plotOutput(ns("pieChart"))
                ),
                tabPanel(
                  title = "Data Table",
                  DTOutput(ns("table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' pie_server Module
#' @description Server logic for pie
#' @name pie_server
#' @title pie_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import dplyr
#' @import DT
#' @import colourpicker
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom writexl write_xlsx
#' @importFrom utils write.table
#' @export
#'
utils::globalVariables(c("Classification", "colors", "Percentage"))
pie_server <- function(input, output, session) {
  ns <- session$ns
  classification_counts <- reactiveVal()
  plot_obj <- reactiveVal()  # 保存生成的plot对象

  observeEvent(input$generate, {
    req(input$file)

    data <- read.csv(input$file$datapath)

    # 检查是否存在“Classification”列
    req("Classification" %in% names(data))

    class_counts <- data %>%
      group_by(Classification) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)

    classification_counts(class_counts)

    output$colorSelectors <- renderUI({
      lapply(1:nrow(class_counts), function(i) {
        colourpicker::colourInput(ns(paste("color", i, sep = "_")),
                                  paste("Select Color for", class_counts$Classification[i]),
                                  value = sample(colors(), 1))
      })
    })

    observe({
      req(input$file)

      color_values <- sapply(1:nrow(class_counts), function(i) {
        input[[paste("color", i, sep = "_")]]
      })

      plot <- ggplot(class_counts, aes(x = "", y = Count, fill = Classification)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        theme(legend.title = element_blank()) +
        geom_text(aes(label = paste(Count, "(", round(Percentage, 1), "%)", sep = "")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = color_values)  # 使用动态生成的颜色

      plot_obj(plot)  # 保存plot对象以便下载

      output$pieChart <- renderPlot({
        print(plot)  # 显示生成的饼图
      })
    })

    output$table <- renderDT({
      datatable(class_counts)
    })
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("pie_chart_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      plot <- plot_obj()  # 获取生成的plot对象

      # 使用ggsave保存为PDF
      ggsave(file, plot = plot, device = "pdf", width = input$width, height = input$height)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("classification_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(classification_counts(), file, row.names = FALSE)
    }
  )
}
