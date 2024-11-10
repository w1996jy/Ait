#' OPLS_DA UI Module
#' @description OPLS_DA UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title OPLS_DA_ui
#' @name OPLS_DA_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @importFrom colourpicker colourInput
#' @export
#'
OPLS_DA_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'OPLS-DA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("datafile"), "Upload Metabolomics Data (CSV)", accept = ".csv"),
          fileInput(ns("groupfile"), "Upload Group Data (CSV)", accept = ".csv"),
          tableOutput(ns("result_table"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("group1_col"), "Group1 color", value = "#F27FB2"),
                colourpicker::colourInput(ns("group2_col"), "Group2 color", value = "#A3C8F7"),
                textInput(ns("xlable"), "X Label:", value = ""),
                textInput(ns("ylable"), "Y Label:", value = "Count")
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("width"), "Plot Width (inches):", value = 8, min = 3, max = 20),
                numericInput(ns("height"), "Plot Height (inches):", value = 6, min = 3, max = 20),
                downloadButton(ns("downloadPlot"), "Download")
              )
              ),
            mainPanel(
              tags$div(
                style = "text-align: left; margin: 10px 0;",
                tags$span("ropls plot")
              ),
              tags$hr(),  # 插入另一个水平线
              plotOutput(ns("OPLS_DA_Plot")),  # 显示生成的图表
              tags$div(
                style = "text-align: left; margin: 10px 0;",
                tags$span("ggplot2 plot")
              ),
              tags$hr(),  # 插入另一个水平线
              plotOutput(ns("OPLS_DA_ggplot_Plot"))
            )
            )
          )
        )
      )
    )
}
#' OPLS_DA_server Server Module
#' @description Server logic for OPLS_DA_server
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import dplyr
#' @import writexl
#' @import shinyWidgets
#' @import shinyFiles
#' @import ggprism
#' @importFrom grDevices pdf dev.off
#' @importFrom dplyr select pull
#' @importFrom utils write.table read.csv
#' @name OPLS_DA_server
#' @export
#'
utils::globalVariables(c("p1", "o1"))
OPLS_DA_server <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$run, {
    req(input$datafile, input$groupfile)  # Ensure files are uploaded

    # Read uploaded files
    df <- read.csv(input$datafile$datapath, row.names = 1)
    group_data <- read.csv(input$groupfile$datapath)

    # Process data
    group <- data.frame(Sample = colnames(df)) %>%
      left_join(group_data, by = "Sample") %>%
      dplyr::select(Group) %>%
      pull(Group)
    output$OPLS_DA_Plot <- renderPlot({
      # Run OPLS-DA
      ATR_oplsda <- ropls::opls(t(df),
                                group,
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
    })
    output$OPLS_DA_ggplot_Plot <- renderPlot({
      ATR_oplsda <- ropls::opls(t(df),
                                group,
                                predI = 1,
                                orthoI = 1,
                                crossvalI = 6,
                                log10L = TRUE)
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group=group
      OPLS_defentu$label=rownames(OPLS_defentu)
      #设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      ggplot(OPLS_defentu,aes(p1,o1,label = rownames(data)))+
        geom_point(aes(colour=Group),shape=16,size = 3)+
        #theme(legend.background = element_rect(colour="black", size=0.5))+
        labs(x=paste0('T score[1] (',ATR_oplsda@modelDF$R2X[1]*100,'%)'),
             y=paste0('Orthogonal T score[1] (',ATR_oplsda@modelDF$R2X[2]*100,'%)'))+
        stat_ellipse(aes(fill=Group),
                     type = "norm", geom ="polygon",
                     alpha=0.2,color=NA,show.legend = FALSE)+
        scale_fill_manual(values = c(input$group1_col, input$group2_col))+
        scale_color_manual(values = c(input$group1_col, input$group2_col))
    })
  })

  # 下载图表为PDF
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("OPLS-DA_score_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      width <- input$width   # 用户输入的宽度
      height <- input$height # 用户输入的高度
      pdf(file, width = width, height = height)  # 设置 PDF 尺寸
      req(input$datafile, input$groupfile)  # Ensure files are uploaded

      # Read uploaded files
      df <- read.csv(input$datafile$datapath, row.names = 1)
      group_data <- read.csv(input$groupfile$datapath)

      # Process data
      group <- data.frame(Sample = colnames(df)) %>%
        left_join(group_data, by = "Sample") %>%
        dplyr::select(Group) %>%
        pull(Group)
        ATR_oplsda <- ropls::opls(t(df),
                                  group,
                                  predI = 1,
                                  orthoI = 1,
                                  crossvalI = 6,
                                  log10L = TRUE)
      OPLS_defentu <- data.frame(ATR_oplsda@scoreMN,
                                 ATR_oplsda@orthoScoreMN)
      OPLS_defentu$Group=group
      OPLS_defentu$label=rownames(OPLS_defentu)
      #设置适合科研的背景色
      theme_set(ggprism::theme_prism(border = TRUE))
      p <- ggplot(OPLS_defentu,aes(p1,o1,label = rownames(data)))+
        geom_point(aes(colour=Group),shape=16,size = 3)+
        #theme(legend.background = element_rect(colour="black", size=0.5))+
        labs(x=paste0('T score[1] (',ATR_oplsda@modelDF$R2X[1]*100,'%)'),
             y=paste0('Orthogonal T score[1] (',ATR_oplsda@modelDF$R2X[2]*100,'%)'))+
        stat_ellipse(aes(fill=Group),
                     type = "norm", geom ="polygon",
                     alpha=0.2,color=NA,show.legend = FALSE)+
        scale_fill_manual(values = c(input$group1_col, input$group2_col))+
        scale_color_manual(values = c(input$group1_col, input$group2_col))
      print(ATR_oplsda)
      print(p)  # 输出图表到PDF文件
      dev.off()  # 关闭PDF设备
    }
  )

}
