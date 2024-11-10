#' pca UI Module
#' @description pca UI Module
#' @title title
#' @name pca_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
pca_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'PCA Analyse',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload Matrix File", multiple = FALSE),
          fileInput(ns("group"), "Upload Group File", multiple = FALSE)
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
                uiOutput(ns("colorSelectors"))
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("Run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                textInput(ns("width"), "Plot Width (inches):", "8"),
                textInput(ns("height"), "Plot Height (inches)", "6"),
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                downloadButton(ns("downloadBtn2"), "Download Figure"),
                br(),
                downloadButton(ns("downloadPlot"), "Download Table")
              )
              ),
            mainPanel(
              tags$hr(style = "border-color: black; border-width: 5px;"),
              tabsetPanel(
                tabPanel("Input data of Matrix File", DT::dataTableOutput(ns("contentsFile"))),
                tabPanel("Input data of Group File", DT::dataTableOutput(ns("contentsGroup"))),
                tabPanel("Figure", plotOutput(ns("resultPlot1")))
              )
            )
            )
          )
        )
      )
    )
}

#' pca UI pca_server
#' @description pca pca_server
#' @title pca_server
#' @name pca_server
#' @param input A unique identifier for the Shiny namespace.
#' @param output A unique identifier for the Shiny namespace.
#' @param session A unique identifier for the Shiny namespace.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @import DT
#' @import ggplot2
#' @importFrom utils write.table
#' @importFrom utils read.csv
#' @export
#'
utils::globalVariables(c("%||%", "PC1", "PC2","Sample","Group"))
pca_server <- function(input, output, session) {

  yanse <- c('#00f000','#0000f0','#b30000','#f0f000',
             '#00f0f0','#a000f0','#f0a000','#7e3343',
             '#00f0a0','#fb8072','#80b1d3','#fdb462',
             '#b3de69','#fccde5','#bc80bd','#ccebc5',
             '#ffed6f','#64b267','#47a7bd','#f36621',
             '#31629d','#9fde00','#ffbe2a','#ec008c','#ff7404')

  contentsFile <- reactive({
    req(input$file)
    inFile <- input$file
    data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE, row.names = 1)
    data
  })

  contentsGroup <- reactive({
    req(input$group)
    inFile <- input$group
    data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    data
  })

  output$contentsFile <- DT::renderDataTable({
    contentsFile()
  })

  output$contentsGroup <- DT::renderDataTable({
    contentsGroup()
  })

  output$colorSelectors <- renderUI({
    req(input$group)

    group_data <- contentsGroup()
    unique_groups <- unique(group_data$Group)

    lapply(seq_along(unique_groups), function(i) {
      colourpicker::colourInput(inputId = session$ns(paste0("color_", i)),
                                label = paste("Choose color for", unique_groups[i]),
                                value = yanse[i])
    })
  })

  plot_fun1 <- reactive({
    df <- contentsFile()
    df <- as.data.frame(df)
    group <- contentsGroup()
    group <- as.data.frame(group)

    if (nrow(df) == 0 || nrow(group) == 0) {
      return(NULL)
    }

    pca_data <- prcomp(t(df), scale = FALSE)

    df_sample <- data.frame(Sample = rownames(pca_data$x), pca_data$x)
    df_sample <- df_sample %>%
      left_join(group, by = "Sample")

    unique_groups <- unique(df_sample$Group)

    selected_colors <- sapply(seq_along(unique_groups), function(i) {
      input[[paste0("color_", i)]] %||% yanse[i]
    })

    ggplot(df_sample, aes(x = PC1, y = PC2, label = Sample)) +
      geom_point(aes(colour = Group), size = 3, shape = 16) +
      xlab(paste("PC1", "(", round(summary(pca_data)$importance[2, 1]*100, 1), "%)", sep = " ")) +
      ylab(paste("PC2", "(", round(summary(pca_data)$importance[2, 2]*100, 1), "%)", sep = " ")) +
      theme(legend.background = element_rect(colour = "black", size = 0.5)) +
      theme_bw() +
      scale_colour_manual(values = selected_colors) +
      scale_fill_manual(values = selected_colors)
  })

  observeEvent(input$Run, {
    output$resultPlot1 <- renderPlot({
      req(contentsFile(), contentsGroup())
      plot_fun1()
    })
  })

  output$downloadBtn2 <- downloadHandler(
    filename = function() {
      paste("PCA_Plot", input$format, sep = ".")
    },
    content = function(file) {
      ggsave(file, plot = plot_fun1(), device = input$format,
             width = as.numeric(input$width), height = as.numeric(input$height))
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("PCA_Data", "csv", sep = ".")
    },
    content = function(file) {
      df <- contentsFile()
      pca_data <- prcomp(t(df), scale = FALSE)
      write.csv(pca_data$x, file)
    }
  )
}
