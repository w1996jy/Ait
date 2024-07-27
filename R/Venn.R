library(shiny)
library(VennDiagram)
library(shinyWidgets)
library(ggplot2)
library(grid)

#' Venn Diagram UI Module
#' @description UI for creating Venn diagrams
#' 
Venn_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV file", accept = c(".csv")),
        uiOutput(ns("col_ui")),
        actionButton(ns("plot_venn"), "Plot Venn Diagram"),
        downloadButton(ns("downloadVenn"), "Download Venn Diagram")
      ),
      mainPanel(
        plotOutput(ns("venn_plot"))
      )
    )
  )
}

#' Venn Diagram Server Module
#' @description Server logic for creating Venn diagrams
#' 
Venn_server <- function(input, output, session) {
  ns <- session$ns
  
  dataset <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  output$col_ui <- renderUI({
    req(dataset())
    cols <- colnames(dataset())
    checkboxGroupInput(ns("selected_cols"), "Select columns for Venn Diagram (up to 5)", choices = cols, selected = cols[1:min(5, length(cols))])
  })
  
  output$venn_plot <- renderPlot({
    req(dataset(), input$selected_cols)
    validate(
      need(length(input$selected_cols) <= 5, "Please select up to 5 columns")
    )
    
    data <- dataset()[, input$selected_cols, drop = FALSE]
    
    sets <- lapply(data, function(col) unique(col))
    names(sets) <- colnames(data)
    
    venn.plot <- venn.diagram(
      x = sets,
      category.names = colnames(data),
      fill = c("red", "blue", "green", "yellow", "purple")[1:length(sets)],
      filename = NULL, # 设置为NULL，避免保存文件
      output = TRUE
    )
    grid.draw(venn.plot)
  })
  
  output$downloadVenn <- downloadHandler(
    filename = function() {
      paste("venn_diagram.png")
    },
    content = function(file) {
      png(file)
      data <- dataset()[, input$selected_cols, drop = FALSE]
      
      sets <- lapply(data, function(col) unique(col))
      names(sets) <- colnames(data)
      
      venn.plot <- venn.diagram(
        x = sets,
        category.names = colnames(data),
        fill = c("red", "blue", "green", "yellow", "purple")[1:length(sets)],
        filename = NULL, # 设置为NULL，避免保存文件
        output = TRUE
      )
      grid.draw(venn.plot)
      dev.off()
    }
  )
}
