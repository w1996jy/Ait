library(shiny)
library(readxl)
library(dplyr)
library(writexl)
library(shinyWidgets)
library(shinyFiles)

#' Merge File UI Module
#' @description UI for merging files
#' 
merge_file_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabPanel("File Selection",
             sidebarLayout(
               sidebarPanel(
                 fileInput(ns("file1"), "Choose first Excel file", accept = c(".xlsx")),
                 fileInput(ns("file2"), "Choose second Excel file", accept = c(".xlsx")),
                 uiOutput(ns("sheet_ui1")),
                 uiOutput(ns("sheet_ui2"))
               ),
               mainPanel(
                 uiOutput(ns("col_ui1")),
                 uiOutput(ns("col_ui2")),
                 actionButton(ns("join_btn"), "Join Files"),
                 downloadButton(ns("downloadData"), "Download")
               )
             )
    ),
    tabPanel("Preview",
             mainPanel(
               tableOutput(ns("preview"))
             )
    )
  )
}

#' Merge File Server Module
#' @description Server logic for merging files
#' 
merge_file_server <- function(input, output, session) {
  ns <- session$ns
  
  data1 <- reactive({
    req(input$file1)
    req(input$sheet1)
    read_excel(input$file1$datapath, sheet = input$sheet1)
  })
  
  data2 <- reactive({
    req(input$file2)
    req(input$sheet2)
    read_excel(input$file2$datapath, sheet = input$sheet2)
  })
  
  observe({
    req(input$file1)
    sheets <- excel_sheets(input$file1$datapath)
    updateSelectInput(session, "sheet1", choices = sheets)
  })
  
  observe({
    req(input$file2)
    sheets <- excel_sheets(input$file2$datapath)
    updateSelectInput(session, "sheet2", choices = sheets)
  })
  
  output$sheet_ui1 <- renderUI({
    req(input$file1)
    selectInput(ns("sheet1"), "Select sheet from first file", choices = NULL)
  })
  
  output$sheet_ui2 <- renderUI({
    req(input$file2)
    selectInput(ns("sheet2"), "Select sheet from second file", choices = NULL)
  })
  
  output$col_ui1 <- renderUI({
    req(data1())
    selectInput(ns("col1"), "Select column from first file", choices = colnames(data1()))
  })
  
  output$col_ui2 <- renderUI({
    req(data2())
    selectInput(ns("col2"), "Select column from second file", choices = colnames(data2()))
  })
  
  joined_data <- eventReactive(input$join_btn, {
    req(data1(), data2(), input$col1, input$col2)
    left_join(data1(), data2(), by = setNames(input$col2, input$col1))
  })
  
  output$preview <- renderTable({
    req(joined_data())
    head(joined_data())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("joined_data.xlsx")
    },
    content = function(file) {
      write_xlsx(joined_data(), path = file)
    }
  )
}
