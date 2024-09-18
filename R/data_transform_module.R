# data_transform_module.R
library(shiny)
library(dplyr)
library(DT)
# UI Function for Data Transformation Module
#' Data Transformation and Normalization UI Module
#'
#' @param id Module ID
#' @return A fluidPage containing the UI elements for data transformation and normalization
#' @name dataTransform_ui
dataTransform_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Data Transformation and Normalization"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("datafile"), "Upload Data File (CSV)", accept = ".csv"),
        selectInput(ns("transformation"), "Select Data Transformation Method",
                    choices = c("None" = "none", 
                                "Log2" = "log2", 
                                "Natural Log (LN)" = "ln", 
                                "Log10" = "log10", 
                                "Power2" = "power2", 
                                "Square Root (Sqrt)" = "sqrt")),
        selectInput(ns("normalization"), "Select Normalization Method",
                    choices = c("None" = "none", 
                                "Auto" = "auto", 
                                "Range" = "range", 
                                "MinMax" = "minmax", 
                                "MaxAbs" = "maxabs", 
                                "Log" = "log", 
                                "Vast" = "vast", 
                                "Pareto" = "pareto", 
                                "Level" = "level", 
                                "Robust" = "robust", 
                                "Median" = "median", 
                                "Center" = "center")),
        actionButton(ns("run"), "Run"),
        br(), br(),
        downloadButton(ns("download"), "Download Table")
      ),
      
      mainPanel(
        DTOutput(ns("dataTable"))  # Use DTOutput for interactive table
      )
    )
  )
}
# Server Function for Data Transformation Module
#' Data Transformation and Normalization Server Module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return None
#' @name dataTransform_server
dataTransform_server <- function(input, output, session) {
  
  # Reactive expression to read data with first column as row names
  data <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath, row.names = 1)
    df
  })
  
  # Reactive expression to apply transformations and normalization
  transformed_data <- reactive({
    req(input$run)  # Ensure 'Run' button is clicked before processing
    df <- data()
    
    # Apply transformation
    if (input$transformation == "log2") {
      df <- df %>% mutate(across(everything(), log2))
    } else if (input$transformation == "ln") {
      df <- df %>% mutate(across(everything(), log))
    } else if (input$transformation == "log10") {
      df <- df %>% mutate(across(everything(), log10))
    } else if (input$transformation == "power2") {
      df <- df %>% mutate(across(everything(), function(x) x^2))
    } else if (input$transformation == "sqrt") {
      df <- df %>% mutate(across(everything(), sqrt))
    }
    
    # Apply normalization
    if (input$normalization == "range" || input$normalization == "minmax") {
      df <- df %>% mutate(across(everything(), ~ (. - min(.)) / (max(.) - min(.))))
    } else if (input$normalization == "maxabs") {
      df <- df %>% mutate(across(everything(), ~ . / max(abs(.))))
    } else if (input$normalization == "log") {
      df <- df %>% mutate(across(everything(), function(x) log(x + 1)))
    } else if (input$normalization == "vast") {
      # Placeholder for 'vast' method
    } else if (input$normalization == "pareto") {
      df <- df %>% mutate(across(everything(), function(x) (x - mean(x)) / sqrt(sd(x))))
    } else if (input$normalization == "level") {
      # Placeholder for 'level' method
    } else if (input$normalization == "robust") {
      df <- df %>% mutate(across(everything(), function(x) (x - median(x)) / IQR(x)))
    } else if (input$normalization == "median") {
      df <- df %>% mutate(across(everything(), function(x) x - median(x)))
    } else if (input$normalization == "center") {
      df <- df %>% mutate(across(everything(), function(x) x - mean(x)))
    }
    
    df
  })
  
  output$dataTable <- renderDT({
    req(input$run)
    datatable(transformed_data(), rownames = TRUE)  # Display row names
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("transformed_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(transformed_data(), file, row.names = TRUE)
    }
  )
}
