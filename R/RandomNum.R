library(shiny)
library(DT)  # 用于数据表格
library(readr)  # 用于CSV下载

# UI部分
randomNum_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Random Number Generator"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("distribution"), "Select Distribution:",
                    choices = c("Uniform" = "uniform", 
                                "Normal" = "normal", 
                                "Binomial" = "binomial", 
                                "Poisson" = "poisson")),
        
        numericInput(ns("n"), "Number of Random Numbers:", value = 10, min = 1),
        
        conditionalPanel(
          condition = "input.distribution == 'normal'",
          numericInput(ns("mean"), "Mean:", value = 0),
          numericInput(ns("sd"), "Standard Deviation:", value = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribution == 'binomial'",
          numericInput(ns("size"), "Number of Trials:", value = 10),
          numericInput(ns("prob"), "Probability of Success:", value = 0.5, min = 0, max = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribution == 'poisson'",
          numericInput(ns("lambda"), "Lambda (Mean):", value = 3)
        ),
        
        actionButton(ns("generate"), "Generate"),
        downloadButton(ns("downloadData"), "Download CSV")
      ),
      
      mainPanel(
        DTOutput(ns("resultsTable"))  # 显示结果表格
      )
    )
  )
}

# Server部分
randomNum_server <- function(input, output, session) {
  ns <- session$ns
  result_data <- reactiveVal(NULL)  # 存储生成的随机数
  
  observeEvent(input$generate, {
    req(input$n)
    result <- switch(input$distribution,
                     uniform = runif(input$n),
                     normal = rnorm(input$n, mean = input$mean, sd = input$sd),
                     binomial = rbinom(input$n, size = input$size, prob = input$prob),
                     poisson = rpois(input$n, lambda = input$lambda))
    
    result_data(data.frame(Random_Numbers = result))  # 存储为数据框
    
    output$resultsTable <- renderDT({
      datatable(result_data(), options = list(pageLength = 5))
    })
  })
  
  # 下载CSV文件
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("random_numbers_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(result_data(), file)
    }
  )
}