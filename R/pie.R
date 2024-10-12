library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)

# UI函数
pie_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Pie Chart Generator"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        actionButton(ns("generate"), "Run"),
        
        numericInput(ns("width"), "Download Width (inches):", value = 8, min = 1),
        numericInput(ns("height"), "Download Height (inches):", value = 6, min = 1),
        
        uiOutput(ns("colorSelectors")),
        
        downloadButton(ns("downloadPlot"), "Download Plot"),
        downloadButton(ns("downloadData"), "Download Data")
      ),
      
      mainPanel(
        plotOutput(ns("pieChart")),
        DTOutput(ns("table"))
      )
    )
  )
}

# Server函数
pie_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$generate, {
    req(input$file)
    
    data <- read.csv(input$file$datapath)
    
    # 检查是否存在“Classification”列
    req("Classification" %in% names(data))
    
    classification_counts <- data %>%
      group_by(Classification) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    output$colorSelectors <- renderUI({
      lapply(1:nrow(classification_counts), function(i) {
        colourpicker::colourInput(ns(paste("color", i, sep = "_")), 
                                  paste("Select Color for", classification_counts$Classification[i]), 
                                  value = sample(colors(), 1))
      })
    })
    
    output$pieChart <- renderPlot({
      req(input$file)
      
      color_values <- sapply(1:nrow(classification_counts), function(i) {
        input[[paste("color", i, sep = "_")]]
      })
      
      ggplot(classification_counts, aes(x = "", y = Count, fill = Classification)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        theme(legend.title = element_blank()) +
        geom_text(aes(label = paste(Count, "(", round(Percentage, 1), "%)", sep = "")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = color_values)  # 使用动态生成的颜色
    })
    
    
    output$table <- renderDT({
      datatable(classification_counts)
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("pie_chart_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = input$width, height = input$height)
        
        color_values <- sapply(1:nrow(classification_counts), function(i) {
          input[[ns(paste("color", i, sep = "_"))]]
        })
        
        req(all(!is.na(color_values)))
        
        print(ggplot(classification_counts, aes(x = "", y = Count, fill = Classification)) +
                geom_bar(stat = "identity", width = 1) +
                coord_polar(theta = "y") +
                theme_void() +
                theme(legend.title = element_blank()) +
                geom_text(aes(label = paste(Count, "(", round(Percentage, 1), "%)", sep = "")),
                          position = position_stack(vjust = 0.5)) +
                scale_fill_manual(values = color_values))
        dev.off()
      }
    )
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("classification_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(classification_counts, file, row.names = FALSE)
      }
    )
  })
}

