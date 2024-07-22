app_server <- function(input, output, session) {}
app_server <- function(input, output, session) {
  output$file_table <- renderTable({
    req(input$file_upload)
    file <- input$file_upload$datapath
    read.csv(file)
  })
  
  output$selected_color <- renderText({
    paste("选中的颜色是：", input$color_picker)
  })
  
  output$selected_option <- renderText({
    paste("选中的选项是：", input$dropdown_menu)
  })
  
  output$selected_checkboxes <- renderText({
    paste("选中的复选框选项是：", paste(input$checkbox_group, collapse = ", "))
  })
  
  output$switch_status <- renderText({
    if (input$toggle_switch) {
      "开关按钮是开启的"
    } else {
      "开关按钮是关闭的"
    }
  })
}