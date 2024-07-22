## home page ---------------------------------------------------------------

homepage_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "首页",
    fluidPage(
      titlePanel("首页内容"),
      sidebarLayout(
        sidebarPanel("侧边栏内容"),
        mainPanel("主内容区")
      )
    )
  )
}

# first_tab_ui ------------------------------------------------------------

first_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "数据",
    fluidPage(
      titlePanel("数据处理"),
      sidebarLayout(
        sidebarPanel(
          "数据侧边栏",
          fileInput(ns("file_upload"), "上传文件", 
                    accept = c(
                      'text/csv', 
                      'text/comma-separated-values', 
                      'text/plain', 
                      '.csv')
          ),
          colourInput(ns("color_picker"), "选择颜色", value = "#FF0000"),
          selectInput(ns("dropdown_menu"), "选择选项",
                      choices = c("选项 1" = "option1",
                                  "选项 2" = "option2",
                                  "选项 3" = "option3"),
                      selected = "option1"),
          checkboxGroupInput(ns("checkbox_group"), "选择多个选项",
                             choices = c("选项 A" = "optionA",
                                         "选项 B" = "optionB",
                                         "选项 C" = "optionC"),
                             selected = c("optionA", "optionB")),
          # 添加开关按钮
          switchInput(ns("toggle_switch"), "开关", value = TRUE)  # 默认开启
        ),
        mainPanel(
          "first_tab_ui",
          tableOutput(ns("file_table")),
          textOutput(ns("selected_color")),
          textOutput(ns("selected_option")),
          textOutput(ns("selected_checkboxes")),
          textOutput(ns("switch_status"))
        )
      )
    )
  )
}


# second_tab_ui -----------------------------------------------------------

second_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "分析",
    fluidPage(
      titlePanel("数据分析"),
      sidebarLayout(
        sidebarPanel("分析侧边栏"),
        mainPanel("分析主内容区")
      )
    )
  )
}
# third_tab_ui -----------------------------------------------------------

third_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "帮助文档",
    fluidPage(
      titlePanel("help"),
      sidebarLayout(
        sidebarPanel("分析侧边栏"),
        mainPanel("third_tab_ui")
      )
    )
  )
}