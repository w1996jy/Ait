library(shiny)
library(shinythemes)

# 定义 UI
ui <- navbarPage(
  theme = shinytheme("spacelab"),
  title = "多标签页示例",
  
  # 首页标签
  tabPanel(
    title = "首页",
    fluidPage(
      titlePanel("首页内容"),
      sidebarLayout(
        sidebarPanel("侧边栏内容"),
        mainPanel("主内容区")
      )
    )
  ),
  
  # 数据标签
  tabPanel(
    title = "数据",
    fluidPage(
      titlePanel("数据处理"),
      sidebarLayout(
        sidebarPanel("数据侧边栏"),
        mainPanel("数据主内容区")
      )
    )
  ),
  
  # 分析标签
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
)

# 定义服务器逻辑
server <- function(input, output, session) {}

# 运行应用
shinyApp(ui = ui, server = server)
