#' Draw a histogram
#'
#' @description Creates a UI for providing help information about various statistical analysis methods.
#'
#' @param id A unique identifier for the UI component.
#' @import shiny
#' @name help_ui
#' @noRd
help_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间以避免ID冲突
  tabPanel(
    title = "Help",
    fluidPage(
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100vh;",  # 使用 Flexbox 居中
        p("If you want to learn more, please click ", tags$a(href = "https://w1996jy.github.io/Aitcookbook/", "Ait Cookbook", target = "_blank"), "."),  # 添加学习链接
        br(),  # 换行
        p("If you need help while using the software, please contact me at Email: fyliangfei@163.com.")
        
      )
    )
  )
}
