#' help UI Module
#' @description help UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title help_ui
#' @name help_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
help_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间以避免ID冲突
  tabPanel(
    title = "Help",
    fluidPage(
      theme = bslib::bs_theme(),  # 添加bslib主题支持
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100vh;",  # 使用 Flexbox 居中
        p("If you want to learn more, please click ", tags$a(href = "https://w1996jy.github.io/Aitcookbook/", "Ait Cookbook", target = "_blank"), "."),  # 添加学习链接
        br(),  # 换行
        p("If you need help while using the software, please contact me at Email: fyliangfei@163.com.")
      )
    )
  )
}
