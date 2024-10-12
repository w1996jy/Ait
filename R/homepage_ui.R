#' Homepage UI
#'
#' @description
#' Creates the UI for the homepage.
#'
#' @param id A time-series omics matrix.
#' @import shiny
#' @name homepage_ui
#' @noRd
homepage_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Homepage",
    fluidPage(
      tags$head(
        tags$link(rel = "icon", href = "./img/favicon.ico")  # 替换为你的favicon路径
      ),
      mainPanel("Homepage")
    )
  )
}

