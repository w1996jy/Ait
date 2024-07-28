#' Line_ui
#' 
#' @description
#' Creates a UI for the Line plot tab in the Shiny application.
#'
#' @param id A unique identifier for the module.
#'
#' @import shiny
Line_ui <- function(id) {
  tabPanel(
    title = "Line",  # The title of the tab panel
    fluidPage(
      mainPanel("Line")  # Main panel content for the Line tab
    )
  )
}
