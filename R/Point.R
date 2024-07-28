#' Point UI Module
#' 
#' @description 
#' UI for displaying Point plot
#' 
#' @param id A unique identifier for the module.
#' 
Point_ui <- function(id) {
  tabPanel(
    title = "Point",
    fluidPage(
      mainPanel("Point")
    )
  )
}
