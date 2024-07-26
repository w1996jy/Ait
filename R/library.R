library(golem)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(dashboardthemes)
customLogo <- shinyDashboardLogoDIY(
  boldText = "SD"
  ,mainText = "Themes"
  ,textSize = 16
  ,badgeText = "v1.1"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
)

# -------------------------------------------------------------------------

custom_css <- "
.navbar-nav .dropdown-menu {
  top: 20px; /* 调整下拉菜单的位置 */
  left: -50px; /* 调整下拉菜单的位置 */
}
.navbar-nav .dropdown-toggle::after {
  content: none; /* 去掉默认的下拉箭头 */
}
"
# -------------------------------------------------------------------------


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js
#' @importFrom golem favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cloop'
    )
    # Add here other external resources
    # for example, you can add
    # shinyalert::useShinyalert()
  )
}
