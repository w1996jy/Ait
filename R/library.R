#' Dependency package
#' 
#' @description
#' Load all required packages for the application.
#' @name package
library(golem)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(dashboardthemes)
library(splines)
library(elliptic)
library(shinyjs)
library(ggplot2)
library(DT)
library(rlang)
library(colourpicker)
library(readxl)
library(dplyr)
library(writexl)

#' Custom Logo
#' 
#' @description
#' Create a custom logo for the dashboard.
customLogo <- shinyDashboardLogoDIY(
  boldText = "SD",
  mainText = "Themes",
  textSize = 16,
  badgeText = "v1.1",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#40E0D0",
  badgeBorderRadius = 3
)

# -------------------------------------------------------------------------

#' Custom CSS
#' 
#' @description
#' Add custom CSS to adjust the dropdown menu and toggle styles.
custom_css <- "
.navbar-nav .dropdown-menu {
  top: 20px; 
  left: -50px; 
}
.navbar-nav .dropdown-toggle::after {
  content: none; 
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
#' @name golem_add_external_resources
#' @noRd
golem_add_external_resources <- function() {
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon("app/www/favicon.ico"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Ait'
    )
    # Add here other external resources
    # for example, you can add
    # shinyalert::useShinyalert()
  )
}
