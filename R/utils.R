#' website logo
#'
#' Ait logo.
#' @return shinyDashboardLogoDIY object.
#' @param version version of metminer
#' @importFrom dashboardthemes shinyDashboardLogoDIY
#' @noRd
#'
customLogo <- function(version) {
  dashboardthemes::shinyDashboardLogoDIY(
    boldText = "Li Lab",
    mainText = "Ait",
    textSize = 14,
    badgeText = version,
    badgeTextColor = "black",
    badgeTextSize = 2,
    badgeBackColor = "#F1C40F",
    badgeBorderRadius = 3
  )
}
