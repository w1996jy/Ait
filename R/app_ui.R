#' Application User Interface
#'
#' @description Defines the user interface of the Shiny application.
#'
#' @param request Internal parameter for `{shiny}`. 
#'     Do not remove.
#' @name app_ui
#' @noRd
app_ui <- function(request) {
  navbarPage(
    theme = shinytheme('spacelab'),
    title = "Ait",
    tabPanel("Home", homepage_ui("home_id")),
    navbarMenu("Analysis",
               tabPanel("Merge File", merge_file_ui("merge_file")),
               tabPanel("Correlation Analysis", cor_ui("cor_id")),
               tabPanel("Descriptive Statistics", describe_ui("describe_id")),
               tabPanel("DESeq2", deseq2_ui("deseq2_id"))
    ),
    navbarMenu("Plot",
               tabPanel("Bar", Bar_ui("bar")),
               tabPanel("Venn", Venn_ui("venn"))
    ),
    tabPanel("Help", help_ui("project_init_id"))
  )
}
