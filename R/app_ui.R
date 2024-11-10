#' @title Shiny App UI Function
#' @description This function defines the user interface (UI) for the Shiny app.
#' @param request Shiny request
#' @name app_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(bootswatch = "lumen"),  # 选择默认主题
      title = "Ait",
      nav_panel(
        "Home page",
        icon = bs_icon("bank"),
        homepage_ui("homepage")
      ),
      nav_menu(
        "Data processing",
        icon = bs_icon("card-checklist"),
        merge_file_ui("merge_file"),
        dataTransform_ui("dataTransform"),
        Long_Wide_Data_T_ui("Long_Wide_Data_T"),
        Grouping_statistics_ui("Grouping_statistics"),
        Grouping_sorting_ui("Grouping_sorting"),
        describe_ui("describe"),
        randomNum_ui("randomNum")
      ),
      nav_menu(
        "Analyse",
        icon = bs_icon("tools"),
        cor_ui("cor"),
        corxy_ui("corxy"),
        deseq2_ui("deseq2"),
        mfuzz_ui("mfuzz"),
        kmeans_ui("kmeans"),
        pca_ui("pca"),
        CCA_ui("CCA"),
        OPLS_DA_ui("OPLS_DA")
      ),
      nav_menu(
        "Plot",
        icon = bs_icon("wrench"),
        bar_ui("bar"),
        Venn_ui("Venn"),
        enrichment_bubble_ui("enrichment_bubble"),
        VolcanoPlot_ui("VolcanoPlot"),
        histogram_ui("histogram"),
        upset_ui("upset"),
        GO_bar_class_ui("GO_bar_class"),
        ridgePlot_ui("ridgePlot"),
        boxplot_ui("boxplot"),
        pie_ui("pie")
      ),
      nav_menu(
        "Sequence",
        icon = bs_icon("check2-all"),
        sequence_extract_ui("sequence_extract"),
        reverse_complement_ui("reverse_complement"),
        seqLogo_ui("seqLogo")
      ),
      nav_panel(
        "Web",
        icon = bs_icon("rocket-takeoff"),
        web_ui("web")
      ),
      nav_panel(
        "Help",
        icon = bs_icon("exclamation-circle"),
        help_ui("help")
      )
      # flexible tools
      # footer = flexible_tools_ui("flexible_tools")
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @name golem_add_external_resources
#' @export
#'
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProteomeAPP"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
