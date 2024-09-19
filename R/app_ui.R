#' Application User Interface
#' @description Defines the user interface of the Shiny application.
#' @param request Internal parameter for {shiny}. Do not remove.
#' @import shiny
#' @import shinythemes
#' @name app_ui
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      theme = shinytheme("readable"),
      customLogo(version = "V.1.0.0"),
      tabPanel("Home", homepage_ui("home_id")),
      navbarMenu(
        "Analysis",
        tabPanel("Merge File", merge_file_ui("merge_file")),
        tabPanel("dataTransform",dataTransform_ui("dataTransform")),
        tabPanel("Correlation Analysis", cor_ui("cor_id")),
        tabPanel("Descriptive Statistics", describe_ui("describe_id")),
        tabPanel("DESeq2", deseq2_ui("deseq2_id")),
        tabPanel("Mfuzz Clustering", mfuzz_ui("mfuzz"))
      ),
      navbarMenu(
        "Plot",
        tabPanel("Bar", bar_ui("bar")),
        tabPanel("Venn", Venn_ui("venn")),
        tabPanel("Enrichment Bubble", enrichment_bubble_ui("enrichment_bubble")),
        tabPanel("Volcano Plot", VolcanoPlot_ui("volcano_plot")),
        tabPanel("wordcloud Plot", wordcloud2_ui("wordcloudModule")),
        tabPanel("Histogram", histogram_ui("histogram")),
        tabPanel("PCA Analyse", pca_ui("pca")),
        tabPanel("Correlation Analysis of Two Traits", corxy_ui("correlation1")),
        tabPanel("UpSetR Visualization", upset_ui("upset")),
        tabPanel("GO Bar Plot", GO_bar_class_ui("GO_bar_class")),
        tabPanel("OPLS-DA Analyse", OPLS_DA_ui("OPLS_DA")),
        tabPanel("CCA Analyse", CCA_ui("CCA"))
      ),
      navbarMenu(
        "Sequence",
        tabPanel("Sequence Extract", Sequence_extract_ui("sequence_extract")),
        tabPanel("Reverse Complement", reverse_complement_ui("reverse_complement")),
        tabPanel("Sequence Logo",seqLogo_ui("seqLogoModule"))
      ),
      navbarMenu(
        "Convert",
        tabPanel("SVG to ...", svg_ui("svg_converter"))
      ),
      tabPanel(
        "Web",
        Web_ui("Web_id")
      ),
      tabPanel(
        "Help",
        help_ui("project_init_id"))
    )
  )
  
}