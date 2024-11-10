#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @name app_server
#' @export
#'
app_server <- function(input, output, session) {
  # Your application server logic
  bslib::bs_themer()
  callModule(merge_file_server, "merge_file")
  callModule(dataTransform_server, "dataTransform")
  callModule(Long_Wide_Data_T_server, "Long_Wide_Data_T")
  callModule(Grouping_statistics_server, "Grouping_statistics")
  callModule(Grouping_sorting_server, "Grouping_sorting")
  callModule(describe_server, "describe")
  callModule(randomNum_server, "randomNum")
  callModule(cor_server, "cor")
  callModule(corxy_server, "corxy")
  callModule(deseq2_server, "deseq2")
  callModule(mfuzz_server, "mfuzz")
  callModule(kmeans_server, "kmeans")
  callModule(pca_server, "pca")
  callModule(CCA_server, "CCA")
  callModule(OPLS_DA_server, "OPLS_DA")
  callModule(Bar_server, "bar")
  callModule(Venn_server, "Venn")
  callModule(enrichment_bubble_server, "enrichment_bubble")
  callModule(VolcanoPlot_server, "VolcanoPlot")
  callModule(histogram_server, "histogram")
  callModule(upset_server, "upset")
  callModule(GO_bar_class_server, "GO_bar_class")
  callModule(ridgePlot_server, "ridgePlot")
  callModule(boxplot_server, "boxplot")
  callModule(pie_server, "pie")
  callModule(sequence_extract_server, "sequence_extract")
  callModule(reverse_complement_server, "reverse_complement")
  callModule(seqLogo_server, "seqLogo")
}
