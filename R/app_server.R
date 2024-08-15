#' Integrate server interface
#' @description Integrate server interface
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @importFrom shiny callModule
#' @name app_server
#' @noRd
app_server <- function(input, output, session) {
  #> bar
  callModule(Bar_server, "bar")
  # merge_file
  callModule(merge_file_server, "merge_file")
  # venn diagram
  callModule(Venn_server, "venn")
  # deseq2
  callModule(deseq2_server, "deseq2_id")
  # correlation analysis
  callModule(cor_server, "cor_id")
  callModule(describe_server, "describe_id") # 添加描述性统计模块
  callModule(svg_server, "svg_converter")
  # enrichment bubble plot
  enrichment_bubble_server("enrichment_bubble")
  # sequence_extract
  callModule(Sequence_extract_server, "sequence_extract")
  # reverse_complement
  callModule(reverse_complement_server, "reverse_complement")
  # VolcanoPlot_server 模块
  callModule(VolcanoPlot_server, "volcano_plot")
}
