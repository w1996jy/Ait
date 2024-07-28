#' Integrate server interface
#' 
#' @description Integrate server interface
#' 
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
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
}
