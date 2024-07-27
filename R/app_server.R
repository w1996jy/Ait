#' Integrate server interface
#' @description Integrate server interface
#' 
app_server <- function(input, output, session) {
  #> bar
  # callModule(Bar_server, "bar")
  # merge_file
  callModule(merge_file_server, "merge_file")
  # venn diagram
  callModule(Venn_server, "venn")
}
