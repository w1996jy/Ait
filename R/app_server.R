app_server <- function(input, output, session) {
  #> bar
  callModule(Bar_server, "bar")
}
