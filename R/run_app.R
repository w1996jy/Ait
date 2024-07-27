#' @export run_Ait
#' @importFrom golem with_golem_options 
#' @importFrom shiny shinyApp
#' @inheritParams shiny::shinyApp

# list.files(path = "E:/Rapp/Ait/R")

# 运行"E:/Rapp/Ait/R"文件夹下除"run_app.R"以外的所有文件 ---------------------------------------------------
folder_path <- "E:/Rapp/Ait/R"
all_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)
files_to_run <- setdiff(all_files, file.path(folder_path, "run_app.R"))
for (file in files_to_run) {
  source(file)
}

run_Ait <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    maxRequestSize = 100,
    ...
) {
  options(shiny.maxRequestSize = maxRequestSize * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = list(launch.browser = TRUE),
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
# run_Ait()
