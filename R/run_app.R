# # 运行指定目录下除"run_app.R"以外的所有R脚本文件
# # rm(list = ls())
# folder_path <- "G:/Ait/R"
# all_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)
# files_to_run <- setdiff(all_files, file.path(folder_path, "run_app.R"))
#
# for (file in files_to_run) {
#   source(file)
# }
# -------------------------------------------------------------------------

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
run_app()
