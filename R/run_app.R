#' Run Ait Application
#' 
#' @description 
#' Run the Ait Shiny application.
#' 
#' @export run_Ait
#' @name run_Ait
#' @importFrom golem with_golem_options 
#' @importFrom shiny shinyApp

# 运行指定目录下除"run_app.R"以外的所有R脚本文件
folder_path <- "E:/Rapp/Ait/R"
all_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)
files_to_run <- setdiff(all_files, file.path(folder_path, "run_app.R"))

for (file in files_to_run) {
  source(file)
}

#' @param onStart A function that will be run before the app is started.
#' @param options A list of options to be passed to shiny::shinyApp.
#' @param enableBookmarking Enable bookmarking for the app.
#' @param uiPattern A URL pattern to match Shiny requests.
#' @param maxRequestSize Maximum request size (in MB) for file uploads.
#' @param ... Additional arguments to be passed to golem options.
#' @name run_Ait
#' 
run_Ait <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    maxRequestSize = 100,
    ...
) {
  options(shiny.maxRequestSize = maxRequestSize * 1024^2)  # 设置最大请求大小
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = list(launch.browser = TRUE),  # 启动浏览器
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

run_Ait()
