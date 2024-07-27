#' @import shinythemes
app_ui <- function(request) {
  tagList(
    # 添加外部资源
    # golem_add_external_resources(),
    # 应用程序 UI 逻辑
    navbarPage(
      theme = shinytheme('spacelab'),
      title = "Ait",
      # 首页标签
      homepage_ui("home_id"),
      # 导入文件标签
      navbarMenu(
        title = 'Analysis',
        icon = icon("upload"),
        OPLS_DA_ui("data_import_raw_id"),
        PCA_ui("data_import_tbl_id"),
        Venn_ui("data_import_massdataset_id")
      ),
      # 数据清理标签
      navbarMenu(
        title = 'Plot',
        icon = icon("filter"),
        Bar_ui("bar"),
        Point_ui("data_rm_noise_id"),
        Line_ui("data_rm_outlier_id")
      ),
      # 项目初始化标签
      help_ui("project_init_id")
    )
  )
}