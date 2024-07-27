#' Integrate ui interface
#' @import shinythemes
#' @description Integrate ui interface
#' 
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
        tabPanel("Merge Files",
                 merge_file_ui("merge_file")
        ),
        tabPanel("OPLS DA",
                 OPLS_DA_ui("data_import_raw_id")
        ),
        tabPanel("PCA",
                 PCA_ui("data_import_tbl_id")
        ),
        tabPanel("Venn",
                 Venn_ui("venn")
        )
      ),
      # 数据清理标签
      navbarMenu(
        title = 'Plot',
        icon = icon("filter"),
        tabPanel("Bar",
                 Bar_ui("bar")
        ),
        tabPanel("Point",
                 Point_ui("data_rm_noise_id")
        ),
        tabPanel("Line",
                 Line_ui("data_rm_outlier_id")
        )
      ),
      # 项目初始化标签
      navbarMenu(
        title = 'Help',
        icon = icon("info"),
        tabPanel("Help",
                 help_ui("project_init_id")
        )
      )
    )
  )
}
