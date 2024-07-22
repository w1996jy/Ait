app_ui <- function(request) {
  tagList(
    # 将此函数用于添加外部资源。
    # golem_add_external_resources(),
    # 应用程序 UI 逻辑
    navbarPage(
      theme = shinytheme("spacelab"),
      title = "Ait",
      # 首页标签
      homepage_ui("home_id"),
      # 数据标签
      first_tab_ui("first_tab_id"),
      # 分析标签
      second_tab_ui("second_tab_id"),
      # 分析标签
      third_tab_ui("second_tab_id")
    )
  )
}
