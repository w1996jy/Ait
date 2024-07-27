#' Draw a histogram
#' @name help_ui
#' @param id A time-series omics matrix.
#' @import shiny
help_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Help",
    fluidPage(
      selectInput(ns("dropdown"), "请选择一个选项:", 
                  choices = c("OPLS_DA", "PCA", "Venn", "Bar", "Point", "Line")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'OPLS_DA'", ns("dropdown")),
        h4("OPLS_DA"),
        p("OPLS-DA (Orthogonal Projections to Latent Structures Discriminant Analysis) 是一种多元统计分析方法，主要用于分类和预测问题，尤其是在生物信息学、代谢组学、蛋白质组学等领域中非常流行。OPLS-DA 是 OPLS (Orthogonal Projections to Latent Structures) 方法的一个扩展，OPLS 本身是一种改进的 PLS (Partial Least Squares) 方法，旨在更好地分离模型中的预测结构和系统噪声。")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'PCA'", ns("dropdown")),
        h4("PCA"),
        p("PCA (Principal Component Analysis) 是一种降维技术，用于将高维数据投影到较低维度空间。")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Venn'", ns("dropdown")),
        h4("Venn"),
        p("Venn 图用于显示不同集合之间的交集和并集。")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Bar'", ns("dropdown")),
        h4("Bar"),
        p("Bar 图用于表示不同类别的数值分布，常用于比较各类别之间的差异。")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Point'", ns("dropdown")),
        h4("Point"),
        p("Point 图用于显示数据点的分布和趋势，常用于散点图。")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Line'", ns("dropdown")),
        h4("Line"),
        p("Line 图用于显示数据随时间或其他连续变量的变化趋势，常用于时间序列分析。")
      )
    )
  )
}
