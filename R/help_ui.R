#' Draw a histogram
#'
#' @description Creates a UI for providing help information about various statistical analysis methods.
#'
#' @param id A time-series omics matrix.
#' @import shiny
#' @name help_ui
#' @noRd
help_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Help",
    fluidPage(
      selectInput(ns("dropdown"), "\u8bf7\u9009\u62e9\u4e00\u4e2a\u9009\u9879\uff1a", 
                  choices = c("OPLS_DA", "PCA", "Venn", "Bar", "Point", "Line")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'OPLS_DA'", ns("dropdown")),
        h4("OPLS_DA"),
        p("OPLS-DA (Orthogonal Projections to Latent Structures Discriminant Analysis) \u662f\u4e00\u79cd\u591a\u5143\u7edf\u8ba1\u5206\u6790\u65b9\u6cd5\uff0c\u4e3b\u8981\u7528\u4e8e\u5206\u7c7b\u548c\u9884\u6d4b\u95ee\u9898\uff0c\u7279\u522b\u5728\u751f\u7269\u4fe1\u606f\u5b66\u3001\u4ee3\u8c08\u7ecf\u7d20\u5b66\u3001\u80f6\u8d44\u7d20\u5b66\u7b49\u9886\u57df\u4e2d\u975e\u5e38\u6d41\u884c\u3002OPLS-DA \u662f OPLS (Orthogonal Projections to Latent Structures) \u65b9\u6cd5\u7684\u4e00\u4e2a\u6269\u5c55\uff0cOPLS \u672c\u8eab\u662f\u4e00\u79cd\u6539\u5584\u7684 PLS (Partial Least Squares) \u65b9\u6cd5\uff0c\u76ee\u7684\u662f\u66f4\u597d\u5730\u5206\u9694\u6a21\u578b\u4e2d\u7684\u9884\u6d4b\u7ed3\u6784\u548c\u7cfb\u7edf\u559c\u5e94\u3002")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'PCA'", ns("dropdown")),
        h4("PCA"),
        p("PCA (Principal Component Analysis) \u662f\u4e00\u79cd\u964d\u7ebf\u6280\u672f\uff0c\u7528\u4e8e\u5c06\u9ad8\u7ebf\u6570\u636e\u6298\u5f71\u5230\u8f83\u4f4e\u7ef4\u5ea6\u7a7a\u95f4\u3002")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Venn'", ns("dropdown")),
        h4("Venn"),
        p("Venn \u56fe\u7528\u4e8e\u663e\u793a\u4e0d\u540c\u96c6\u5408\u4e4b\u95f4\u7684\u4ea4\u96c6\u548c\u5e76\u96c6\u3002")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Bar'", ns("dropdown")),
        h4("Bar"),
        p("Bar \u56fe\u7528\u4e8e\u8868\u793a\u4e0d\u540c\u7c7b\u522b\u7684\u6570\u503c\u5206\u5e03\uff0c\u5e38\u7528\u4e8e\u6bd4\u8f83\u5404\u7c7b\u522b\u4e4b\u95f4\u7684\u5dee\u5f02\u3002")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Point'", ns("dropdown")),
        h4("Point"),
        p("Point \u56fe\u7528\u4e8e\u663e\u793a\u6570\u636e\u70b9\u7684\u5206\u5e03\u548c\u8d8b\u52bf\uff0c\u5e38\u7528\u4e8e\u6563\u70b9\u56fe\u3002")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Line'", ns("dropdown")),
        h4("Line"),
        p("Line \u56fe\u7528\u4e8e\u663e\u793a\u6570\u636e\u968f\u65f6\u95f4\u6216\u5176\u4ed6\u8f7b\u52a8\u53d8\u91cf\u7684\u53d8\u5316\u8d8b\u52bf\uff0c\u5e38\u7528\u4e8e\u65f6\u95f4\u5e8f\u5217\u5206\u6790\u3002")
      )
    )
  )
}
