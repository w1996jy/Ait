library(shiny)
library(htmltools)
library(shinythemes)

# Web UI 子模块
Web_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2, tags$a(href = "https://itol.embl.de/", tags$img(src = "https://github.com/anhuikylin/Pic/blob/master/Ait/pic/iTOL.png?raw=true", width = "100%")),
             tags$p("iTOL: Phylogenetic Tree Analysis Website", style = "text-align: center;")),
      column(2, tags$a(href = "https://meme-suite.org/meme/", tags$img(src = "https://github.com/anhuikylin/Pic/blob/master/Ait/pic/MEME.png?raw=true", width = "100%")),
             tags$p("MEME: Motif Analysis Toolbox", style = "text-align: center;")),
      column(2, tags$a(href = "http://www.swisstargetprediction.ch/", tags$img(src = "https://github.com/anhuikylin/Pic/blob/master/Ait/pic/SwissDrugDesign.png?raw=true", width = "100%")),
             tags$p("SwissTargetPrediction: Component Target Prediction Database", style = "text-align: center;"))
    )
  )
}