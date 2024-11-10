#' homepage UI Module
#' @description homepage UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title homepage_ui
#' @name homepage_ui
#' @import shiny
#' @import bslib
#' @export
#'
homepage_ui <- function(id) {
  fluidPage(
    # Software Introduction Section
    div(style = "font-family: 'Times New Roman'; width: 50%; margin: auto; margin-bottom: 20px; font-size: 16px;",
        h2("Software Introduction", style = "font-size: 20px;"),
        p("Ait V.1.0.0 is a powerful bioinformatics analysis tool built on the Shiny framework, designed to help researchers and data analysts easily perform data processing, statistical analysis, and visualization. The application features an intuitive user interface that supports various data analysis functions, making it suitable for in-depth exploration of biological and experimental data."),
        h3("Key Features:", style = "font-size: 18px;"),
        tags$ul(
          tags$li("Data Processing: Functions such as file merging, data transformation, long-wide data transformation, grouping statistics, and descriptive statistics enable users to efficiently manage and preprocess their data."),
          tags$li("Analysis Tools: Includes correlation analysis, DESeq2 analysis, clustering analysis (Mfuzz and K-means), PCA, and OPLS-DA, helping users extract important patterns and relationships from their data."),
          tags$li("Rich Visualization Options: Offers various chart generation tools, including bar plots, Venn diagrams, volcano plots, box plots, and pie charts, allowing users to visually present their data results with customizable color choices and download options."),
          tags$li("Sequence Analysis: Supports sequence extraction, reverse complement, and sequence logo operations, facilitating in-depth analysis of genomic and transcriptomic data."),
          tags$li("Web: Many useful external links.")
        ),
        p("With Ait V.1.0.0, users can efficiently process and analyze complex data, enhancing research productivity and supporting scientific discoveries.")
    ),

    # Add Flowchart Section above the image
  )
}
