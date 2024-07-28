app_ui <- function(request) {
  navbarPage(
    theme = shinytheme('spacelab'),
    title = "Ait",
    tabPanel("Home", homepage_ui("home_id")),
    navbarMenu("Analysis",
               tabPanel("Merge File", merge_file_ui("merge_file")),
               tabPanel("OPLS-DA", OPLS_DA_ui("data_import_raw_id")),
               tabPanel("PCA", PCA_ui("data_import_tbl_id")),
               tabPanel("Venn", Venn_ui("venn")),
               tabPanel("DESeq2", deseq2_ui("deseq2_id"))
    ),
    navbarMenu("Plot",
               tabPanel("Bar", Bar_ui("bar")),
               tabPanel("Point Plot", Point_ui("data_rm_noise_id")),
               tabPanel("Line Plot", Line_ui("data_rm_outlier_id"))
    ),
    tabPanel("Help", help_ui("project_init_id"))
  )
}
