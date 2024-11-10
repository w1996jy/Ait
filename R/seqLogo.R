#' seqLogo UI Module
#' @description seqLogo UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @param title The title of the sequence logo UI, default is "Sequence Logo".
#' @title seqLogo ui Module
#' @name seqLogo_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
seqLogo_ui <- function(id, title = "Sequence Logo") {
  ns <- NS(id)
  nav_panel(
    title = 'Sequence Logo',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Choose DNA Sequence File", accept = c(".fa", ".fasta"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Figure of result",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Download',
                radioButtons(ns("format"), "Select Download Format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg")),
                numericInput(ns("width"), "Width of the Image:", value = 8, min = 5, max = 20),
                numericInput(ns("height"), "Height of the Image:", value = 6, min = 5, max = 20),
                downloadButton(ns("downloadBtn"), "Download")
                )
              ),
            mainPanel(
              plotOutput(ns("logoPlot"))
            )
            )
          )
        )
      )
    )
}
#' Server logic for DNA Sequence Logo Module
#'
#' Handles file input, rendering of the sequence logo plot, and downloading of the plot.
#' @param input Shiny server input
#' @param output Shiny server output
#' @param session Shiny session object
#' @importFrom ggseqlogo ggseqlogo
#' @importFrom Biostrings readDNAStringSet
#' @importFrom ggplot2 ggsave
#' @name seqLogo_server
#' @title seqLogo_server
#' @export
#'
seqLogo_server <- function(input, output, session) {
  ns <- session$ns

  # Render DNA sequence logo plot
  output$logoPlot <- renderPlot({
    req(input$file1)
    inFile <- input$file1

    dna_seqs <- readDNAStringSet(inFile$datapath)
    dna_seqs <- as.character(dna_seqs)

    if (length(dna_seqs) > 0) {
      ggseqlogo(dna_seqs, seq_type = "dna")
    } else {
      plot.new()
      text(0.5, 0.5, "No valid sequences found.", cex = 1.5)
    }
  })

  # Handle the download of the sequence logo
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("sequence-logo.", input$format, sep = "")
    },
    content = function(file) {
      req(input$file1)
      inFile <- input$file1
      dna_seqs <- readDNAStringSet(inFile$datapath)
      dna_seqs <- as.character(dna_seqs)
      plot <- ggseqlogo(dna_seqs, seq_type = "dna")

      ggsave(file, plot = plot, device = input$format, width = input$width, height = input$height)
    }
  )
}
