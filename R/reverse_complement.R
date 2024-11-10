#' reverse_complement UI Module
#' @description reverse_complement UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title reverse_complement_ui
#' @name reverse_complement_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
reverse_complement_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Reverse Complement',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameter",
          radioButtons(ns("complement_type"), "Choose complement type:",
                       choices = list("Forward Complement" = "forward", "Reverse Complement" = "reverse"),
                       selected = "reverse")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("run_btn"), "Run"),
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("download_fasta"), "Download")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Result",
            textAreaInput(ns("input_seq"), "Enter DNA sequence",
                          rows = 10, width  = "100%",
                          placeholder = "Enter DNA sequence...",
                          value = "ATTTATCCTTCAGCCCCTCTAGGCTTAGCAGTCAGCTTGACTTGAGGAGGAAAAAAAAACAACACACCACACTGCATTAATAATGGGGCGATCACCATGCTGTGAGAAGATTGGACTGAAGAAAGGTCCATGGACGCCGGAGGAGGATGAGAAGCTGCTTGCCTTCGTTGAGGAACATGGACACGGGAGCTGGCGGGCATTACCTGCGAAGGCAGGCTTGCAGAGGTGTGGGAAGAGCTGCAGGTTGAGGTGGACAAACTACCTGAGGCCGGACATCAAGAGGGGCAAGTTCAGCTTGCAAGAAGAACAGACCATCATCCAGCTTCACGCTCTTTTAGGCAACAGGTGGTCGGCCATTGCAACACATCTACCAAATCGCACGGACAACGAGATCAAGAACCACTGGAACACGCACCTCAAGAAAAGGCTGGCCAAGATTGGGATCGATCCTGTCACCCACAAATCTACCTGTGGCACTCTCACTGGCACCACGAACGACAGATCAGCCAAGGCCGCGGCAAGCCTCAGCCACATGGCACAATGGGAGAATGCCCGCCTCGAGGCTGAGGCACGGCTGGCTCGAGAATCGAAGACACGAACAGCAACACCGACGCCATCTGCACTCCATGCGCAGCCAATGGATCTACCTGCCTCTGCTGCTTCTCCATGGCTTGACGTGTTGCATGCTTGGCAGGGTGCAAAGATAGACCTGGAGTCACCTACCTCCACACTGACGTTTACAGGGAGCAATGGTGGCATGCTGCCAACCCCCAGGACCAACGGACCAGAGGTATCAGAAAGCAACTCCGCGATGTCGCATTATCAGATGAGCGATGAGTTGGAGGGTGAAGAAACCTATTGGCAGATCTTCAGCAAGCACCAAGTGCCGGAAGTGGACAGCAAGGAGAGTGAAGATGACTTCATTGGCTGTGAGGAGCCGTGGTTCTCAGGGATGGCTGGGGTTGGAGCTGGCATGCTGCTTGATGTATCCAATGAGCATGAGCTATCAGAATGCTGGGGTGAGTCCAGCAGTGGCCAAACTGTTGAGCACAGCAAGCAAGCATCCGATAAGGAGGACAAGGATTATTGGAATTGGGTCCTTGACAGAGTAAACTCAGAGCTGACAGCACAGTCGCCTTCCTTGGTCTAAAATACAGCCCCCCCCCCCCCCCCCCCCCCCCCTGCAGTTTTTTTTTCAACGACTCCTTAGCAAAATCTATTGATCATCTTAAGTCAAAAGAGAAACAGAACATACGAGAAGTACAAATAAATTCTACAGAAGGATGTTTTTTGTCCATGCATGTGAGTGAAATAATTAGCTGTTGTGTGTATGTAGTAAATATTTTTTTCCCTGATTTCTGAATGTATGTTAAGCCTCCAGGACAACAGCAGGGACACTACATTAATGGTCCAAGGACTTTACAGTTGTTAGTTCAACGGCCAAGTTGTCTTGTCTCCCTCTCTTTTTGAATCCATCCCAGAATGAGTAAGAA"),
            mainPanel(
              tags$div(
                style = "width: 100%;",
                verbatimTextOutput(ns("output_seq"))
              )
            )
            )
          )
        )
      )
    )
}
#' reverse_complement_server Module
#' @description Server logic for reverse_complement
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import Biostrings
#' @importFrom readr read_csv
#' @title reverse_complement_server
#' @name reverse_complement_server
#' @export
#'
reverse_complement_server <- function(input, output, session) {
  complement_seq <- eventReactive(input$run_btn, {
    req(input$input_seq)

    # Remove whitespace characters
    cleaned_input_seq <- gsub("\\s+", "", input$input_seq)
    input_seq <- DNAString(cleaned_input_seq)

    if (input$complement_type == "reverse") {
      reverseComplement(input_seq)
    } else {
      complement(input_seq)
    }
  })

  output$output_seq <- renderText({
    as.character(complement_seq())
  })

  output$download_fasta <- downloadHandler(
    filename = function() {
      paste("complement_sequence_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".fa", sep = "")
    },
    content = function(file) {
      seq_set <- DNAStringSet(complement_seq())
      writeXStringSet(seq_set, filepath = file, format = "fasta")
    }
  )
}
