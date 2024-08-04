#' Define the user interface for reverse complement functionality
#'
#' @description Create a Shiny UI for getting complement sequences (forward and reverse).
#'
#' @param id Module ID
#'
#' @return Returns a Shiny UI layout.
#' @name reverse_complement_ui
#' @noRd
#' 
reverse_complement_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Get Complement Sequence"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput(ns("input_seq"), "Enter DNA sequence", rows = 10, placeholder = "Enter DNA sequence...", 
                      value = "ATTTATCCTTCAGCCCCTCTAGGCTTAGCAGTCAGCTTGACTTGAGGAGGAAAAAAAAACAACACACCACACTGCATTAATAATGGGGCGATCACCATGCTGTGAGAAGATTGGACTGAAGAAAGGTCCATGGACGCCGGAGGAGGATGAGAAGCTGCTTGCCTTCGTTGAGGAACATGGACACGGGAGCTGGCGGGCATTACCTGCGAAGGCAGGCTTGCAGAGGTGTGGGAAGAGCTGCAGGTTGAGGTGGACAAACTACCTGAGGCCGGACATCAAGAGGGGCAAGTTCAGCTTGCAAGAAGAACAGACCATCATCCAGCTTCACGCTCTTTTAGGCAACAGGTGGTCGGCCATTGCAACACATCTACCAAATCGCACGGACAACGAGATCAAGAACCACTGGAACACGCACCTCAAGAAAAGGCTGGCCAAGATTGGGATCGATCCTGTCACCCACAAATCTACCTGTGGCACTCTCACTGGCACCACGAACGACAGATCAGCCAAGGCCGCGGCAAGCCTCAGCCACATGGCACAATGGGAGAATGCCCGCCTCGAGGCTGAGGCACGGCTGGCTCGAGAATCGAAGACACGAACAGCAACACCGACGCCATCTGCACTCCATGCGCAGCCAATGGATCTACCTGCCTCTGCTGCTTCTCCATGGCTTGACGTGTTGCATGCTTGGCAGGGTGCAAAGATAGACCTGGAGTCACCTACCTCCACACTGACGTTTACAGGGAGCAATGGTGGCATGCTGCCAACCCCCAGGACCAACGGACCAGAGGTATCAGAAAGCAACTCCGCGATGTCGCATTATCAGATGAGCGATGAGTTGGAGGGTGAAGAAACCTATTGGCAGATCTTCAGCAAGCACCAAGTGCCGGAAGTGGACAGCAAGGAGAGTGAAGATGACTTCATTGGCTGTGAGGAGCCGTGGTTCTCAGGGATGGCTGGGGTTGGAGCTGGCATGCTGCTTGATGTATCCAATGAGCATGAGCTATCAGAATGCTGGGGTGAGTCCAGCAGTGGCCAAACTGTTGAGCACAGCAAGCAAGCATCCGATAAGGAGGACAAGGATTATTGGAATTGGGTCCTTGACAGAGTAAACTCAGAGCTGACAGCACAGTCGCCTTCCTTGGTCTAAAATACAGCCCCCCCCCCCCCCCCCCCCCCCCCTGCAGTTTTTTTTTCAACGACTCCTTAGCAAAATCTATTGATCATCTTAAGTCAAAAGAGAAACAGAACATACGAGAAGTACAAATAAATTCTACAGAAGGATGTTTTTTGTCCATGCATGTGAGTGAAATAATTAGCTGTTGTGTGTATGTAGTAAATATTTTTTTCCCTGATTTCTGAATGTATGTTAAGCCTCCAGGACAACAGCAGGGACACTACATTAATGGTCCAAGGACTTTACAGTTGTTAGTTCAACGGCCAAGTTGTCTTGTCTCCCTCTCTTTTTGAATCCATCCCAGAATGAGTAAGAA"),
        radioButtons(ns("complement_type"), "Choose complement type:",
                     choices = list("Forward Complement" = "forward", "Reverse Complement" = "reverse"),
                     selected = "reverse"),
        actionButton(ns("run_btn"), "Get Complement Sequence"),
        downloadButton(ns("download_fasta"), "Download")
      ),
      mainPanel(
        verbatimTextOutput(ns("output_seq"))
      )
    )
  )
}
#' Define the server logic for reverse complement functionality
#'
#' @description Implement the server logic to get complement sequences.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @import Biostrings
#' @return No return value.
#' @name reverse_complement_server
#' @noRd
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
