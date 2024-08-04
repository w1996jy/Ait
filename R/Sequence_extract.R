#' Define the user interface for sequence extraction
#'
#' @description Create a Shiny application UI for sequence extraction.
#'
#' @param id Module ID
#'
#' @return Returns a Shiny UI layout.
#' @name Sequence_extract_ui
#' @noRd
Sequence_extract_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Sequence Extraction"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("id_file"), "Upload ID File (CSV)"),
        fileInput(ns("fasta_file"), "Upload FASTA File"),
        numericInput(ns("nbchar"), "Sequence Length per Line (bp)", value = 60, min = 1),
        actionButton(ns("run_btn"), "Run"),
        downloadButton(ns("download_fasta"), "Download Matched Sequences FASTA File", disabled = TRUE)
      ),
      mainPanel(
        verbatimTextOutput(ns("status"))
      )
    ),
    tags$script(
      'Shiny.addCustomMessageHandler("enableButton", function(message) {
         $("#" + message.id).prop("disabled", false);
      });'
    )
  )
}

#' Define the server logic for sequence extraction
#'
#' @description Implement the server logic for sequence extraction.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value.
#' @name Sequence_extract_server
#' @importFrom seqinr read.fasta
#' @importFrom seqinr write.fasta
#' @noRd
Sequence_extract_server <- function(input, output, session) {
  temp_file <- NULL
  
  observeEvent(input$run_btn, {
    req(input$id_file, input$fasta_file)
    
    # Read ID file
    id_data <- read_csv(input$id_file$datapath)
    ids <- id_data[[1]]
    
    # Read FASTA file
    fasta_data <- read.fasta(input$fasta_file$datapath, forceDNAtolower = FALSE)
    
    # Extract matching sequences
    matched_ids <- ids[ids %in% names(fasta_data)]
    select_list <- lapply(matched_ids, function(id) grep(id, names(fasta_data), ignore.case = TRUE))
    select_transcript <- fasta_data[unlist(select_list)]
    
    # Write to temporary FASTA file
    temp_file <<- tempfile(fileext = ".fa")
    write.fasta(sequences = select_transcript,
                names = names(select_transcript),
                nbchar = input$nbchar,
                file.out = temp_file)
    
    # Update status information
    output$status <- renderText("Sequence extraction completed!")
    
    # Enable download button
    session$sendCustomMessage(type = 'enableButton', message = list(id = 'download_fasta'))
  })
  
  output$download_fasta <- downloadHandler(
    filename = function() {
      paste("extracted_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".fa", sep = "")
    },
    content = function(file) {
      file.copy(temp_file, file)
    },
    contentType = "application/octet-stream"
  )
}
