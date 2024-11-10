options(shiny.maxRequestSize = 200 * 1024 * 1024)  # 最大上传文件大小为10MB
#' sequence_extract UI Module
#' @description sequence_extract UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title sequence_extract_ui
#' @name sequence_extract_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
sequence_extract_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Sequence Extract',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("id_file"), "Upload ID File (CSV)"),
          fileInput(ns("fasta_file"), "Upload FASTA File")
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
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                numericInput(ns("nbchar"), "Sequence Length per Line (bp)", value = 60, min = 1)
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                downloadButton(ns("download_fasta"),"Download", disabled = TRUE)
              )
              ),
            mainPanel(
              verbatimTextOutput(ns("status"))
            ),
            tags$script(
              'Shiny.addCustomMessageHandler("enableButton", function(message) {
         $("#" + message.id).prop("disabled", false);
      });'
            )
            )
          )
        )
      )
    )
}
#' sequence_extract_server Module
#' @description Server logic for sequence_extract
#' @param input Standard shiny server arguments
#' @param output Standard shiny server arguments
#' @param session Standard shiny server arguments
#' @import shiny
#' @import seqinr
#' @importFrom readr read_csv
#' @title sequence_extract_server
#' @name sequence_extract_server
#' @export
#'
sequence_extract_server <- function(input, output, session) {
  ns <- session$ns
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
