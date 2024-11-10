#' Flexible Tools User Interface
#'
#' Creates a user interface for flexible data export tools in a Shiny application.
#' This panel allows users to select data export formats and specify output filenames.
#'
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @name flexible_tools_ui
#' @title flexible_tools_ui
#' @export
flexible_tools_ui <- function(id) {
  ns <- NS(id)
  absolutePanel(
    id = "export_footer",
    class = "card-no-gap",
    draggable = TRUE,
    width = "340px",
    height = "110px",
    top = "auto",
    right = 0,
    bottom = 0,
    card(
      card_header(class = "bg-dark", "Flexible tools"),
      card_body(
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Data Export",
            icon = bs_icon("caret-down-fill"),
            radioButtons(
              inputId = ns("export_format"),
              label = "Data Format",
              choices = c("Figure", "Table", "Both"),
              selected = "Both"
            ),
            textInput(
              inputId = ns("export_prefix"),
              label = "Output Filename",
              value = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")  # 格式化时间戳
            ),
            actionButton(inputId = ns("export_button"), label = "Export", icon = icon("file-export"))
          )
        )
      )
    )
  )
}
