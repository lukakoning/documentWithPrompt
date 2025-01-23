#' Retrieve the selected code from the source editor
#'
#' @return A list with the context, range, and code of the selected code
#'
#' @keywords internal
retrieve_code_selection <- function() {
  # Ensure RStudio API is available
  if (!rstudioapi::isAvailable()) {
    warning("RStudio API is not available.")
    return(NULL)
  }

  # Get the context of the source editor
  context <- rstudioapi::getSourceEditorContext()

  # Check if the context exists
  if (is.null(context)) {
    warning("No source editor is active.")
    return(NULL)
  }

  # Retrieve the selected code
  selected_code <- context$selection[[1]]$text

  # If nothing is selected, inform the user
  if (selected_code == "") {
    warning("No code selected in the source editor.")
    return(NULL)
  }

  # Return the selected code
  return(list(
    context = context,
    range = context$selection[[1]]$range,
    code = selected_code
  ))
}

#' Launch a gadget to document selected code
#'
#' @return NULL; the gadget is launched in the RStudio viewer pane. The user can
#'  edit the selected code and save or cancel the changes.
#' @export
document_gadget <- function() {
  # Retrieve the selected code and its position
  selection <- retrieve_code_selection()

  if (is.null(selection)) return(invisible(NULL))

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    bslib::card(
      bslib::card_header("Edit Selected Code"),
      bslib::card_body(
        shiny::textAreaInput(
          "code_editor", "Edit your code:",
          value = selection$code, rows = 10, width = "100%"
        )
      ),
      bslib::card_footer(
        shiny::actionButton("done", "Save"),
        shiny::actionButton("cancel", "Cancel")
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      new_code <- input$code_editor
      if (new_code != selection$code) {
        rstudioapi::modifyRange(
          location = selection$range,
          text = new_code,
          id = selection$context$id
        )
      }
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Edit Code", width = 1200, height = 1600))
}
