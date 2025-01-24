#' Launch a gadget to document code with LLMs
#'
#' @return NULL; this function is called for its side effects (launching the gadget)
#' @export
document_gadget <- function() {
  selection <- retrieve_code_selection()
  if (is.null(selection)) return(invisible(NULL))

  #### UI #####################################################################

  gadget_ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    bslib::card(
      bslib::card_header(
        "",
        shiny::tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",

          # Left-aligned group: unredact and redact
          shiny::tags$div(
            style = "display: flex; gap: 10px;",
            shiny::actionButton("redact", "Redact", class = "btn btn-primary btn-sm") |>
              bslib::tooltip("Replace all string values with redacted values.", placement = "bottom"),
            shiny::actionButton("unredact", "Unredact", class = "btn btn-success btn-sm") |>
              bslib::tooltip("Change redacted values to original values.", placement = "bottom")
          ),

          # Right-aligned group: reset, clear, cancel, and paste
          shiny::tags$div(
            style = "display: flex; gap: 10px;",
            shiny::actionButton("paste", "Paste", class = "btn btn-info btn-sm") |>
              bslib::tooltip("Set clipboard content as content of the editor."),
            shiny::actionButton("clear", "Clear", class = "btn btn-warning btn-sm") |>
              bslib::tooltip("Clear all code from the editor."),
            shiny::actionButton("reset", "Reset", class = "btn btn-secondary btn-sm") |>
              bslib::tooltip("Reset the editor to the original code."),
            shiny::actionButton("cancel", "Cancel", class = "btn btn-danger btn-sm") |>
              bslib::tooltip("Exit without saving changes to the script.")
          )
        )
      ),
      bslib::card_body(
        shinyAce::aceEditor(
          "code_editor",
          "Edit Code:",
          value = selection$code,
          mode = "r",
          # theme = rstudioapi::getThemeInfo()$editor,
          height = "600px",
          fontSize = 14,
          wordWrap = TRUE
        )
      ),
      shiny::tags$div(
        style = "margin-top: 10px; display: flex; gap: 10px;",
        shiny::actionButton("copy_prompt", "Copy prompt") |>
          bslib::tooltip("Copy the prompt to the clipboard.", placement = "bottom"),
        shiny::actionButton("copy_prompt_open_ms", "Copy prompt & open MS Copilot") |>
          bslib::tooltip(
            paste0(
              "Copy the prompt to the clipboard, and open MS Copilot",
              " (you can then paste the prompt there)."
            ),
            placement = "bottom"
          ),
        shiny::actionButton("prepopulate_ms", "Open MS Copilot with prompt") |>
          bslib::tooltip(
            paste0(
              "Open MS Copilot, automatically entering the prompt (passed in URL).",
              " May not always work."
            ),
            placement = "bottom"
          ),
        shiny::actionButton("send_prompt_llm", "Send prompt to API & auto-edit") |>
          bslib::tooltip(
            paste0(
              "Send the prompt to the LLM API; automatically enter the response in the code editor."
            ),
            placement = "bottom"
          )
      ),
      bslib::card_footer(
        shiny::tags$div(
          style = "display: flex; justify-content: flex-end;",
          shiny::actionButton("save_bottom", "Save", class = "btn btn-primary") |>
            bslib::tooltip("Save the edited code to the script and exit.", placement = "bottom")
        )
      )
    )
  )



  #### Server ##################################################################

  gadget_server <- function(input, output, session) {

    # A reactiveVal storing the "string -> placeholder" map
    redacted_strings_map <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$redact, {
      new_code <- redact_strings(input$code_editor, redacted_strings_map)

      if (identical(new_code, input$code_editor)) {
        shiny::showNotification(
          "No string values found to redact",
          duration = 2.5,
          type = "message"
        )
        return(invisible(NULL))
      }

      shinyAce::updateAceEditor(session, "code_editor", value = new_code)
      shiny::showNotification(
        "String values were redacted",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$unredact, {
      new_code <- unredact_strings(input$code_editor, redacted_strings_map)

      if (identical(new_code, input$code_editor)) {
        shiny::showNotification(
          "No redacted string values found",
          duration = 2.5,
          type = "message"
        )
        return(invisible(NULL))
      }

      shinyAce::updateAceEditor(session, "code_editor", value = new_code)
      shiny::showNotification(
        "String values were unredacted",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$reset, {
      # Optionally clear the map when resetting
      redacted_strings_map(character(0))
      shinyAce::updateAceEditor(session, "code_editor", value = selection$code)
      shiny::showNotification(
        "Editor reset to original code",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$clear, {
      # Clear editor (keep or clear map as you like)
      shinyAce::updateAceEditor(session, "code_editor", value = "")
    })

    shiny::observeEvent(input$save_bottom, {
      on_save <- function() {
        new_code_editor <- input$code_editor
        final_code <- reindent_code(new_code_editor, selection$common_prefix)
        original_code <- paste(selection$original_lines, collapse = "\n")

        if (!identical(final_code, original_code)) {
          rstudioapi::modifyRange(
            location = selection$range,
            text = final_code,
            id = selection$context$id
          )
        }
        shiny::stopApp()
      }
      on_save()
    })

    shiny::observeEvent(input$copy_prompt, {
      prompt <- build_prompt(input$code_editor)
      clipr::write_clip(prompt$construct_prompt_text())
      shiny::showNotification(
        "Prompt copied",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$copy_prompt_open_ms, {
      prompt <- build_prompt(input$code_editor)
      prompt_text <- prompt$construct_prompt_text()
      clipr::write_clip(prompt_text)
      shiny::showNotification(
        "Prompt copied; opening MS Copilot in browser",
        duration = 2.5,
        type = "message"
      )
      base_url <- "https://copilot.microsoft.com/"
      utils::browseURL(base_url)
    })

    shiny::observeEvent(input$paste, {
      tryCatch({
        clipboard_content <- clipr::read_clip() # Read clipboard
        clipboard_text <- paste(clipboard_content, collapse = "\n")
        shinyAce::updateAceEditor(session, "code_editor", value = clipboard_text)
        shiny::showNotification(
          "Clipboard content pasted into editor",
          duration = 2.5,
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          "Failed to read clipboard content",
          duration = 2.5,
          type = "error"
        )
      })
    })

    shiny::observeEvent(input$prepopulate_ms, {
      prompt <- build_prompt(input$code_editor)
      prompt_text <- prompt$construct_prompt_text()
      encoded_prompt_text <- utils::URLencode(prompt_text, reserved = TRUE)

      base_url <- "https://copilot.microsoft.com/"
      full_url <- paste0(base_url, "?q=", encoded_prompt_text)

      shiny::showNotification(
        "Opening MS Copilot in browser (with prompt)",
        duration = 2.5,
        type = "message"
      )
      utils::browseURL(full_url)
    })

    shiny::observeEvent(input$send_prompt_llm, {
      prompt <- build_prompt(input$code_editor)
      shiny::showNotification(
        "Sending prompt to LLM API; please wait...",
        duration = 2.5,
        type = "message"
      )

      new_code <- tryCatch({
        send_prompt_to_api(prompt)
      }, error = function(e) {
        shiny::showNotification(
          "Error sending prompt to LLM API; see console",
          duration = 2.5,
          type = "error"
        )
        NULL
      })

      if (is.null(new_code)) return(invisible(NULL))

      # If you want to keep lines flush, you can reindent with "".
      reindented <- reindent_code(new_code, "")
      shinyAce::updateAceEditor(session, "code_editor", value = reindented)

      shiny::showNotification(
        "Editor updated with response from LLM API",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }



  #### Run gadget ##############################################################

  tryCatch({
    suppressMessages(
      shiny::runGadget(
        gadget_ui,
        gadget_server,
        viewer = shiny::dialogViewer("documentWithPrompt", width = 1200, height = 1000),
        stopOnCancel = TRUE
      )
    )
  }, error = function(e) {
    message("The gadget was closed without saving or canceling.")
  })
}
