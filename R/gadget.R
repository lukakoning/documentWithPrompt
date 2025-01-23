document_gadget <- function() {
  selection <- retrieve_code_selection()
  if (is.null(selection)) return(invisible(NULL))

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    bslib::card(
      bslib::card_header(
        "",
        shiny::tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::checkboxInput("toggle_redact", "Redact strings?", value = FALSE) |>
            bslib::tooltip(
              "When checked, all strings in your code get redacted in the prompt."
            ),
          shiny::tags$div(
            style = "display: flex; gap: 10px;",
            shiny::actionButton("reset", "Reset", class = "btn btn-secondary") |>
              bslib::tooltip("Reset the code to the original code."),
            shiny::actionButton("clear", "Clear", class = "btn btn-warning") |>
              bslib::tooltip("Clear the code editor."),
            shiny::actionButton("cancel", "Cancel", class = "btn btn-danger btn-sm") |>
              bslib::tooltip("Close without saving changes.")
          )
        )
      ),
      bslib::card_body(
        shinyAce::aceEditor(
          "code_editor",
          "Edit Code:",
          value = selection$code,
          mode = "r",
          theme = rstudioapi::getThemeInfo()$theme,
          height = "300px",
          fontSize = 14,
          wordWrap = TRUE
        ),
        shiny::tags$div(
          style = "margin-top: 10px; display: flex; gap: 10px;",
          shiny::actionButton("copy_prompt", "Copy prompt"),
          shiny::actionButton("copy_prompt_open_ms", "Copy prompt & open MS Copilot"),
          shiny::actionButton("send_prompt_llm", "Send prompt to LLM API & auto-edit")
        )
      ),
      bslib::card_footer(
        shiny::tags$div(
          style = "display: flex; justify-content: flex-end;",
          shiny::actionButton("save_bottom", "Save", class = "btn btn-primary") |>
            bslib::tooltip("Save changes back to the script.")
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Re-add the common prefix to each line upon saving
    reindent_code <- function(edited_code, common_prefix) {
      new_lines <- strsplit(edited_code, "\n")[[1]]
      # For each line:
      #  1. we figure out how much whitespace is actually leading now
      #  2. we re-add the previously removed `common_prefix` at the front
      reindented <- vapply(
        new_lines,
        function(line) {
          paste0(common_prefix, line)
        },
        character(1)
      )
      paste(reindented, collapse = "\n")
    }

    # Function to do the actual saving
    on_save <- function() {
      new_code_editor <- input$code_editor
      final_code      <- reindent_code(new_code_editor, selection$common_prefix)

      # Only update if the new code differs from old code
      original_code <- paste(selection$original_lines, collapse = "\n")
      if (!identical(final_code, original_code)) {
        rstudioapi::modifyRange(
          location = selection$range,
          text     = final_code,
          id       = selection$context$id
        )
      }
      shiny::stopApp()
    }

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
    shiny::observeEvent(input$save_bottom, {
      on_save()
    })
    # If you still have input$done somewhere, you can use it
    shiny::observeEvent(input$done, {
      on_save()
    })

    # Reset back to the originally shifted code
    shiny::observeEvent(input$reset, {
      shinyAce::updateAceEditor(session, "code_editor", value = selection$code)
    })

    # Clear the code editor
    shiny::observeEvent(input$clear, {
      shinyAce::updateAceEditor(session, "code_editor", value = "")
    })

    # copy prompt
    shiny::observeEvent(input$copy_prompt, {
      prompt <- build_tidyprompt(input$code_editor, redact = input$toggle_redact)
      clipr::write_clip(prompt$construct_prompt_text())
    })
    shiny::observeEvent(input$copy_prompt_open_ms, {
      prompt <- build_tidyprompt(input$code_editor, redact = input$toggle_redact)
      clipr::write_clip(prompt$construct_prompt_text())
      # open MS Copilot ...
    })
    shiny::observeEvent(input$send_prompt_llm, {
      prompt   <- build_tidyprompt(input$code_editor, redact = input$toggle_redact)
      new_code <- send_prompt_to_api(prompt)
      if (is.null(new_code)) return(invisible(NULL))

      # "Shift" the new code so it's consistent in the editor
      # We do the reverse: strip off the same `common_prefix` we had,
      # so the lines remain aligned in the editor
      # but preserve the deeper indentation among lines.
      # For simplicity, if you want to keep them flush, you can just replace
      # with new_code directly. Or do the same “common prefix” approach.
      #
      # For example, if you want to keep things flush:
      reindented <- reindent_code(new_code, "")
      shinyAce::updateAceEditor(session, "code_editor", value = reindented)
    })
  }

  tryCatch(
    {
      shiny::runGadget(
        ui,
        server,
        viewer = shiny::dialogViewer("documentWithPrompt", width = 1200, height = 1600),
        stopOnCancel = TRUE
      )
    },
    error = function(e) {
      message("The gadget was closed without saving or canceling.")
    }
  )
}
