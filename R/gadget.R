#' Launch a gadget to document code with LLMs
#'
#' @return NULL; this function is called for its side effects (launching the gadget)
#' @export
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

          # Left-aligned group: unredact and redact
          shiny::tags$div(
            style = "display: flex; gap: 10px;",
            shiny::actionButton("redact", "Redact", class = "btn btn-primary btn-sm") |>
              bslib::tooltip("Replace all strings with redacted values.", placement = "bottom"),
            shiny::actionButton("unredact", "Unredact", class = "btn btn-success btn-sm") |>
              bslib::tooltip("Restore all redacted strings to their original values.", placement = "bottom")
          ),

          # Right-aligned group: reset, clear, and cancel
          shiny::tags$div(
            style = "display: flex; gap: 10px;",
            shiny::actionButton("reset", "Reset", class = "btn btn-secondary btn-sm") |>
              bslib::tooltip("Reset the editor to the original code."),
            shiny::actionButton("clear", "Clear", class = "btn btn-warning btn-sm") |>
              bslib::tooltip("Clear all code from the editor."),
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
              " (without a prepopulated prompt; you can then paste it there)."
            ),
            placement = "bottom"
          ),
        shiny::actionButton("prepopulate_ms", "Open MS Copilot with prompt") |>
          bslib::tooltip(
            paste0(
              "Open MS Copilot, automatically entering the prompt (passed in URL).",
              " May not always work; only seems to work for shorter prompts (there is a length limit)."
            ),
            placement = "bottom"
          ),
        shiny::actionButton("send_prompt_llm", "Send prompt to API & auto-edit") |>
          bslib::tooltip(
            paste0(
              "Send the prompt to the LLM API, and automatically enter the response in the code editor."
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

    # Named char vector: names = "original quoted string", value = "placeholder"
    redacted_strings <- shiny::reactiveVal(character(0))

    # Helper: Check if a string is already a redacted placeholder
    is_placeholder <- function(x) {
      grepl('^["\']__redacted\\d+__["\']$', x)
    }

    # 1) Redact function
    redact_strings <- function(code) {
      old_map <- redacted_strings()

      # Regex to capture any single- or double-quoted strings:
      string_pattern <- '"[^"]*"|\'[^\']*\''
      matches <- regmatches(code, gregexpr(string_pattern, code))
      all_found <- unique(unlist(matches))

      if (length(all_found) == 0) {
        # No quoted strings at all; nothing to do
        return(code)
      }

      # Next ID to use starts after however many placeholders we already have
      next_id <- length(old_map) + 1

      # 1a) Separate placeholders vs. genuine strings
      placeholders <- all_found[ vapply(all_found, is_placeholder, logical(1)) ]
      genuine_strings <- setdiff(all_found, placeholders)

      # 1b) For each genuine string found:
      #     - If it’s already in old_map, keep the same placeholder
      #     - Else create a new placeholder
      new_map <- old_map
      for (str in genuine_strings) {
        if (!str %in% names(new_map)) {
          # Assign a new placeholder with the next ID
          new_map[str] <- paste0("\"__redacted", next_id, "__\"")
          next_id <- next_id + 1
        }
      }

      # 2) Replace all genuine_strings in the code with placeholders from new_map
      redacted_code <- code
      for (str in genuine_strings) {
        placeholder <- new_map[[str]]
        redacted_code <- gsub(str, placeholder, redacted_code, fixed = TRUE)
      }

      # Update the reactive map
      redacted_strings(new_map)

      redacted_code
    }

    # 2) Unredact function
    unredact_strings <- function(code) {
      current_map <- redacted_strings()
      if (length(current_map) == 0) {
        return(code)
      }
      # For each original -> placeholder, revert code
      unredacted_code <- code
      for (original_str in names(current_map)) {
        placeholder_str <- current_map[[original_str]]
        unredacted_code <- gsub(placeholder_str, original_str, unredacted_code, fixed = TRUE)
      }
      unredacted_code
    }

    # --- Observe button clicks ---
    shiny::observeEvent(input$redact, {
      new_code <- redact_strings(input$code_editor)
      shinyAce::updateAceEditor(session, "code_editor", value = new_code)
    })

    shiny::observeEvent(input$unredact, {
      new_code <- unredact_strings(input$code_editor)
      shinyAce::updateAceEditor(session, "code_editor", value = new_code)
    })

    shiny::observeEvent(input$reset, {
      # Optionally clear the map when resetting
      redacted_strings(character(0))
      shinyAce::updateAceEditor(session, "code_editor", value = selection$code)
    })

    shiny::observeEvent(input$clear, {
      # Clear editor. Keep or clear map as you like
      # If you want to allow re-redacting when you refill code, you might keep the map.
      # Or you can reset it if you'd like a blank slate:
      # redacted_strings(character(0))
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
      prompt <- build_tidyprompt(input$code_editor)
      clipr::write_clip(prompt$construct_prompt_text())
      shiny::showNotification(
        "Prompt copied to clipboard!",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$copy_prompt_open_ms, {
      prompt <- build_tidyprompt(input$code_editor)
      prompt_text <- prompt$construct_prompt_text()
      clipr::write_clip(prompt_text)
      shiny::showNotification(
        "Prompt copied to clipboard & opening MS Copilot in browser!",
        duration = 2.5,
        type = "message"
      )
      base_url <- "https://copilot.microsoft.com/"
      utils::browseURL(base_url)
    })

    shiny::observeEvent(input$prepopulate_ms, {
      prompt <- build_tidyprompt(input$code_editor)
      prompt_text <- prompt$construct_prompt_text()
      encoded_prompt_text <- utils::URLencode(prompt_text, reserved = TRUE)

      # Construct the URL with the encoded prompt
      base_url <- "https://copilot.microsoft.com/"
      full_url <- paste0(base_url, "?q=", encoded_prompt_text)

      shiny::showNotification(
        "Opening prepopulated MS Copilot in browser!",
        duration = 2.5,
        type = "message"
      )

      # Open the URL in the default web browser
      utils::browseURL(full_url)
    })

    shiny::observeEvent(input$send_prompt_llm, {
      prompt   <- build_tidyprompt(input$code_editor)

      shiny::showNotification(
        "Sending to LLM API! Please wait...",
        duration = 2.5,
        type = "message"
      )

      new_code <- tryCatch({
        send_prompt_to_api(prompt)
      }, error = function(e) {
        shiny::showNotification(
          "Error sending prompt to API.",
          duration = 2.5,
          type = "error"
        )
        NULL
      })

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

      shiny::showNotification(
        "Editor updated with response from LLM API!",
        duration = 2.5,
        type = "message"
      )
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  tryCatch(
    {
      suppressMessages(
        shiny::runGadget(
          ui,
          server,
          viewer = shiny::dialogViewer("documentWithPrompt", width = 1200, height = 1600),
          stopOnCancel = TRUE
        )
      )
    },
    error = function(e) {
      message("The gadget was closed without saving or canceling.")
    }
  )
}
