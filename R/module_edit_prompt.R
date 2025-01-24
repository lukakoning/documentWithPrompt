#' Edit Prompt Module UI
#'
#' @param id Shiny module ID
#' @param label The label for the button that opens the modal
#' @return An actionButton (when clicked, it will show the prompt editor modal).
editPromptModuleUI <- function(id, label = "Configure prompt") {
  ns <- shiny::NS(id)
  shiny::actionButton(
    inputId = ns("open_modal"),
    label = label,
    class = "btn btn-primary btn-sm"
  )
}

#' Edit Prompt Module Server
#'
#' This module handles:
#'   - Loading/saving user-defined prompts from an RDS file
#'   - Combining them with predefined prompts
#'   - Providing a modal to pick or create a prompt
#'   - Returning a reactive that holds the currently chosen prompt
#'
#' @param id Shiny module ID
#' @param predefined_prompts Named vector or list of predefined prompts
#' @param prompts_file Path to RDS file for user-defined prompts
#'
#' @return A reactive expression containing the current prompt text.
editPromptModuleServer <- function(
    id,
    predefined_prompts = c(
      "Document" = "Add documentation to this code."
    ),
    prompts_file = file.path("~", ".documentWithPrompt_prompts.rds")
) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    #####################
    # Load/save helpers
    #####################
    load_user_prompts <- function() {
      if (file.exists(prompts_file)) {
        out <- readRDS(prompts_file)
        if (is.null(out) || !is.list(out)) return(list())
        out
      } else {
        list()
      }
    }
    save_user_prompts <- function(x) {
      saveRDS(x, prompts_file)
    }

    #####################
    # Initialize storage
    #####################
    ups <- load_user_prompts()

    initial_prompt <- predefined_prompts[1]
    # If a default prompt is stored, override the provided initial_prompt
    if (!is.null(ups[["__DEFAULT_PROMPT__"]])) {
      initial_prompt <- ups[["__DEFAULT_PROMPT__"]]
    }

    # ReactiveVal storing the user-defined prompts
    user_prompts <- shiny::reactiveVal(ups)

    # ReactiveVal for the currently active prompt text
    current_prompt <- shiny::reactiveVal(initial_prompt)

    # Temporary storage for the text we plan to "Save as Named Prompt"
    temp_prompt_to_save <- shiny::reactiveVal(NULL)

    #####################
    # Show main modal
    #####################
    shiny::observeEvent(input$open_modal, {
      ups <- user_prompts()

      # Exclude the special default from user-defined prompts so it doesn't duplicate
      ups_for_select <- ups
      ups_for_select[["__DEFAULT_PROMPT__"]] <- NULL

      # Build choices: placeholders, predefined, user-defined
      prompt_choices <- c(
        "..." = "",
        predefined_prompts,
        if (length(ups_for_select)) unlist(ups_for_select)
      )

      # Remove any old matching logic
      shiny::showModal(
        shiny::modalDialog(
          shiny::tagList(
            tags$head(
              tags$style('.card{overflow: visible !important;}'),
              tags$style('.card-body{overflow: visible !important;}')
            ),
            bslib::card(
              bslib::card_body(
                shiny::div(
                  shiny::fluidRow(
                    style = "display: flex; flex-wrap: nowrap; gap: 10px;", # Flexbox layout with nowrap to force single-row layout
                    shiny::tagList(
                      bslib::card(
                        style = "flex: 1; min-width: 300px;  overflow: visible !important;", # Ensure the card takes equal width and minimum space
                        bslib::card_header("Select prompt"),
                        bslib::card_body(
                          shiny::selectizeInput(
                            ns("prompt_selector"),
                            "",
                            choices = prompt_choices,
                            selected = if (current_prompt() %in% prompt_choices) {
                              current_prompt()
                            } else {
                              ""
                            }
                          )
                        )
                      ),
                      bslib::card(
                        style = "flex: 1; min-width: 300px;", # Similar styling to ensure alignment
                        bslib::card_header("New prompt"),
                        shiny::p(""),
                        bslib::card_body(
                          style = "display: flex; gap: 10px;",
                          shiny::actionButton(ns("new_prompt_btn"), "Create", class = "btn btn-secondary")
                        )
                      )
                    )
                  )
                ),
                bslib::card(
                  bslib::card_header("Edit prompt"),
                  bslib::card_body(
                    shiny::textAreaInput(
                      ns("prompt_text"),
                      "",
                      value = current_prompt(),
                      rows = 5,
                      width = "100%"
                    )
                  ),
                  bslib::card_footer(
                    style = "display:flex; justify-content: space-between;",
                    shiny::actionButton(ns("use_temp"), "Use", class = "btn btn-primary"),
                    shiny::actionButton(ns("save_perm"), "Use & save", class = "btn btn-success"),
                    shiny::actionButton(ns("delete_prompt"), "Delete", class = "btn btn-danger"),
                    shiny::actionButton(ns("set_default"), "Set as default", class = "btn btn-info")
                  )
                )
              )
            )
          ),
          size = "l"
        )
      )
    })

    #####################
    # Prompt selection
    #####################
    shiny::observeEvent(input$prompt_selector, {
      sel <- input$prompt_selector
      if (is.na(sel) || sel == "") return()  # do not overwrite text area
      shiny::updateTextAreaInput(session, "prompt_text", value = sel)
    })

    # Switch to "New prompt (custom)" instantly
    shiny::observeEvent(input$new_prompt_btn, {
      shiny::updateSelectInput(session, "prompt_selector", selected = "")
      shiny::updateTextAreaInput(session, "prompt_text", value = "")
    })

    # Use once (not saved permanently)
    shiny::observeEvent(input$use_temp, {
      current_prompt(input$prompt_text)
      shiny::removeModal()
      shiny::showNotification("Current prompt updated", duration = 2.5, type = "message")
    })

    #####################
    # Save as named prompt
    #####################
    # Save as named prompt
    shiny::observeEvent(input$save_perm, {
      if (!nzchar(input$prompt_text)) {
        shiny::showNotification("Cannot save: prompt text is empty", duration = 3, type = "warning")
        return()
      }

      # Check if the prompt text already exists in predefined or user-defined prompts
      existing_prompts <- c(predefined_prompts, unlist(user_prompts()))
      if (input$prompt_text %in% existing_prompts) {
        shiny::showNotification("Cannot save: identical prompt is already saved", duration = 3, type = "error")
        return()
      }

      # Store the current text so we can save it after user enters a name
      temp_prompt_to_save(shiny::isolate(input$prompt_text))

      # Close the first modal
      shiny::removeModal()

      # Show second modal for user to input the prompt name
      shiny::showModal(shiny::modalDialog(
        title = "Enter a short name for the prompt",
        shiny::textInput(ns("prompt_name_modal"), ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),  # closes this second modal
          shiny::actionButton(ns("confirm_save"), "Confirm", class = "btn btn-success")
        ),
        easyClose = FALSE
      ))
    })

    # Save permanently: After saving, reopen the main modal
    shiny::observeEvent(input$confirm_save, {
      nm <- input$prompt_name_modal
      if (!nzchar(nm)) {
        shiny::showNotification("Please enter a name", duration = 3, type = "warning")
        return()
      }

      # Check if the name already exists in user-defined or predefined prompts
      ups <- user_prompts()
      if (nm %in% names(ups) || nm %in% names(predefined_prompts)) {
        shiny::showNotification("This name already exists. Please choose a unique name", duration = 3, type = "error")
        return()
      }

      # Save the named prompt
      ups[[nm]] <- temp_prompt_to_save()
      user_prompts(ups)
      save_user_prompts(ups)

      current_prompt(temp_prompt_to_save())

      shiny::removeModal() # Close the name input modal
      shiny::showNotification(paste0(
        "Prompt saved under name '", nm, "' and set as current"
      ), duration = 2.5, type = "message")
    })



    #####################
    # Delete user-defined prompt
    #####################
    shiny::observeEvent(input$delete_prompt, {
      sel <- input$prompt_selector
      ups <- user_prompts()

      # Find if `sel` is one of the user-defined prompts
      to_delete <- NULL
      for (n in names(ups)) {
        if (identical(ups[[n]], sel)) {
          to_delete <- n
          break
        }
      }

      # Determine the current default
      current_default <- if (!is.null(ups[["__DEFAULT_PROMPT__"]])) {
        ups[["__DEFAULT_PROMPT__"]]
      } else if (length(predefined_prompts) > 0) {
        predefined_prompts[1]  # Implicit default
      } else {
        NULL
      }

      # Prevent deletion of the default prompt
      if (!is.null(current_default) && identical(current_default, sel)) {
        shiny::showNotification("Cannot delete: this is the default prompt.", duration = 3, type = "error")
        return()
      }

      if (is.null(to_delete)) {
        shiny::showNotification("Cannot delete: not a user-defined prompt.", duration = 3, type = "warning")
      } else {
        # Delete the user-defined prompt
        ups[[to_delete]] <- NULL
        user_prompts(ups)
        save_user_prompts(ups)
        shiny::showNotification(sprintf("User-defined prompt deleted: %s", to_delete), duration = 3, type = "message")

        # Determine fallback prompt
        fallback_prompt <- if (!is.null(ups[["__DEFAULT_PROMPT__"]])) {
          ups[["__DEFAULT_PROMPT__"]]
        } else if (length(predefined_prompts) > 0) {
          predefined_prompts[1]
        } else {
          ""  # Empty fallback if no default or predefined prompts exist
        }

        # Set the fallback prompt as current
        current_prompt(fallback_prompt)
        shiny::updateSelectInput(session, "prompt_selector", selected = fallback_prompt)
        shiny::updateTextAreaInput(session, "prompt_text", value = fallback_prompt)
      }
    })



    #####################
    # Set current prompt as default
    #####################
    shiny::observeEvent(input$set_default, {
      txt <- input$prompt_text
      ups <- user_prompts()

      # Check the current explicit default or implicit default
      current_default <- if (!is.null(ups[["__DEFAULT_PROMPT__"]])) {
        ups[["__DEFAULT_PROMPT__"]]
      } else if (length(predefined_prompts) > 0) {
        predefined_prompts[1]  # Implicit default
      } else {
        NULL
      }

      # Verify that the prompt exists in predefined or user-defined prompts
      existing_prompts <- c(predefined_prompts, unlist(ups))
      if (!txt %in% existing_prompts) {
        shiny::showNotification(
          "Cannot set default: prompt text must be saved first.",
          duration = 3, type = "error"
        )
        return()
      }

      # If the implicit default is being set explicitly, block the action
      if (identical(txt, predefined_prompts[1]) && is.null(ups[["__DEFAULT_PROMPT__"]])) {
        shiny::showNotification(
          "This prompt is already the implicit default and cannot be set explicitly.",
          duration = 3, type = "info"
        )
        return()
      }

      # If the current prompt is already explicitly set as default, block the action
      if (!is.null(current_default) && identical(current_default, txt)) {
        shiny::showNotification(
          "This prompt is already set as the default.",
          duration = 2.5, type = "warning"
        )
        return()
      }

      # Store the selected prompt as the new explicit default
      ups[["__DEFAULT_PROMPT__"]] <- txt
      user_prompts(ups)
      save_user_prompts(ups)

      current_prompt(txt)
      shiny::showNotification("Prompt set as default for future sessions.", duration = 2.5, type = "message")
    })


    # Return the current prompt as a reactive
    return(current_prompt)
  })
}
