#' Edit Prompt Module UI
#'
#' @param id Shiny module ID
#' @param label The label for the button that opens the modal
#' @return A tagList containing shiny UI elements
editPromptModuleUI <- function(id, label = "Configure prompt") {
  ns <- shiny::NS(id)
  # Include shinyjs so we can enable/disable the name field
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::actionButton(
      inputId = ns("open_modal"),
      label = label,
      class = "btn btn-primary btn-sm"
    )
  )
}

#' Edit Prompt Module Server
#'
#' @param id Shiny module ID
#' @param predefined_prompts Named vector/list of predefined prompts (name=title, value=text)
#' @param prompts_file Path to RDS file for user-defined prompts
#'
#' @return A reactive expression containing the current prompt text
editPromptModuleServer <- function(
    id,
    predefined_prompts = c(
      "Document" = "Add documentation to this code."
    ),
    prompts_file = file.path("~", ".documentWithPrompt_prompts.rds")
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #-------------------------
    # Utility: load/save
    #-------------------------
    load_user_prompts <- function() {
      if (file.exists(prompts_file)) {
        out <- readRDS(prompts_file)
        # Expecting structure:
        #   list(
        #     .metadata = list(default_prompt_name = "someNameOrNULL"),
        #     "My Prompt1" = "prompt text",
        #     "My Prompt2" = "prompt text"
        #   )
        if (is.null(out) || !is.list(out)) out <- list()
        if (is.null(out$.metadata) || !is.list(out$.metadata)) {
          out$.metadata <- list(default_prompt_name = NULL)
        }
        if (is.null(out$.metadata$default_prompt_name)) {
          out$.metadata$default_prompt_name <- NULL
        }
        out
      } else {
        list(.metadata = list(default_prompt_name = NULL))
      }
    }

    save_user_prompts <- function(x) {
      saveRDS(x, prompts_file)
    }

    #-------------------------
    # Initialize
    #-------------------------
    ups_raw <- load_user_prompts()
    meta_info <- ups_raw[[".metadata"]]
    if (is.null(meta_info$default_prompt_name)) {
      meta_info$default_prompt_name <- NULL
    }

    # Actual user-defined prompts (excluding metadata)
    user_defined_prompts <- ups_raw
    user_defined_prompts[[".metadata"]] <- NULL

    # Determine which name is the default
    default_name <- meta_info$default_prompt_name

    # Try to find default_name’s text if set
    if (!is.null(default_name)) {
      if (default_name %in% names(predefined_prompts)) {
        default_prompt_text <- predefined_prompts[[default_name]]
      } else if (default_name %in% names(user_defined_prompts)) {
        default_prompt_text <- user_defined_prompts[[default_name]]
      } else {
        # fallback to first predefined or blank
        default_name <- names(predefined_prompts)[1] %||% "New Prompt"
        default_prompt_text <- predefined_prompts[[default_name]] %||% ""
      }
    } else {
      # fallback to first predefined or blank
      default_name <- names(predefined_prompts)[1] %||% "New Prompt"
      default_prompt_text <- predefined_prompts[[default_name]] %||% ""
    }

    #-------------------------
    # Reactive "database"
    #-------------------------
    user_prompts <- shiny::reactiveVal(user_defined_prompts)
    metadata     <- shiny::reactiveVal(meta_info)

    # The currently active prompt text
    current_prompt <- shiny::reactiveVal(default_prompt_text)

    # We'll store:
    #  - current_name: the actual name used in user_prompts or in the predefined list
    #  - typed_name:   what is displayed in the textInput (may be different if user is editing)
    current_name <- shiny::reactiveVal(default_name)
    typed_name   <- shiny::reactiveVal(default_name)

    # Are we currently editing the name textInput? If TRUE -> button says "Save Name"
    editing_name <- shiny::reactiveVal(FALSE)

    # Helper: re-save the entire structure
    save_all <- function() {
      out_list <- c(user_prompts(), list(.metadata = metadata()))
      save_user_prompts(out_list)
    }

    #=========================
    # Show main modal
    #=========================
    shiny::observeEvent(input$open_modal, {
      # Build the dropdown choices: "displayName" => "promptText"
      # We'll also add "New prompt..." => "" for convenience
      choice_vec <- c("New prompt..." = "")

      if (length(predefined_prompts)) {
        choice_vec <- c(
          choice_vec,
          stats::setNames(unname(predefined_prompts), names(predefined_prompts))
        )
      }
      if (length(user_prompts())) {
        ups_now <- user_prompts()
        choice_vec <- c(
          choice_vec,
          stats::setNames(unname(ups_now), names(ups_now))
        )
      }

      # Which text is currently in use?
      current_choice_text <- current_prompt()
      preselect_value <- if (current_choice_text %in% choice_vec) {
        current_choice_text
      } else {
        ""
      }

      # Show modal
      shiny::showModal(
        shiny::modalDialog(
          shiny::tagList(
            tags$head(
              tags$style('.card{overflow: visible !important;}'),
              tags$style('.card-body{overflow: visible !important;}')
            ),
            bslib::card(
              bslib::card_body(
                # Row with "New Prompt" button + select dropdown
                shiny::fluidRow(
                  style = "display: flex; flex-wrap: nowrap; gap: 10px;",
                  bslib::card(
                    style = "flex: 1; min-width: 300px;",
                    bslib::card_header("Select Prompt"),
                    bslib::card_body(
                      shiny::selectInput(
                        ns("prompt_selector"),
                        label = NULL,
                        choices = choice_vec,
                        selected = preselect_value
                      )
                    )
                  ),
                  bslib::card(
                    style = "flex: 1; min-width: 300px;",
                    bslib::card_header("New prompt"),
                    bslib::card_body(
                      shiny::actionButton(ns("new_prompt_btn"), "Create", class = "btn btn-secondary")
                    )
                  )
                ),

                # Prompt editor card
                bslib::card(
                  bslib::card_header("Current prompt"),
                  bslib::card_body(
                    # A fluidRow for the prompt name + "Edit" button
                    shiny::fluidRow(
                      shiny::column(
                        width = 9,
                        shiny::textInput(
                          ns("prompt_name"),
                          label = "Name:",
                          value = current_name(),
                          width = "100%"
                        )
                      ),
                      shiny::column(
                        width = 3,
                        # Wrap the action button in a <div> and push it down
                        tags$div(
                          style = "margin-top: 25px;",
                          shiny::actionButton(
                            ns("toggle_name_btn"),
                            label = "Edit",
                            icon = shiny::icon("pencil"),
                            class = "btn btn-secondary"
                          )
                        )
                      )
                    ),

                    # The main text area
                    shiny::textAreaInput(
                      ns("prompt_text"),
                      label = "Text:",
                      value = current_prompt(),
                      rows = 5, width = "100%"
                    )
                  ),
                  bslib::card_footer(
                    style = "display:flex; justify-content: space-between;",
                    shiny::actionButton(ns("use_temp"), "Use", class = "btn btn-primary") |>
                      bslib::tooltip("Use this prompt.", placement = "bottom"),
                    shiny::actionButton(ns("save_perm"), "Save", class = "btn btn-success") |>
                      bslib::tooltip("Save this prompt for re-use. (Not required for use in only this session.)", placement = "bottom"),
                    shiny::actionButton(ns("delete_prompt"), "Delete", class = "btn btn-danger") |>
                      bslib::tooltip("Delete a saved prompt.", placement = "bottom"),
                    shiny::actionButton(ns("set_default"), "Set as default", class = "btn btn-info") |>
                      bslib::tooltip("Set this prompt as the default for future sessions.", placement = "bottom")
                  )
                )
              )
            )
          ),
          size = "l", easyClose = TRUE
        )
      )

      # Initialize selections
      if (preselect_value == "") {
        # means "New prompt"
        current_name("New Prompt")
        typed_name("New Prompt")
        current_prompt("")
      } else {
        # match user-defined or predefined
        ud <- user_prompts()
        matched_user <- names(ud)[ud == preselect_value]
        if (length(matched_user)) {
          current_name(matched_user[1])
          typed_name(matched_user[1])
          current_prompt(preselect_value)
        } else {
          # maybe predefined
          pd_names <- names(predefined_prompts)[predefined_prompts == preselect_value]
          if (length(pd_names)) {
            current_name(pd_names[1])
            typed_name(pd_names[1])
            current_prompt(preselect_value)
          } else {
            # fallback
            current_name("New Prompt")
            typed_name("New Prompt")
            current_prompt("")
          }
        }
      }

      # Update inputs
      shiny::updateTextInput(session, "prompt_name", value = typed_name())
      shinyjs::disable("prompt_name")
      shiny::updateTextAreaInput(session, "prompt_text", value = current_prompt())

      # By default, name is disabled until user clicks "Edit Name"
      editing_name(FALSE)
    })

    #=========================
    # Observers to handle enabling/disabling name field
    #=========================
    shiny::observe({
      # If editing_name() is TRUE => "Save Name" and enable
      if (editing_name()) {
        shiny::updateActionButton(session, "toggle_name_btn", label = "Save", icon = shiny::icon("save"))
        shinyjs::enable("prompt_name")
      } else {
        shiny::updateActionButton(session, "toggle_name_btn", label = "Edit", icon = shiny::icon("pencil"))
        shinyjs::disable("prompt_name")
      }
    })

    # Whenever user types in the name field, we store it in typed_name
    shiny::observeEvent(input$prompt_name, {
      typed_name(input$prompt_name)
    })

    #=========================
    # "Toggle Name" button
    #=========================
    shiny::observeEvent(input$toggle_name_btn, {
      if (!editing_name()) {
        # We are going from "Edit Name" -> "Save Name"
        # If it's a predefined prompt, disallow rename
        if (current_name() %in% names(predefined_prompts)) {
          shiny::showNotification("Cannot rename a predefined prompt.", type = "error")
          return()
        }
        editing_name(TRUE)
      } else {
        # We are going from "Save Name" -> finishing rename
        new_name <- typed_name()
        old_name <- current_name()

        # If user tries to keep "New Prompt" as final name, not allowed
        if (new_name == "New Prompt") {
          shiny::showNotification("Please choose a different name (cannot use 'New Prompt').", type = "error")
          return()
        }

        ud <- user_prompts()
        is_currently_in_user <- old_name %in% names(ud)

        # If new_name is already used by a different user-defined prompt
        if (new_name %in% names(ud) && new_name != old_name) {
          shiny::showNotification(
            paste0("Name '", new_name, "' already exists. Please choose a unique name."),
            type = "error"
          )
          return()
        }

        # Cannot rename to a name that is in predefined
        if (new_name %in% names(predefined_prompts) && !(old_name %in% names(predefined_prompts))) {
          shiny::showNotification(
            paste0("Name '", new_name, "' conflicts with a predefined prompt."),
            type = "error"
          )
          return()
        }

        # If old_name was previously saved, rename that entry
        if (is_currently_in_user && (old_name != new_name)) {
          all_meta <- metadata()
          # If old_name was the default, update default name as well
          if (identical(all_meta$default_prompt_name, old_name)) {
            all_meta$default_prompt_name <- new_name
            metadata(all_meta)
          }
          # Move the prompt text over
          ud[[new_name]] <- ud[[old_name]]
          ud[[old_name]] <- NULL
          user_prompts(ud)
          save_all()
        }

        # Update current_name
        current_name(new_name)

        # --- NEW CODE: update the dropdown so the new name is selected ---
        # (Only if it's really in user_prompts, i.e. either it was a rename or
        #  the old name was already in user_prompts.)
        # If we want to always refresh, we can do so unconditionally:
        choice_vec <- c("New prompt..." = "")
        if (length(predefined_prompts)) {
          choice_vec <- c(choice_vec, stats::setNames(unname(predefined_prompts), names(predefined_prompts)))
        }
        if (length(user_prompts())) {
          ups_now <- user_prompts()
          choice_vec <- c(choice_vec, stats::setNames(unname(ups_now), names(ups_now)))
        }
        # selected = user_prompts()[[new_name]] if it exists, else current prompt
        sel_text <- ud[[new_name]] %||% current_prompt()
        shiny::updateSelectInput(session, "prompt_selector", choices = choice_vec, selected = sel_text)

        editing_name(FALSE)
        shiny::showNotification(paste("Name updated to:", new_name), type = "message")
      }
    })

    #=========================
    # If dropdown changes
    #=========================
    shiny::observeEvent(input$prompt_selector, {
      sel_text <- input$prompt_selector
      ud <- user_prompts()

      if (sel_text == "") {
        current_name("New Prompt")
        typed_name("New Prompt")
        current_prompt("")
      } else {
        # Check user-defined
        match_user <- names(ud)[ud == sel_text]
        if (length(match_user)) {
          current_name(match_user[1])
          typed_name(match_user[1])
          current_prompt(sel_text)
        } else {
          # maybe predefined
          match_pd <- names(predefined_prompts)[predefined_prompts == sel_text]
          if (length(match_pd)) {
            current_name(match_pd[1])
            typed_name(match_pd[1])
            current_prompt(sel_text)
          } else {
            current_name("New Prompt")
            typed_name("New Prompt")
            current_prompt("")
          }
        }
      }

      # Update the UI fields
      shiny::updateTextInput(session, "prompt_name", value = typed_name())
      shiny::updateTextAreaInput(session, "prompt_text", value = current_prompt())

      editing_name(FALSE)
    })

    #=========================
    # "New Prompt" button
    #=========================
    shiny::observeEvent(input$new_prompt_btn, {
      shiny::updateSelectInput(session, "prompt_selector", selected = "")
      current_name("New Prompt")
      typed_name("New Prompt")
      current_prompt("")
      shiny::updateTextInput(session, "prompt_name", value = "New Prompt")
      shiny::updateTextAreaInput(session, "prompt_text", value = "")
      editing_name(FALSE)
    })

    #=========================
    # Use (just sets current_prompt reactive, then closes modal)
    #=========================
    shiny::observeEvent(input$use_temp, {
      if (current_prompt() == input$prompt_text) {
        shiny::showNotification("No changes made to current prompt.", duration = 2.5, type = "message")
      } else {
        current_prompt(input$prompt_text)
        shiny::showNotification("Current prompt updated.", duration = 2.5, type = "message")
      }
      shiny::removeModal()
    })

    # JavaScript event to detect modal dismissal
    shinyjs::extendShinyjs(
      text = "
        shinyjs.bindModalClose = function(id) {
          $('#' + id).on('hidden.bs.modal', function() {
            Shiny.setInputValue(id + '_modal_closed', Math.random());
          });
        };
      ",
      functions = c("bindModalClose")
    )


    # Bind modal close event on modal creation
    shiny::observeEvent(input$open_modal, {
      shinyjs::runjs(sprintf("shinyjs.bindModalClose('%s');", ns("open_modal_modal")))
    })

    # Detect modal dismissal
    shiny::observeEvent(input$open_modal_modal_closed, {
      if (current_prompt() == input$prompt_text) {
        shiny::showNotification("No changes made to current prompt.", type = "message")
      }
    })



    #=========================
    # Save (prompt text)
    #=========================
    shiny::observeEvent(input$save_perm, {
      nm <- current_name()
      txt <- input$prompt_text

      # Disallow saving under the name "New Prompt"
      if (nm == "New Prompt") {
        shiny::showNotification("Cannot save with the name 'New Prompt'. Please choose a different name.", type = "error")
        return()
      }

      # If prompt text is empty, warn the user
      if (!nzchar(txt)) {
        shiny::showNotification("Prompt text is empty. Cannot save an empty prompt.", type = "warning")
        return()
      }

      # If name is predefined, do not allow changing text
      if (nm %in% names(predefined_prompts)) {
        # If text differs from the official predefined text, revert
        if (!identical(txt, predefined_prompts[[nm]])) {
          shiny::showNotification("Cannot modify predefined prompt text.", type = "error")
          shiny::updateTextAreaInput(session, "prompt_text", value = predefined_prompts[[nm]])
          return()
        }
        shiny::showNotification("No changes to predefined prompt.", type = "message")
        return()
      }

      # Overwrite or create new user-defined prompt
      ud <- user_prompts()
      ud[[nm]] <- txt
      user_prompts(ud)
      save_all()

      # Update the dropdown options to reflect the new/updated prompt
      choice_vec <- c("New prompt..." = "")
      if (length(predefined_prompts)) {
        choice_vec <- c(choice_vec, stats::setNames(unname(predefined_prompts), names(predefined_prompts)))
      }
      if (length(ud)) {
        choice_vec <- c(choice_vec, stats::setNames(unname(ud), names(ud)))
      }
      shiny::updateSelectInput(session, "prompt_selector", choices = choice_vec, selected = txt)

      current_prompt(txt)
      shiny::showNotification(paste("Prompt text saved for:", nm), type = "message")
    })

    #=========================
    # Delete (only user-defined)
    #=========================
    shiny::observeEvent(input$delete_prompt, {
      txt <- input$prompt_text
      if (!nzchar(txt)) {
        shiny::showNotification("No valid prompt selected to delete.", type = "warning")
        return()
      }

      ud <- user_prompts()
      # Find which name in user-defined has this text
      name_to_del <- names(ud)[ud == txt]
      if (!length(name_to_del)) {
        shiny::showNotification("Cannot delete: not a user-defined prompt.", type = "warning")
        return()
      }
      name_to_del <- name_to_del[1]

      # Initialize default_text for later reference
      all_meta <- metadata()
      default_name <- all_meta$default_prompt_name

      # Remove the prompt
      ud[[name_to_del]] <- NULL
      user_prompts(ud)
      save_all()

      shiny::showNotification(sprintf("User-defined prompt '%s' deleted.", name_to_del), type = "message")

      # Update dropdown choices
      choice_vec <- c("New prompt..." = "")
      if (length(predefined_prompts)) {
        choice_vec <- c(choice_vec, stats::setNames(unname(predefined_prompts), names(predefined_prompts)))
      }
      if (length(ud)) {
        choice_vec <- c(choice_vec, stats::setNames(unname(ud), names(ud)))
      }

      # Check if a default prompt exists
      if (!is.null(default_name)) {
        # Display the default prompt
        default_text <- if (default_name %in% names(predefined_prompts)) {
          predefined_prompts[[default_name]]
        } else if (default_name %in% names(ud)) {
          ud[[default_name]]
        } else {
          ""
        }

        shiny::updateSelectInput(session, "prompt_selector", choices = choice_vec, selected = default_text)
        current_prompt(default_text)
        current_name(default_name)
        typed_name(default_name)
        shiny::updateTextAreaInput(session, "prompt_text", value = default_text)
        shiny::updateTextInput(session, "prompt_name", value = default_name)
      } else {
        # Fallback to "New Prompt"
        shiny::updateSelectInput(session, "prompt_selector", choices = choice_vec, selected = "")
        current_prompt("")
        current_name("New Prompt")
        typed_name("New Prompt")
        shiny::updateTextAreaInput(session, "prompt_text", value = "")
        shiny::updateTextInput(session, "prompt_name", value = "New Prompt")
      }

      editing_name(FALSE)
    })



    #=========================
    # Set Default
    #=========================
    shiny::observeEvent(input$set_default, {
      txt <- input$prompt_text
      if (!nzchar(txt)) {
        shiny::showNotification("No prompt to set as default.", type = "warning")
        return()
      }

      # find name from text
      ud <- user_prompts()
      matched_user <- names(ud)[ud == txt]
      matched_pre  <- names(predefined_prompts)[predefined_prompts == txt]
      if (!length(matched_user) && !length(matched_pre)) {
        shiny::showNotification("Prompt must be saved or be predefined before setting default.", type = "error")
        return()
      }
      the_name <- if (length(matched_user)) matched_user[1] else matched_pre[1]

      # If already default
      all_meta <- metadata()
      if (identical(all_meta$default_prompt_name, the_name)) {
        shiny::showNotification("This prompt is already the default.", type = "warning")
        return()
      }

      all_meta$default_prompt_name <- the_name
      metadata(all_meta)
      save_all()
      current_prompt(txt)

      shiny::showNotification(sprintf("'%s' is now the default prompt.", the_name), type = "message")
    })

    #=========================
    # Return the current prompt text for external use
    #=========================
    return(current_prompt)
  })
}
