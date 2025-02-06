#' Get the LLM provider for 'documentWithPrompt'
#'
#' This function retrieves the LLM provider for the 'documentWithPrompt' add-in.
#' This providers is responsible for handling prompts send to a LLM API.
#'
#' @return An object of class 'LlmProvider' (see ?`llm_provider-class` from package 'tidyprompt')
#'
#' @export
get_llm_provider <- function() {
  options("documentWithPrompt.llm_provider")
}

#' Set the LLM provider for 'documentWithPrompt'
#'
#' This function sets the LLM provider for the 'documentWithPrompt' add-in.
#' This providers is responsible for handling prompts send to a LLM API.
#'
#' @param llm_provider An object of class 'LlmProvider' (see ?`llm_provider-class` from package 'tidyprompt')
#'
#' @return NULL
#'
#' @export
set_llm_provider <- function(llm_provider) {
  if (!inherits(llm_provider, "LlmProvider")) {
    stop(
      "llm_provider must be of class LlmProvider (see ?`llm_provider-class` from package 'tidyprompt')"
    )
  }

  # Assign the provider to the global environment
  options("documentWithPrompt.llm_provider" = llm_provider)

  cli::cli_alert_success("Set the LLM provider for 'documentWithPrompt'")
}
