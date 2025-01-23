send_prompt_to_api <- function(prompt) {
  # Check if there is a OpenAI API key
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    warning(
      "No OpenAI API key found. Please set the OPENAI_API_KEY environment variable.",
      "\nThis is a demo version which will soon get support for configuring other LLM providers, including local."
    )
    return(invisible(NULL))
  }

  llm <- tidyprompt::llm_provider_openai()

  prompt |>
    tidyprompt::send_prompt(llm) |>
    tidyprompt::extract_from_return_list("extracted_code")
}
