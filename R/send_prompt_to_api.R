send_prompt_to_api <- function(
    prompt, llm_provider = getOption("documentWithPrompt.llm_provider", default = tidyprompt::llm_provider_openai())
) {
  prompt |>
    tidyprompt::send_prompt(llm_provider) |>
    tidyprompt::extract_from_return_list("extracted_code")
}
