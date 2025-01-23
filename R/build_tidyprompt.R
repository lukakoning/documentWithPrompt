build_tidyprompt <- function(code, redact = FALSE) {
  if (redact) code <- redact_strings(code)

  prompt <- tidyprompt::tidyprompt(code) |>
    tidyprompt::add_text(glue::glue(
      "You are given a code snippet:", "\n\n",
      "```r"
    ), position = "before", sep = "\n") |>
    tidyprompt::add_text(glue::glue(
      "```", "\n\n",
      "Please add documentation to this code snippet.\nYour documentation should explain what the code does,",
      " how it works, and why it is important.", "\n",
      "Your documentation should be clear, concise, and easy to understand.", "\n",
      "You may not alter the code itself or change how it functions."
    ), position = "after", sep = "\n") |>
    answer_using_r_updated(
      add_text = glue::glue(
        "You must respond with only a single documented code snippet.", "\n",
        "Use no other words except for the documented code snippet itself.\n",
        "The documented code snippet should contain all of the original code."
      )
    ) |>
    tidyprompt::prompt_wrap(
      modify_fn = function(x) {
        stringr::str_replace(x, "\nYou may not install or load any additional packages.", "")
      }
    )

  return(prompt)
}

# Function to redact strings from the code
redact_strings <- function(code) {
  string_pattern <- '"[^"]*"|\'[^\']*\'' # Match double or single quoted strings
  gsub(string_pattern, '"..."', code)
}
