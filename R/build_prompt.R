build_prompt <- function(
    code, text = "Add documentation to this code."
) {
  prompt <- tidyprompt::tidyprompt(code) |>
    tidyprompt::add_text(paste0(
      "```", "\n\n", text
    ), position = "after", sep = "\n") |>
    answer_using_r_updated(
      add_text = glue::glue(
        "You must respond with only a single updated code snippet.", "\n",
        "Use no other words except for the updated code snippet itself.\n",
        "The updated code snippet should be complete."
      )
    ) |>
    tidyprompt::prompt_wrap(
      modify_fn = function(x) { # Remove unwanted text from `answer_using_r`
        stringr::str_replace(x, "\nYou may not install or load any additional packages.", "")
      }
    )

  return(prompt)
}
