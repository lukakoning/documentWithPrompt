% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_edit_prompt.R
\name{editPromptModuleServer}
\alias{editPromptModuleServer}
\title{Edit Prompt Module Server}
\usage{
editPromptModuleServer(
  id,
  predefined_prompts = c(Document = "Add documentation to this code."),
  prompts_file = file.path("~", ".documentWithPrompt_prompts.rds")
)
}
\arguments{
\item{id}{Shiny module ID}

\item{predefined_prompts}{Named vector/list of predefined prompts (name=title, value=text)}

\item{prompts_file}{Path to RDS file for user-defined prompts}
}
\value{
A reactive expression containing the current prompt text
}
\description{
Edit Prompt Module Server
}
