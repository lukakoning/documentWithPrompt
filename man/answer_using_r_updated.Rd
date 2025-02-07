% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/answer_using_r.R
\name{answer_using_r_updated}
\alias{answer_using_r_updated}
\title{Enable LLM to draft and execute R code}
\usage{
answer_using_r_updated(
  prompt,
  add_text = "You must code in the programming language 'R' to answer this prompt.",
  pkgs_to_use = c(),
  objects_to_use = list(),
  list_packages = TRUE,
  list_objects = TRUE,
  skim_dataframes = TRUE,
  evaluate_code = FALSE,
  r_session_options = list(),
  output_as_tool = FALSE,
  return_mode = c("full", "code", "console", "object", "formatted_output", "llm_answer")
)
}
\arguments{
\item{prompt}{A single string or a [tidyprompt()] object}

\item{add_text}{Single string which will be added to the prompt text,
informing the LLM that they must code in R to answer the prompt}

\item{pkgs_to_use}{A character vector of package names that may be used
in the R code that the LLM will generate. If evaluating the R code, these
packages will be pre-loaded in the R session}

\item{objects_to_use}{A named list of objects that may be used in the R code
that the LLM will generate. If evaluating the R code, these objects will be pre-loaded
in the R session. The names of the list will be used as the object names in the
R session}

\item{list_packages}{Logical indicating whether the LLM should be informed
about the packages that may be used in their R code (if TRUE, a list of the
loaded packages will be shown in the initial prompt)}

\item{list_objects}{Logical indicating whether the LLM should be informed
about the existence of 'objects_to_use' (if TRUE, a list of the objects
plus their types will be shown in the initial prompt)}

\item{skim_dataframes}{Logical indicating whether the LLM should be informed
about the structure of dataframes present in 'objects_to_use' (if TRUE,
a skim summary of each `data.frame` type object will be shown in the initial prompt).
This uses the function [skim_with_labels_and_levels()]}

\item{evaluate_code}{Logical indicating whether the R code should be
evaluated. If TRUE, the R code will be evaluated in a separate R session
(using 'callr' to create an isolated R session via \link[callr]{r_session}).
Note that setting this to 'TRUE' means that code generated by the LLM will
run on your system; use this setting with caution}

\item{r_session_options}{A list of options to pass to the \link[callr]{r_session}.
This can be used to customize the R session. See \link[callr]{r_session_options}
for the available options. If no options are provided, the default options
will be used but with 'system_profile' and 'user_profile' set to FALSE}

\item{output_as_tool}{Logical indicating whether the console output of the
evaluated R code should be sent back to the LLM, meaning the LLM will use
R code as a tool to formulate a final answer to the prompt. If TRUE, the LLM
can decide if they can answer the prompt with the output, or if they need to modify
their R code. Once the LLM does not provide new R code (i.e., the prompt is being answered)
this prompt wrap will end (it will continue for as long as the LLM provides R code).
When this option is enabled, the resulting [prompt_wrap()] will be of type 'tool'.
If TRUE, the return mode will also always be set to 'llm_answer'}

\item{return_mode}{Single string indicating the return mode. One of:
\itemize{
 \item 'full': Return a list with the final LLM answer, the extracted R code,
 and (if argument 'evaluate_code' is TRUE) the output of the R code
 \item 'code': Return only the extracted R code
 \item 'console': Return only the console output of the evaluated R code
 \item 'object': Return only the object produced by the evaluated R code
 \item 'formatted_output': Return a formatted string with the extracted R code
 and its console output, and a print of the last object (this is identical to how it would
 be presented to the LLM if 'output_as_tool' is TRUE)
 \item 'llm_answer': Return only the final LLM answer
 }
 When choosing 'console' or 'object', an additional instruction will be added to
 the prompt text to inform the LLM about the expected output of the R code.
 If 'output_as_tool' is TRUE, the return mode will always be set to 'llm_answer'
 (as the LLM will be using the R code as a tool to answer the prompt)}
}
\value{
A [tidyprompt()] object with the [prompt_wrap()] added to it, which
will handle R code generation and possibly evaluation
}
\description{
This function adds a prompt wrap to a [tidyprompt()] that instructs the
LLM to answer the prompt with R code. There are various options to customize
the behavior of this prompt wrap, concerning the evaluation of the R code,
the packages that may be used, the objects that already exist in the R
session, and if the console output that should be sent back to the LLM.
}
\details{
For the evaluation of the R code, the 'callr' package is required.
Please note: automatic evaluation of generated R code may be dangerous to your
system; you must use this function with caution.
}
