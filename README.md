
<!-- README.md is generated from README.Rmd. Please edit that file -->

# documentWithPrompt

<!-- badges: start -->
<!-- badges: end -->

‘documentWithPrompt’ provides an RStudio add-in which assists you in
documenting your code using large language models.

The process is as follows:

1)  In your R script within RStudio, select the code you want to
    document.

2)  Run the add-in. You can do this by clicking on the “Add-ins”
    dropdown menu in RStudio, and selecting “documentWithPrompt”. You
    can set a keyboard shortcut for this via Tools \> Add-ins \> Browse
    Add-ins \> Keyboard Shortcuts (see
    <https://rstudio.github.io/rstudio-extensions/rstudio_addins.html#keyboard-shorcuts>).
    Alternatively, you can use function `documentWithPrompt::gadget()`
    in your console.

3)  The add-in will show a code editor with the selected code. Here you
    have the option to copy a prompt to your clipboard, which you can
    then paste into a large language model. This prompt will contain the
    selected code, and instructions on how to document it. You can also
    copy the code and open MS Copilot, or open MS Copilot with the
    prompt prepulated (prompt is passed in the URL; may only work for
    shorter prompts). You can then use the LLM reply and paste it back
    into the editor. Alternatively, you can send the prompt to an LLM
    provider API after which the add-in will automatically insert the
    returned documentation into the editor.

4)  If you are happy with the edited code, you can click ‘Save’ to save
    the changes to your script. If not, you can just press ‘Cancel’ and
    exit without saving.

Users also have the option to redact strings in their code. This can be
useful if you are concerned about them containing sensitive information.
You can redact the code, send it to an LLM, then when the code is
documented you can unredact it (placing back the original string
values).

This is a quick demo add-in, may contain bugs, and has limited
configurability. However, if you want to choose your LLM provider you
can use `set_llm_provider()`:

``` r
# Set to Ollama (local LLM provider)
set_llm_provider(tidy_prompt::llm_provider_ollama())
# Add setting this option to your .Rprofile to make it permanent for every new R session
# See 'tidyprompt' package for more information on LLM providers
```

## Installation

You can install the development version of ‘documentWithPrompt’ from
[GitHub](https://github.com/lukakoning/documentWithPrompt) with:

``` r
# install.packages("remotes")
remotes::install_github("lukakoning/documentWithPrompt")
```
