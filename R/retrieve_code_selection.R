# Utility: find the common leading‐whitespace prefix among all lines
find_common_prefix <- function(strings) {
  if (!length(strings)) return("")
  split_strings <- strsplit(strings, "")
  # find the shortest line’s length
  min_len <- min(vapply(split_strings, length, FUN.VALUE = integer(1)))
  prefix_chars <- character()

  for (i in seq_len(min_len)) {
    # Check if the i-th character of each line is the same
    ith_chars <- vapply(split_strings, `[[`, i, FUN.VALUE = character(1))
    # Must all be identical (and must be whitespace) to remain in the prefix
    if (length(unique(ith_chars)) == 1) {
      prefix_chars <- c(prefix_chars, ith_chars[1])
    } else {
      break
    }
  }

  paste(prefix_chars, collapse = "")
}

#' Retrieve the selected code from the source editor
#' capturing original indentation, but removing only the common prefix
#'
#' @return A list with the context, range, code (shifted but preserving relative
#'         indentation), and details to let us re‐add the original indentation
#'
#' @keywords internal
retrieve_code_selection <- function() {
  if (!rstudioapi::isAvailable()) {
    warning("RStudio API is not available.")
    return(NULL)
  }

  context <- rstudioapi::getSourceEditorContext()
  if (is.null(context)) {
    warning("No source editor is active.")
    return(NULL)
  }

  selected_code <- context$selection[[1]]$text
  if (selected_code == "") {
    warning("No code selected in the source editor.")
    return(NULL)
  }

  # Split into lines
  original_lines <- strsplit(selected_code, "\n")[[1]]

  # For each line, get the total leading whitespace
  get_leading_ws <- function(line) sub("^(\\s*).*", "\\1", line)
  leading_spaces <- vapply(original_lines, get_leading_ws, character(1))

  # Find the common prefix among all lines (e.g. "    " if every line starts w/ 4 spaces)
  common_prefix <- find_common_prefix(leading_spaces)
  prefix_len <- nchar(common_prefix)

  # For each line i:
  #   1. Remove that "common_prefix" from leading_spaces[i]
  #   2. Keep the rest of the leading spaces (so deeper indentation is preserved)
  #   3. Then combine leftover leading spaces with the rest of the line
  # That becomes the “shifted” code for the gadget’s editor.
  shift_one_line <- function(full_line, full_leading) {
    # The code portion is everything after the leading whitespace
    code_part <- substring(full_line, nchar(full_leading) + 1)
    # Remove the common prefix from the leading whitespace
    leftover_leading <- substring(full_leading, prefix_len + 1)
    paste0(leftover_leading, code_part)
  }

  shifted_lines <- mapply(
    shift_one_line,
    full_line = original_lines,
    full_leading = leading_spaces,
    USE.NAMES = FALSE
  )

  # This is what goes into the Ace editor
  shifted_code <- paste(shifted_lines, collapse = "\n")

  # Return all needed info for re‐applying
  list(
    context = context,
    range = context$selection[[1]]$range,
    code = shifted_code,
    original_lines = original_lines, # might be useful
    common_prefix = common_prefix
  )
}
