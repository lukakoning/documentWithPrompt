# Re-add the common prefix to each line upon saving
reindent_code <- function(edited_code, common_prefix) {
  new_lines <- strsplit(edited_code, "\n")[[1]]
  reindented <- vapply(
    new_lines,
    function(line) {
      paste0(common_prefix, line)
    },
    character(1)
  )
  paste(reindented, collapse = "\n")
}

# Named char vector: names = "original quoted string", value = "placeholder"
# Helper: Check if a string is already a redacted placeholder
is_placeholder <- function(x) {
  grepl('^["\']__redacted\\d+__["\']$', x)
}

# Redact strings in the code
redact_strings <- function(code, redacted_strings) {
  old_map <- redacted_strings()

  # Regex to capture any single- or double-quoted strings:
  string_pattern <- '"[^"]*"|\'[^\']*\''
  matches <- regmatches(code, gregexpr(string_pattern, code))
  all_found <- unique(unlist(matches))

  if (length(all_found) == 0) {
    # No quoted strings at all; nothing to do
    return(code)
  }

  next_id <- length(old_map) + 1

  # Separate placeholders vs. genuine strings
  placeholders     <- all_found[vapply(all_found, is_placeholder, logical(1))]
  genuine_strings  <- setdiff(all_found, placeholders)

  # For each genuine string found:
  #   - If it’s already in old_map, keep the same placeholder
  #   - Else create a new placeholder
  new_map <- old_map
  for (str in genuine_strings) {
    if (!str %in% names(new_map)) {
      new_map[str] <- paste0("\"__redacted", next_id, "__\"")
      next_id <- next_id + 1
    }
  }

  # Replace all genuine_strings in the code with placeholders from new_map
  redacted_code <- code
  for (str in genuine_strings) {
    placeholder <- new_map[[str]]
    redacted_code <- gsub(str, placeholder, redacted_code, fixed = TRUE)
  }

  # Update the reactive map
  redacted_strings(new_map)

  redacted_code
}

# Unredact strings in the code
unredact_strings <- function(code, redacted_strings) {
  current_map <- redacted_strings()
  if (length(current_map) == 0) {
    return(code)
  }
  # For each original -> placeholder, revert code
  unredacted_code <- code
  for (original_str in names(current_map)) {
    placeholder_str <- current_map[[original_str]]
    unredacted_code <- gsub(placeholder_str, original_str, unredacted_code, fixed = TRUE)
  }
  unredacted_code
}
