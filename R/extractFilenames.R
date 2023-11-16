#' Extract file names from user prompt
#'
#' This function extracts file names from the user prompt
#' @param text user prompt
#'
#' @examples
#' \dontrun{
#' extractFilenames(text=user_prompt)
#' }
#' @export
extractFilenames <- function(text) {

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(text),
    assertthat::noNA(text)
  )
  # -----------------------------------------------------------------------------

  # Use regular expression to match common file extensions
  matches <- gregexpr("\\b\\S+\\.(txt|tsv|csv|xls|xlsx)\\b", text, ignore.case = TRUE)
  filenames <- regmatches(text, matches)[[1]]

  # Return filenames if found, otherwise return NA
  if (length(filenames) == 0) {
    return(NA)
  } else {
    return(filenames)
  }
}
