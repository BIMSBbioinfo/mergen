#' Extract file names from user prompt
#'
#' This function extracts file names from the user prompt. Current filenames that
#' are supported by this function are *.txt, *.tsv, *.csv *.xls, *.xlsx,
#' *.bed, *.bigWig, *.bw and *.bigBed. Other filenames will not be extracted.
#' If no filenames are found, the function will return NA.
#' @param text user prompt
#' @returns A list holding file names from the user prompt.
#' @examples
#' {
#' user_prompt <- "How do I perform PCA on data in my file called test.txt?"
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
  matches <- gregexpr("\\b\\S+\\.(txt|tsv|csv|xls|xlsx|bed|bigWig|bw|bigBed)\\b", text, ignore.case = TRUE)
  filenames <- regmatches(text, matches)[[1]]

  # Return filenames if found, otherwise return NA
  if (length(filenames) == 0) {
    return(NA)
  } else {
    return(filenames)
  }
}
