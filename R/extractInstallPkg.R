#' Check and Install R Package
#'
#' This function checks if an R package is installed, and if not, attempts to install
#' it using either the standard CRAN repository or the Bioconductor repository.
#'
#' @param package_name A character string specifying the name of the package to be checked and installed.
#'
#' @importFrom BiocManager install
#' @importFrom BiocManager valid
#' @importFrom utils install.packages
#' @return  TRUE if the package is already installed or can be installed. FALSE otherwise
#' @examples
#' \donttest{
#' # Check and install "dplyr" package
#' check_install("dplyr")
#' }
#'
#' @export
check_install<-function(package_name){

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(package_name),
    assertthat::noNA(package_name)
  )
  # -----------------------------------------------------------------------------

  if (!require(package_name, character.only = TRUE)) {
     message("\ntrying to install: ", package_name,"\n")
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }

    if (requireNamespace("BiocManager", quietly = TRUE)) {
      tryCatch(
        BiocManager::install(package_name, dependencies = TRUE,ask=FALSE,update=FALSE),
        error = function(e) {
          install.packages(package_name, dependencies = TRUE,ask=FALSE)
        }
      )
    }
  }

  # returns true if the package is installed
  require(package_name, character.only = TRUE)

}

#' extract package names and install them
#'
#' This function extracts all package names that
#' are needed to run the code returned by the agent
#' and installs them as needed.
#' @param code code block returned by the agent.
#' @return  final status of the packages as a logical vector, TRUE if packages is already installed
#' or could be installed. FALSE if the package can't be install.
#' @examples
#' \donttest{
#' # Check code for packages that need installing
#' code <- "library(devtools)\n x<-5"
#' extractInstallPkg(code)
#' }
#'
#' @export
extractInstallPkg<-function(code){

  # Argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(code),
    assertthat::noNA(code)
  )
  # -----------------------------------------------------------------------------

  # Split the code into separate lines
  code_lines <- strsplit(code, "\n")[[1]]


  # Define a regex pattern to match 'library' or 'require' calls.
  # It allows for optional spaces (\\s*) between the function name and the opening parenthesis,
  # and captures the argument inside the parentheses.
  pattern <- "(library|require)\\s*\\(([^)]+)\\)"

  # Apply the regex pattern to each line.
  # 'gregexpr' returns a list of match positions for each line.
  matches <- regmatches(code_lines, gregexpr(pattern, code_lines))

  # Process each line's matches.
  package_names <- lapply(matches, function(match) {
    # For each match, extract the package name.
      sapply(match, function(m) {
        # Extract the content within the parentheses.
        # We use a regex with lookaheads and lookbehinds to capture this content.
        pkg_name <- regmatches(m, regexpr("(?<=\\()[^)]+(?=\\))", m, perl = TRUE))
        # trim white space
        trimws(pkg_name)
      })
  })

  # remove extra quotation marks and make the vecor
  pkg_names <- unique(gsub('\"', '', unlist(package_names) ) )


  ## ! old code refactored above
  # For each line look for library call and install things if not installed
  #for(a in 1:length(code_lines)){

  #  if(grepl("(library|require)\\(", code_lines[a])){
  #    new_code <- gsub("(library|require)\\((.*)\\)", "check_install('\\2')", code_lines[a])
  #    message("trying to install ",new_code,"\n")
  #    eval(str2expression(new_code)) # Execute install code
  #  }
  #}

  # check and if doesn't exist, install packages
  sapply(pkg_names,check_install)
}
