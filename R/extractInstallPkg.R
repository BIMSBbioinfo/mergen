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
#' @return  No value returned. Called for installation of package.
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
}

#' extract package names and install them
#'
#' This function extracts all package names that
#' are needed to run the code returned by the agent
#' and installs them as needed.
#' @param code code block returned by the agent.
#' @return  No value returned. Called for installation of package.
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

  # For each line look for library call and install things if not installed
  for(a in 1:length(code_lines)){

    if(grepl("library\\(", code_lines[a])){
      new_code <- gsub("library\\((.*)\\)", "check_install('\\1')", code_lines[a])
      message("trying to install ",new_code,"\n")
      eval(str2expression(new_code)) # Execute install code

    }
  }
}
