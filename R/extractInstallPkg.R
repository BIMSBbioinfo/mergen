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
#'
#' @examples
#' \dontrun{
#' # Check and install "dplyr" package
#' check_install("dplyr")
#' }
#'
#' @export
check_install<-function(package_name){

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
extractInstallPkg<-function(code){

  # Split the code into separate lines
  code_lines <- strsplit(code, "\n")[[1]]

  # for each line look for library call and install things if not installed
  for(a in 1:length(code_lines)){

    if(grepl("library\\(", code_lines[a])){
      new_code <- gsub("library\\((.*)\\)", "check_install('\\1')", code_lines[a])
      message("trying to install ",new_code,"\n")
      eval(str2expression(new_code)) # execute install code

    }
  }
}
