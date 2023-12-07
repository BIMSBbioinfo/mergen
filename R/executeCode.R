#' execute code
#'
#' The function executes a chunk of code either in the current working environment
#' or saves the output as an HTML file to be rendered as a part of a web page
#'
#' @param code code chunk as text, without any decorators or HTML-specific characters.
#' @param output If the output is "eval", the code is executed as is. If the output is "html",
#'               the code is not executed.
#' @param output.file If the output is "html", user can provide a file name for the html.
#'                  If not provided a temporary file will be created.
#' @returns If the output is "eval": if running the code causes errors, errors are returned.
#' Otherwise NA is returned  If output is "html", output file is returned.
#' @export
executeCode <- function(code, output = "eval",
                         output.file = NULL) {

  # Argument validation
  #------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(code),
    assertthat::noNA(code)
  )

  assertthat::assert_that(
    assertthat::is.string(output),
    assertthat::noNA(output)
  )

  if (!is.null(output.file)){
    assertthat::assert_that(
      assertthat::is.string(output.file),
      assertthat::noNA(output.file)
    )}

  #-------------------------------------------------------------------

  # Check if the output option is valid
  if (!output %in% c("eval", "html")) {
    stop("Invalid output option. Choose either 'eval' or 'html'.")
  }


  if (output == "eval") {

    expr<-str2expression(code)

  } else if (output == "html") {

    # Check if the output.file is provided
    if (is.null(output.file)) {
      # Create a temporary output file path
      output.file <- tempfile(fileext = ".html")
      message("Please provide an output.file path for the HTML output. Using temporary file:",
              output.file)
    }

    # Create a temporary R script file to store the parsed code
    temp_file <- tempfile(fileext = ".R")
    print(temp_file)
    writeLines(code, temp_file)

    wd<-getwd()
    expr <- quote({

      message("HTML file created at:", output.file,"\n")
      message("wdir is:", wd,"\n")

      # Produce html fragment
      rmarkdown::render(temp_file, output_file = output.file,
                        knit_root_dir=wd,
                        output_format=rmarkdown::output_format(
                          knitr = rmarkdown::knitr_options(
                            opts_chunk = list(echo = FALSE)),
                          pandoc = rmarkdown::pandoc_options(to = "html",from = NULL),
                          clean_supporting = TRUE,
                          base_format=rmarkdown::html_fragment()
                        )
      )



      #rmarkdown::render(temp_file, output.file = output.file)

      message("HTML file created at:", output.file,"\n")

      # Read the HTML file content and return it
      #html_content <- readChar(output.file, file.info(output.file)$size)
      return(output.file)

    })
  }

  # Create the error holder
  err<-list() # error and warning list

  # Evaluate the expression
  res<-tryCatch(
    withCallingHandlers(
      {

        eval(expr)

      },
      warning = function(w) {
        # Append the warning message to the list
        err[["warning"]] <<- c(err[["warning"]], w$message)

        # Return NULL to continue the program
        NULL
      }
    )
    ,error=function(e){
      #print(e$message)
      err[["error"]] <<- c(err[["error"]], e$message )
      NULL
    }

  )

  if(is.list(err) & ("error" %in% names(err) ) ){
    return(err)
  }else{
    # Return NULL when output is 'eval'
    return(res)
  }

}


