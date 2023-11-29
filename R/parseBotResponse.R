#' extract the code and text from the text returned by LLM agent
#'
#' The function parses the code and can return the text and code as single blocks
#' This good for execution and might be useful for displaying purposes later on.
#'
#' @param text A character string containing the text with embedded code blocks.
#' @param delimiter A character string representing the delimiter used to enclose the code blocks (default: "```").
#'
#' @return A list with two elements: 'code' and 'text'. 'code' contains the concatenated code blocks, and
#'         'text' contains the remaining text with code blocks removed.
#' @examples
#' text <- "\n\nThe following, normalize the table and do PCA.
#' \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```"
#' result <- extractCode(text)
#' print(result$code)
#' print(result$text)
#'
#'
#' @export
extractCode<-function(text,delimiter="```"){

  # Argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(text),
    assertthat::noNA(text)
  )

  assertthat::assert_that(
    assertthat::is.string(delimiter),
    assertthat::noNA(delimiter)
  )
  # -----------------------------------------------------------------------------

  # Split the text by the delimiter
  parts <- strsplit(text, delimiter, fixed = TRUE)[[1]]

  # Get matches to delimiter
  mloc<-gregexpr(delimiter,text,fixed=T)[[1]]
  mdf<-cbind(mloc[-length(mloc)],mloc[-1]) # Create a df with matches

  if(nrow(mdf)>1){
    mdf <- mdf[-seq(2, nrow(mdf), 2),, drop=FALSE ] # Remove incorrect blocks
  }

  # Extract code blocks
  code_blocks<-mapply(function(x,y) substr(text,x,y), mdf[,1]+3,mdf[,2]-1)

  # Concatenate the code blocks and remaining text
  concatenated_code <- paste(code_blocks, collapse = "\n")
  text_reg<-paste0(delimiter,".*?",delimiter)
  concatenated_remaining_text <-gsub(text_reg, "\n",text)

  return(list(code = concatenated_code,
              text = concatenated_remaining_text))
}


