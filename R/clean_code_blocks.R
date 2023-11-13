#' Clean code blocks returned by the agent
#'
#' This function cleans up the code blocks which
#' are returned by the agent to ensure code blocks
#' can run.
#'
#' @param response response recieved by the agent
#'
#' @examples
#' \dontrun{
#' clean_code <- clean_code_blocks(response)
#' }
#' @export
clean_code_blocks<-function(response){

  #clear response of weird characters, otherwise this will return as error
  response<-gsub("```r", "```", response)
  response<-gsub("```R", "```", response)
  response<-gsub("```\\{r\\}", "```", response)
  response<-gsub("```\\{R\\}", "```", response)

  # clean install.packages calls
  response <- gsub("install.packages\\([^)]+\\)+", "", response)

  return(response)
}
