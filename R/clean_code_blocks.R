#' Clean code blocks returned by the agent
#'
#' This function cleans up the response
#' returned by the agent to ensure code blocks
#' can run. It ensures that characters such as
#' 'R' and 'r' are cleaned from code blocks in the agents
#' response, so that the code blocks are able to be extracted by
#' the extractCode() function and ran as expected. It also cleans
#' the response from any install.package calls, and recorded output,
#' so that when code blocks are extracted, the code can run smoothly.
#' @param response response received from the agent
#' @return A string holding the response of the agent, cleaned from any unwanted characters.
#' @examples
#' {
#' response <- "To perform PCA, do the following: ```R prcomp(data)``` This funcion will perform PCA."
#' clean_code <- clean_code_blocks(response)
#' }
#' @export
clean_code_blocks<-function(response){

  # argument validation
  #-----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(response),
    assertthat::noNA(response)
  )

  #-----------------------------------------------------------------------------

  #clear response of weird characters, otherwise this will return as error
  response<-gsub("```r", "```", response)
  response<-gsub("```R", "```", response)
  response<-gsub("```bash", "```", response)
  response<-gsub("```\\{r\\}", "```", response)
  response<-gsub("```\\{R\\}", "```", response)

  # clean of possible output text
  response<-gsub("\n\\[[0-9]+\\].*\n","",response)

  # clean install.packages calls
  response <- gsub("install.packages\\([^)]+\\)+", "", response)

  return(response)
}
