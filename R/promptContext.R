#' Predefined prompt contexts for prompt engineering
#'
#' This function holds various predefined prompt
#' contexts that can be used for prompt engineering.
#'
#'@param type specifies the type of context you wish to be returned.
#' Valid options are "simple", "actAs", "CoT" and "rbioinfoExp"
#' @return A string holding the predefined context.
#' @export


promptContext <- function(type="simple"){

  rbionfoExp <-"Act as an expert bioformatician and R user. Answer questions using your expertise. When providing code provide the code in triple backtics and as a single block."
  simple <- "Instruction: Provide R code for the following tasks. Provide the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response. Do no provide code output.\ntask:\n"
  actAs <- "Instruction: Act as an expert bioformatician and R programmer. You also have a lot of knowledge about biology. Complete the following tasks, using your expertise and always provide relevant code. When providing the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response.\ntask:\n"
  CoT <- "Instruction: Act as an expert bioformatician and R programmer. You also have a lot of knowledge about biology. Answer questions using your expertise and always provide code. When providing code, provide the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response. Let's work this out in a step by step way to be sure we have the right answer.\ntask:\n"

  # argument validation
  #-----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(type)
  )

  if (!type %in% c("simple","actAs", "CoT", "rbionfoExp")){
    stop ("Not a valid context. Valid contexts are: simple, actAs, CoT or rbionfoExp")
  }else{
    return (eval(parse(text=type)))
  }

  #-----------------------------------------------------------------------------
}
