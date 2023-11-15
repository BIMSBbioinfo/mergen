#' Self correct the code returned by the agent
#'
#' The function attempts to correct the code returned by the agent
#' by re-feeding to the agent with the error message. If there are no
#' error messages function returns the original response.
#'
#' @param agent An object containing the agent's information (e.g., type and model).
#' @param prompt The prompt text to send to the language model.
#' @param context Optional context to provide alongside the prompt (default is rbionfoExp).
#' @param attempts numeric value denoting how many times the code should be sent back for fixing.
#' @param output.file Optional output file created holding parsed code
#' @param ... Additional arguments to be passed to the \code{\link{sendPrompt}} function.
#' @return A list containing the following elements:
#' \item{init.response}{A character vector representing the initial prompt response.}
#' \item{init.blocks}{A list of initial blocks.}
#' \item{final.blocks}{A list of final blocks.}
#' \item{code.works}{A boolean value indicating whether the code works.}
#' \item{exec.result}{A character string representing the execution results.}
#' \item{tried.attempts}{An integer representing the number of attempts.}
#'
#' @examples
#' \dontrun{
#'
#' response <- selfcorrect(agent,prompt,context=rbionfoExp, max_tokens = 500)
#' }
#' @export
selfcorrect<-function(agent,prompt,context=rbionfoExp,attempts=3,output.file=NULL,...){

  #---------------------------------------------------------------------------
  # Validate arguments

  assertthat::assert_that(
    assertthat::`%has_name%`(agent,c("name","model","type","openai_api_key")),
    assertthat::noNA(agent)
  )

  assertthat::assert_that(
    assertthat::is.string(prompt),
    assertthat::noNA(prompt)
  )

  assertthat::assert_that(
    assertthat::is.string(context),
  )

  assertthat::assert_that(
    assertthat::is.number(attempts),
    assertthat::noNA(attempts)
  )

  if (!is.null(output.file)) {
    assertthat::assert_that(
      assertthat::is.string(output.file),
      assertthat::noNA(output.file)
    )
  }
  #------------------------------------------------------------------------------------------
  # send prompt
  response <- sendPrompt(agent,prompt,context,return.type="text",...)

  # clean the code backtick structure and install.packages calls
  response<-clean_code_blocks(response)

  initial.response <- response


  # parse the code
  blocks <- extractCode(text=initial.response,delimiter="```")
  initial.blocks <- blocks

  # extract and install packages if needed
  extractInstallPkg(blocks$code)


  # list of messages to the bot
  msgs<- list(
    list(
      "role" = "user",
      "content" = paste(context,"\n",prompt)
    ),
    list(
      "role"="assistant",
      "content"=initial.response
    )
  )


  # check if any code is returned
  if(blocks$code==""){
    stop("no code returned")

  }

  # Define the prompt template to inject the error message
  promptTemplate <- "The previous code returned the following errors and/or warnings,\n <error> \n return fixed code in one block, delimited in triple backticks"

  # set up the on of the final variables that will be returned in the end
  codeWorks=FALSE

  # execute the code up to "attempts" times
  for(i in 1:attempts){

    # see if the code runs without errors
    res<-executeCode(blocks$code, output = "html",output.file = output.file )

    # if there are errors
    if(is.list(res) & ("error" %in% names(res) )){

      # get error messages

      # Collapse the character vectors within the list elements
      # this is good if we have multiple errors in the list per element
      collapsed_list <- lapply(res, function(x) paste(x, collapse = "\n"))

      # get error/warning text
      errs<-  paste(paste0(names(collapsed_list ), ": ", collapsed_list ), collapse = "\n ")

      # Use sub() to substitute the replacement string for the wildcard string
      promptAddon <- sub("<error>", errs, promptTemplate)

      #get an updated prompt
      #new.prompt<-paste(response,promptAddon)
      new.prompt<-promptAddon


      # send prompt
      #response <- sendPrompt(agent,new.prompt,
      #                      context=rbionfoExp,return.type="text",...)
      msgs<-append(msgs,list(list("role" = "user","content" = new.prompt)))
      resp<-openai::create_chat_completion(model=agent$model,
                                           messages=msgs,
                                           openai_api_key = agent$openai_api_key)

      response<-resp$choices[1,4]
      msgs<-append(msgs,list(list("role" = "assistant","content" = response)))

      # clean code from wrong backticks
      response<-clean_code_blocks(response)

      # parse the code
      blocks<- extractCode(text=response,delimiter="```")

      # extract and install libs needed
      extractInstallPkg(blocks$code)


    }else{
      # break the loop if the code works without errors
      codeWorks=TRUE
      break

    }

  }


  # return the latest code and initial prompt and everthing else
  return(list(init.response=initial.response,
              init.blocks=initial.blocks,
              final.response=response,
              final.blocks=blocks,
              code.works=codeWorks,
              exec.result=res,
              tried.attempts=i))
}
