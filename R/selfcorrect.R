#' Self correct the code returned by the agent
#'
#' The function attempts to correct the code returned by the agent
#' by re-feeding to the agent with the error message. If there are no
#' error messages function returns the original response.
#'
#' @param agent An object containing the agent's information (e.g., type and model).
#' @param prompt The prompt text to send to the language model.
#' @param context Optional context to provide alongside the prompt (default is rbionfoExp).
#' @param attempts Numeric value denoting how many times the code should be sent back for fixing.
#' @param output.file Optional output file created holding parsed code
#' @param responseWithError a list of response and errors returned from executeCode().
#' @param history parameter to send history of the chat.
#' First element is expected to be the response and the second element is the error list returned by executeCode().
#' @param ... Additional arguments to be passed to the \code{\link{sendPrompt}} function.
#' @return A list containing the following elements:
#' \item{init.response}{A character vector representing the initial prompt response.}
#' \item{init.blocks}{A list of initial blocks.}
#' \item{final.blocks}{A list of final blocks.}
#' \item{code.works}{A boolean value indicating whether the code works.}
#' \item{exec.result}{A character string representing the execution results.}
#' \item{tried.attempts}{An integer representing the number of attempts.}
#' @seealso \code{\link{promptContext}} for predefined contexts to use.
#' @examples
#' \dontrun{
#'
#' response <- selfcorrect(agent,prompt,context=rbionfoExp, max_tokens = 500)
#' }
#' @export
selfcorrect<-function(agent,prompt,context=rbionfoExp,attempts=3,output.file=NULL,responseWithError=NULL, history=NULL,...){

  #---------------------------------------------------------------------------
  # Validate arguments
  assertthat::assert_that(
    assertthat::`%has_name%`(agent,c("name","model","API","url","headers","ai_api_key","type")),
    assertthat::noNA(agent)
  )

  assertthat::assert_that(
    assertthat::is.string(prompt),
    assertthat::noNA(prompt)
  )

  assertthat::assert_that(
    assertthat::is.string(context)
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
  if (agent$API =="openai"){
    if (agent$type == "completion"){
      stop("selfcorrect cannot be used with type completion. Can only be used with type chat.")
    }
    }

  # Define the prompt template to inject the error message
  promptTemplate <- "The previous code above returned the following errors and/or warnings,\n <error> \n fix errors and return the fixed code in one block, delimited in triple backticks"

  # Set up the on of the final variables that will be returned in the end
  codeWorks=FALSE

  attmpt.st=1 # attempt counter starting point

  if(is.null(output.file)){output="eval"} # this doesn't do anything yet

  if(is.null(responseWithError)){


    # Send prompt
    if (is.null(history)){
      response <- sendPrompt(agent,prompt,context,return.type="text",...)
    }else{
      response <- sendPrompt(agent,prompt,context,return.type="text",previous.msgs=history)
    }

    # Clean the code backtick structure and install.packages calls
    init.response <- response
    response<-clean_code_blocks(response)

    initial.response <- response


    # Parse the code
    blocks <- extractCode(text=initial.response,delimiter="```")
    initial.blocks <- blocks

    # Check if any code is returned
    if(blocks$code==""){
      message(response)
      stop("no code returned")

    }

    # Extract and install packages if needed
    extractInstallPkg(blocks$code)


    # List of messages to the bot
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


  }else if(length(responseWithError)==2 && is.list(responseWithError)){

    message("attempt number ",1," returned error, trying to fix the error")
    attmpt.st=2

    # process errors
    # Collapse the character vectors within the list elements
    # This is good if we have multiple errors in the list per element
    err0<-responseWithError[[2]]
    collapsed_list <- lapply(err0, function(x) paste(x, collapse = "\n"))

    # Get error/warning text
    errs<-  paste(paste0(names(collapsed_list ), ": ", collapsed_list ), collapse = "\n ")


    # get new prompt
    # Use sub() to substitute the replacement string for the wildcard string
    promptAddon <- sub("<error>", errs, promptTemplate)

    new.prompt<-promptAddon

    msgs<- list(
      list(
        "role" = "system",
        "content" = context
      ),
      list(
        "role"="user",
        "content"= prompt
      ),
      list(
        "role"="assistant",
        "content"= responseWithError[[1]]
      )
    )
    # send prompt
    response <- sendPrompt(agent=agent, prompt=new.prompt,
                           context=context,
                           return.type = "text",previous.msgs = msgs ,...)

    msgs<-append(msgs,list(list("role" = "user","content" = new.prompt)))

    # Clean the code backtick structure and install.packages calls
    init.response <- response

    response<-clean_code_blocks(response)

    msgs<-append(msgs,list(list("role" = "assistant","content" = response)))

    initial.response <- response


    # Parse the code
    blocks <- extractCode(text=initial.response,delimiter="```")
    initial.blocks <- blocks

    # Check if any code is returned
    if(blocks$code==""){
      message(response)
      stop("no code returned")

    }

    # Extract and install packages if needed
    extractInstallPkg(blocks$code)

  }


  # Execute the code up to "attempts" times
  for(i in   attmpt.st:attempts){

    message("attempt number ",i," started for fixing the code in the response")

    # See if the code runs without errors
    res<-executeCode(blocks$code, output = "html",output.file = output.file )

    # If there are errors
    if(is.list(res) & ("error" %in% names(res) )){

      # Get error messages
      message("attempt number ",i," returned error, trying again for a fix")

      # Collapse the character vectors within the list elements
      # This is good if we have multiple errors in the list per element
      collapsed_list <- lapply(res, function(x) paste(x, collapse = "\n"))

      # Get error/warning text
      errs<-  paste(paste0(names(collapsed_list ), ": ", collapsed_list ), collapse = "\n ")

      # Use sub() to substitute the replacement string for the wildcard string
      promptAddon <- sub("<error>", errs, promptTemplate)

      # Get an updated prompt
      #new.prompt<-paste(response,promptAddon)
      new.prompt<-promptAddon


      # Send prompt
      response <- sendPrompt(agent=agent, prompt=new.prompt,context=context,
                             return.type = "text",previous.msgs=msgs,...)
      msgs<-append(msgs,list(list("role" = "assistant","content" = response)))

      # Clean code from wrong backticks
      response<-clean_code_blocks(response)

      # Parse the code
      blocks<- extractCode(text=response,delimiter="```")

      # Extract and install libs needed
      extractInstallPkg(blocks$code)


    }else{
      # Break the loop if the code works without errors
      codeWorks=TRUE
      break

    }

  }


  # Return the latest code, initial prompt, and everything else
  return(list(init.response=init.response,
              init.blocks=initial.blocks,
              final.response=response,
              final.blocks=blocks,
              code.works=codeWorks,
              exec.result=res,
              tried.attempts=i))
}



#' Sample more solutions when non-executable code returned by the agent
#'
#' The function attempts to sample more solutions by the agent when
#' the code returned by the agent is faulty. The function simply asks for solutions
#' until an executable code is returned or until number of attempts is reached.
#  If there are no error messages, the function returns the original response.
#'
#' @param agent An object containing the agent's information (e.g., type and model).
#' @param prompt The prompt text to send to the language model.
#' @param context Optional context to provide alongside the prompt (default is rbionfoExp).
#' @param attempts Numeric value denoting how many times the code should be sent back for fixing.
#' @param output.file Optional output file created holding parsed code
#' @param responseWithError a list of response and errors returned from executeCode().
#' First element is expected to be the response and the second element is the error list returned by executeCode().
#' @param ... Additional arguments to be passed to the \code{\link{sendPrompt}} function.
#' @return A list containing the following elements:
#' \item{init.response}{A character vector representing the initial prompt response.}
#' \item{init.blocks}{A list of initial blocks.}
#' \item{final.blocks}{A list of final blocks.}
#' \item{code.works}{A boolean value indicating whether the code works.}
#' \item{exec.result}{A character string representing the execution results.}
#' \item{tried.attempts}{An integer representing the number of attempts.}
#' @seealso \code{\link{promptContext}} for predefined contexts to use.
#' @examples
#' \dontrun{
#'
#' resp.list <- sampleResponse(agent,prompt,context=rbionfoExp, max_tokens = 500)
#' }
#' @export
sampleResponse<-function(agent,prompt,
                         context=rbionfoExp,attempts=3,output.file=NULL,responseWithError=NULL,...){

  #---------------------------------------------------------------------------
  # Validate arguments
  assertthat::assert_that(
    assertthat::`%has_name%`(agent,c("name","model","API","url","headers","ai_api_key","type")),
    assertthat::noNA(agent)
  )

  assertthat::assert_that(
    assertthat::is.string(prompt),
    assertthat::noNA(prompt)
  )

  assertthat::assert_that(
    assertthat::is.string(context)
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
  if (agent$API =="openai"){
    if (agent$type == "completion"){
      stop("selfcorrect cannot be used with type completion. Can only be used with type chat.")
    }
  }

  # Define the prompt template to inject the error message
  promptTemplate <- "The previous code above returned the following errors and/or warnings,\n <error> \n fix errors and return the fixed code in one block, delimited in triple backticks"

  # Set up the on of the final variables that will be returned in the end
  codeWorks=FALSE
  attmpt.st=1 # attempt counter starting point

  if(is.null(responseWithError)){


    # Send prompt
    response <- sendPrompt(agent,prompt,context,return.type="text",...)

    # Clean the code backtick structure and install.packages calls
    init.response <- response
    response<-clean_code_blocks(response)

    initial.response <- response


    # Parse the code
    blocks <- extractCode(text=initial.response,delimiter="```")
    initial.blocks <- blocks

    # Check if any code is returned
    if(blocks$code==""){
      message(response)
      stop("no code returned")

    }

    # Extract and install packages if needed
    extractInstallPkg(blocks$code)


    # List of messages to the bot
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


  }else if(length(responseWithError)==2 && ("error" %in% names(responseWithError[[2]]) ) ){

    message("attempt number ",1," returned error, sampling LLM response again for a solution")
    attmpt.st=2
    # send prompt again for a new and potentially different response
    response <- sendPrompt(agent=agent, prompt=prompt,context=context,
                           return.type = "text",...)


    # Clean the code backtick structure and install.packages calls
    init.response <- response

    response<-clean_code_blocks(response)

    initial.response <- response


    # Parse the code
    blocks <- extractCode(text=initial.response,delimiter="```")
    initial.blocks <- blocks

    # Check if any code is returned
    if(blocks$code==""){
      message(response)
      stop("no code returned")

    }

    # Extract and install packages if needed
    extractInstallPkg(blocks$code)

  }


  # Execute the code up to "attempts" times
  for(i in  attmpt.st:attempts){

    message("attempt number ",i," started: sampling a new and hopefully better LLM response")
    # See if the code runs without errors
    res<-executeCode(blocks$code, output = "html",output.file = output.file )

    # If there are errors
    if(is.list(res) & ("error" %in% names(res) )){


      message("attempt number ",i," returned error, sampling LLM response again for a solution")

      # Send prompt
      response <- sendPrompt(agent=agent, prompt=prompt,context=context,
                             return.type = "text",...)

      # Clean code from wrong backticks
      response<-clean_code_blocks(response)

      # Parse the code
      blocks<- extractCode(text=response,delimiter="```")

      # Extract and install libs needed
      extractInstallPkg(blocks$code)


    }else{
      # Break the loop if the code works without errors
      codeWorks=TRUE
      break

    }

  }


  # Return the latest code, initial prompt, and everything else
  return(list(init.response=init.response,
              init.blocks=initial.blocks,
              final.response=response,
              final.blocks=blocks,
              code.works=codeWorks,
              exec.result=res,
              tried.attempts=i))
}




