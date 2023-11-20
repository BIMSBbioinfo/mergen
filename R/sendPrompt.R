

# send prompt to an agent and get a response

rbionfoExp="Act as an expert bioformatician and R user. Answer questions using your expertise. When providing code provide the code in triple backtics and as a single block."


#' Send a prompt to a specified language model agent and return the response.
#'
#' @param agent An object containing the agent's information (e.g., type and model).
#' @param prompt The prompt text to send to the language model.
#' @param context Optional context to provide alongside the prompt (default is rbionfoExp).
#' @param return.type The type of output to return, either the text response ("text") or the entire response object ("object").
#' @param ... Additional arguments to be passed to the prompt function.
#' @return The text response or the entire response object, based on the specified return type.
#' @examples
#' \dontrun{
#' agent <- list(agent = "openai", type = "chat", model = "gpt-4", openai_api_key = "your_api_key")
#' prompt <- "tell me a joke"
#' response <- sendPrompt(agent, prompt)
#'
#' response <- sendPrompt(agent,prompt,context=rbionfoExp,return.type="text", max_tokens = 500)
#' }
#' @import openai
#'
#' @export
sendPrompt<-function(agent,prompt,context=rbionfoExp,
                     return.type=c("text","object"),...){

  # argument validation
  #-----------------------------------------------------------------------------

  if(agent$name=="openai"){
    assertthat::assert_that(
      assertthat::`%has_name%`(agent,c("name","model","type","openai_api_key")),
      assertthat::noNA(agent)
    )
  }

  assertthat::assert_that(
    assertthat::is.string(prompt),
    assertthat::noNA(prompt)
  )

  if (!is.null(context)) {
    assertthat::assert_that(
      assertthat::is.string(context),
      assertthat::noNA(context)
    )
  }

  assertthat::assert_that(
    assertthat::is.string(return.type),
    assertthat::noNA(return.type)
  )

  # -------------------------------------------------------------------------------------

  # define the prompt function based on openai
  if(agent$name=="openai"){

    if(agent$type=="chat"){
      # TODO may need to wrap this up to higher function to unify access
      # to promptFunc
      promptFunc=.openai_chat

    }else if(agent$type=="completion"){
      promptFunc=.openai_comp
    }

  }else if(agent$name=="testAgent"){
    promptFunc=testPrompter
  }else if(agent$name=="userAgent"){
    promptFunc=.userPrompter
  }else{
    stop("the specified LLM agent is not compatiable with the current setup")
  }

  # build the prompt with context if exists
  final.prompt=paste(context,prompt,sep="\n")

  # send the prompt and get the result
  res<-promptFunc(agent=agent, prompt=final.prompt, ...)

  if(return.type=="text" & agent$name=="openai" ){

    if(agent$type=="chat"){
      return(res$choices[1,4])

    }else if(agent$type=="completion"){
      return(res$choices[1,1])
    }
  }else{
    return(res)
  }

}

# this function returns three responses one after another
# it is used to test the selfcorrect function() and maybe used for other
# tests
#' @noRd
testPrompter<-function(agent,prompt,...){

  # Define a static variable to keep track of the count
  if (!exists("prompterCount")) {
    prompterCount <<- 0
  }

  if(prompterCount >= 3){prompterCount <<- 0}

  prompterCount  <<- prompterCount  + 1

  # list of responses where it gradually gets the correct code
  botResponses=list(
    "\n\nThe following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n```\nnormalized.data <- scale(data, center = TRUE, scale = TRUE)\n```\n\nFinally, the normalized data will be used to do a Principal Component Analysis (PCA):\n\n```\npca <- princomp(normalized.data)\n```",

    "\n\nThe second response.The following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n```\nnormalized.data <- scale(data, center = TRUE, scale = TRUE)\n```\n\nFinally, the normalized data will be used to do a Principal Component Analysis (PCA):\n\n```\npca <- princomp(normalized.data)\n```",

    "\n\nThe third response.The following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\nplot(1:10)```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n"
    )

  return(botResponses[[prompterCount]])
}

# internal completion code for open ai
# hides specific stuff so that promptFunc works in a unified way
# across agents
#' @noRd
.openai_comp<-function(agent,...){

  openai::create_completion(model=agent$model,
             openai_api_key = agent$openai_api_key, ...)
}

# internal completion code for open ai
# hides specific stuff so that promptFunc works in a unified way
# across agents
#' @noRd
.openai_chat<-function(agent,prompt,...){
  args <- list(...)
  if ("messages" %in% names(args)){
    openai::create_chat_completion(model=agent$model,
                                   messages=args$messages,
                                   openai_api_key = agent$openai_api_key)
  }else{
    openai::create_chat_completion(model=agent$model,
                                   messages=list(
                                     list(
                                       "role" = "user",
                                       "content" = prompt
                                     )),
                                   openai_api_key = agent$openai_api_key,
                                   ...)
  }
}

# internal completion code for user ai
# hides specific stuff so that promptFunc works in a unified way
# across agents
# how to send prompt to agent in unified way. Think about this!
#' @noRd
.userPrompter <- function(agent,prompt,...){

  #check
  print("imhere")
  print(agent$url)

  #setup body for request
  #specific per url used!
  body <- list()
  body[["model"]] <- agent$model
  body[["messages"]] <- list(list("role"="user","content"=prompt))
  body[["user"]] <- NULL
  body[["temperature"]] <- 1
  body[["top_p"]] <- 1
  body[["n"]] <- 1
  body[["stream"]] <- FALSE
  body[["stop"]] <- NULL
  body[["max_tokens"]] <- NULL
  body[["presence_penalty"]] <- 0
  body[["frequency_penalty"]] <- 0
  body[["logit_bias"]] <- NULL

  # send request
  response <- httr::POST(
    url = agent$url,
    httr::add_headers(.headers = agent$headers),
    body = body,
    encode = "json"
  )

  parsed <- response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
}
