

# send prompt to an agent and get a response

rbionfoExp="Act as an expert bioformatician and R user. Answer questions using your expertise. When providing code provide the code in triple backtics and as a single block."


#' Send a prompt to a specified language model agent and return the response.
#'
#' @param agent An object containing the agent's information (e.g., type and model etc.).
#' @param prompt The prompt text to send to the language model.
#' @param context Optional context to provide alongside the prompt (default is rbionfoExp).
#' @param return.type The type of output to return, either the text response ("text") or the entire response object ("object").
#' @param ... Additional arguments to be passed to the prompt function.
#' @return The text response or the entire response object, based on the specified return type.
#' @examples
#' \dontrun{
#' agent <- setupAgent(name="openai",type="chat",model="gpt-4",
#'                     ai_api_key=Sys.getenv("OPENAI_API_KEY"))
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
  assertthat::assert_that(
    assertthat::`%has_name%`(agent,c("name","model","API","url","headers","ai_api_key","type")),
    assertthat::noNA(agent)
  )

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

  if(agent$name=="testAgent"){
    promptFunc=testPrompter
  }else if(agent$name=="userAgent"){
    if (agent$API == "openai"){
      if (agent$type=="completion"){
        promptFunc = .openai_comp
      }else if (agent$type =="chat"){
        promptFunc = .openai_chat
      }else{
        stop (cat("Agent type", agent$type ,"is not compatiable with the current setup"))
      }
    }else if (agent$API == "replicate"){
      promptFunc = .replicate_chat
    }else{
      stop(cat("the specified API",agent$API,"is not compatiable with the current setup"))
    }
  }else{
    stop("the specified LLM agent is not compatiable with the current setup")
  }

  # send the prompt and get the result
  if (return.type != "text" & return.type !="object"){
    stop (cat("Return type",return.type,"not supported"))
  }


  # build the prompt with context if exists
  final.prompt=paste(context,prompt,sep="\n")


  res<-promptFunc(agent=agent, prompt=final.prompt, return.type=return.type, ...)

  return (res)
}

# this function returns three responses one after another
# it is used to test the selfcorrect function() and maybe used for other
# tests
#' @noRd
testPrompter<-function(agent,prompt, ...){

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
.openai_comp<-function(agent, return.type, prompt,...){
  res <- openai::create_completion(model=agent$model,
             openai_api_key = agent$ai_api_key,
             prompt=prompt,...)
  if (return.type == "text"){
    return (res$choices$text)
  }else{
    return(res)
  }
}

# internal completion code for open ai
# hides specific stuff so that promptFunc works in a unified way
# across agents
#' @noRd
.openai_chat<-function(agent,prompt,return.type,...){
  args <- list(...)
  # for working with self-correct function
  if ("messages" %in% names(args)){
    res <- openai::create_chat_completion(model=agent$model,
                                   messages=args$messages,
                                   openai_api_key = agent$ai_api_key)
  }else{
    res <- openai::create_chat_completion(model=agent$model,
                                   messages=list(
                                     list(
                                       "role" = "user",
                                       "content" = prompt
                                     )),
                                   openai_api_key = agent$ai_api_key,
                                   ...)
  }
  if (return.type=="text"){
    return (res$choices[1,4])
  }else(
    return(res)
  )
}

# internal completion code for replicate AI
# hides specific stuff so that promptFunc works in a unified way
# across agents.
#' @noRd
.replicate_chat <- function(agent,prompt,return.type,...){

    #setup body for replicate request
    body <- list()
    body[["version"]] <- agent$model
    body[["input"]] <- list("prompt"= prompt)
    body[["max_new_tokens"]]<-Inf

    #send request:
    posted <- httr::POST(
      url = agent$url,
      httr::add_headers(.headers = agent$headers ),
      body = body,
      encode = "json"
    )

    #parse response to retrieve url needed for fetching response
    parsed_post <- posted %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)



    # if for some reason request did not go through
    if (!parsed_post$status =="starting"){
      print (parsed_post)
      stop ("Request failed.")
    }

    #fetch status and parse
    respons <- httr::GET(
      url = parsed_post$urls$get,
      httr::add_headers(.headers = agent$headers )
    )

    parsed_get <- respons %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    # If not yet finished request again until finished
    while (parsed_get$status!= "succeeded"){
      #fetch response and parse
      respons <- httr::GET(
        url = parsed_post$urls$get,
        httr::add_headers(.headers = agent$headers )
      )

      parsed_get <- respons %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)
      # pause .2 sec until next request
      Sys.sleep(0.2)
    }

    # flatten response when return is text. Otherwise return object
    if (return.type == "text"){
      response<-c()
      for (i in parsed_get$output){
        response <- paste0(response,i)
      }
      return(response)
    }else{
      return (parsed_get)
    }
}
