#' Setting up LLM agent
#'
#' This function is used for the
#' setup of the online large language model (LLM) API for subsequent tasks
#'
#' @param model Specify LLM to be used
#' @param type Specify type of model (chat or completion)
#' @param openai_api_key openAI API key
#' @param openai_organization openAI organization
#'
#' @examples
#' \dontrun{
#' agent=setupopenaiAgent(model="text-davinci-003",type="completion", openai_api_key="")
#' }
#' @export
setupopenaiAgent<-function(model,type=c("chat","completion"),
                           openai_api_key= Sys.getenv("OPENAI_API_KEY"),
                           openai_organization = NULL
                           ){
  chatModels= strsplit("gpt-4, gpt-4-0314, gpt-4-32k, gpt-4-32k-0314, gpt-3.5-turbo, gpt-3.5-turbo-0301",", ")[[1]]
  completionModels=strsplit("text-davinci-003, text-davinci-002, text-curie-001, text-babbage-001, text-ada-001",", ")[[1]]

  if(type=="chat" && (! model %in% chatModels) ){
    warning("provided model \'",model,"\' is not one of the provided models from openai\n","check openAI API for current models that can be used for chat")
  }else if(type=="completion" && (! model %in% completionModels) ){
    warning("provided model \'",model,"'\ is not one of the provided models from openai\n","check openAI API for current models that can be used for completion")
  }

  return(list(name="openai",model=model,type=type,
              openai_api_key=openai_api_key,
              openai_api_key=openai_organization))
}


#' setup any online  large language model (LLM) API for subsequent tasks
#' parameters of the agent
#' sysgetenv default to something different!
#' @noRd
setupAgent<-function(URL, task, model, ai_api_key, authorization_name){
  base_url <- glue::glue("{URL}{task}")
  headers <- c(
    "Authorization" = paste(authorization_name, ai_api_key),
    "Content-Type" = "application/json")
  return(list(name="userAgent",url=base_url, model=model, headers=headers,ai_api_key=ai_api_key))
}


#' setup a test agent to be used for test prompting
#' should return a random set of responses
#' @noRd
setupTestAgent<-function(agentName="testAgent"){

  return(list(name="testAgent",model="crap",type="completion"))
}
