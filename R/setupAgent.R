#' Setting up openai LLM agent
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
#' @noRd
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


#' set up an online LLM API for subsequent tasks
#'
#' This function sets up an large language model API for tasks.
#' @param name Name of the API you want to use. Currently supported APIs are "openai" and "replicate"
#' @param type Specify type of model (chat or completion). This parameter only needs to be specified when using 'openai
#' @param model LLM model you wish to use.
#' @param ai_api_key personal API key for accessing LLM
#'
#' @examples
#' \dontrun{
#' myAgent <- setupAgent(name="openai",type="chat",model="gpt-4",ai_api_key=Sys.getenv("AI_API_KEY"))
#'
#' myAgent <- setupAgent(name="replicate",type=NULL,model="02e509c789964a7ea8736978a43525956ef40397be9033abf9fd2badfe68c9e3",ai_api_key=Sys.getenv("AI_API_KEY"))
#' }
#' @export


setupAgent<-function(name=c("openai","replicate"), type=NULL, model=NULL, ai_api_key=Sys.getenv("AI_API_KEY")){
  if (ai_api_key==""){
    stop("Invalid API key provided. Please set this as a string or load AI_API_KEY into your system environment.")
  }
  if (name =="replicate"){
    if (!is.null(type)){
      warning ("Type cannot be specified when using replicate. This will be ignored.")
      type=NULL
    }
    base_url = "https://api.replicate.com/v1/predictions"
    headers <- c(
      "Authorization" = paste("Token", ai_api_key),
      "Content-Type" = "application/json")
    if (is.null(model)){
      warning ("No model given. Model will be set to llama-2-70b-chat")
      model = "02e509c789964a7ea8736978a43525956ef40397be9033abf9fd2badfe68c9e3"
    }
  }else if(name == "openai"){
    if (is.null(type)){
      warning("No type selected. Will set type to chat")
      type="chat"
    }
    if (type=="chat"){
      chatModels= strsplit("gpt-4, gpt-4-0314, gpt-4-32k, gpt-4-32k-0314, gpt-3.5-turbo, gpt-3.5-turbo-0301",", ")[[1]]
      base_url ="https://api.openai.com/v1/chat/completions"
      if (is.null(model)){
        warning ("No model selected. Model will be set to gtp-3.5-turbo.")
        model = "gtp-3.5-turbo"
      }else if(!model%in% chatModels){
        stop (cat("Invalid model selected. Please choose one of the following models:\n ",chatModels))
      }
    }else if (type=="completion"){
      completionModels=strsplit("text-davinci-003, text-davinci-002, text-curie-001, text-babbage-001, text-ada-001",", ")[[1]]
      base_url ="https://api.openai.com/v1/completions"
      if (is.null(model)){
        warning ("No model selected. Model will be set to text-curie-001.")
        model = "text-curie-001"
      }else if(!model%in%completionModels){
        stop (cat("Invalid model selected. Please choose one of the following models:\n ",completionModels))
      }
    }else{
      stop (cat("Type",type,"not supported"))
    }
    headers <- c(
      "Authorization" = paste("Bearer", ai_api_key),
      "Content-Type" = "application/json")
  }else {
    stop (cat("Chosen API",name, "not supported."))
  }
  return(list(name = "userAgent",type=type,API=name, url=base_url, model=model, headers=headers,ai_api_key=ai_api_key))
}


#' setup a test agent to be used for test prompting
#' should return a random set of responses
#' @noRd
setupTestAgent<-function(agentName="testAgent"){

  return(list(name="testAgent",model="crap",type="completion"))
}
