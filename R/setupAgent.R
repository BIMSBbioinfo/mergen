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
  chatModels= getModels(openai_api_key)
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
#' @param name A string for the name of the API, one of "openai", "replicate" or "generic".
#'             Currently supported APIs are "openai" and "replicate". If the user wishes to use
#'             another API that has similar syntax to openai API this is also supported via the
#'             the "generic" option. In this case, the user should also provide a url for the API
#'             using the
#' @param type Specify type of model (chat or completion). This parameter only needs to be specified when using 'openai
#' @param model LLM model you wish to use.
#' For openAI chat model examples are:
#' \itemize{
#' \item 'gtp-3-5-turbo'
#' \item 'gtp-4'}
#' For openAI completion models examples are:
#' \itemize{
#' \item 'text-curie-001'
#' \item 'text-davinci-002'}
#' For replicate models examples are:\itemize{
#'  \item llama-2-70b-chat ( as '02e509c789964a7ea8736978a43525956ef40397be9033abf9fd2badfe68c9e3')
#'  \item llama-2-13b-chat ( as 'f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d')}
#'  For a full list of openAI models see
#'  https://platform.openai.com/docs/models/overview/. For a full list of Replicate models,
#'  see https://replicate.com/collections/language-models.
#' @param url the url for the API in case the API "generic" is selected. (Default: NULL)
#' @param ai_api_key personal API key for accessing LLM
#' @return A list holding agent information.
#' @examples
#' {
#' myAgent <- setupAgent(name="openai",type="chat",model="gpt-4",ai_api_key="my_key")
#'
#' myAgent <- setupAgent(name="replicate",type=NULL,
#'                      model="02e509c789964a7ea8736978a43525956ef40397be9033abf9fd2badfe68c9e3",
#'                      ai_api_key="my_key")
#' }
#' @export


setupAgent<-function(name=c("openai","replicate","generic"),
                     type=NULL, model=NULL,url=NULL, ai_api_key=Sys.getenv("AI_API_KEY")){

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
      chatModels= getModels(ai_api_key)
      base_url ="https://api.openai.com/v1/chat/completions"
      if (is.null(model)){
        warning ("No model selected. Model will be set to gtp-3.5-turbo.")
        model = "gpt-3.5-turbo"
      }else if(!model%in% chatModels){
        stop(paste("Invalid model selected. Please choose one of the following models:\n ",chatModels))
      }
    }else if (type=="completion"){
      completionModels=strsplit("text-davinci-003, text-davinci-002, text-curie-001, text-babbage-001, text-ada-001",", ")[[1]]
      base_url ="https://api.openai.com/v1/completions"
      if (is.null(model)){
        warning ("No model selected. Model will be set to text-curie-001.")
        model = "text-curie-001"
      }else if(!model%in%completionModels){
        stop(paste("Invalid model selected. Please choose one of the following models:\n ",completionModels))
      }
    }else{
      stop(paste("Type",type,"not supported"))
    }
    headers <- c(
      "Authorization" = paste("Bearer", ai_api_key),
      "Content-Type" = "application/json")
  }
  else if(name=="generic"){

    base_url=url
    headers <- c(
      "Authorization" = paste("Bearer", ai_api_key),
      "Content-Type" = "application/json")
  }else {
    stop("Chosen API ",name, " not supported.")
  }
  return(list(name = "userAgent",type=type,API=name, url=base_url, model=model, headers=headers,ai_api_key=ai_api_key))
}


#' setup a test agent to be used for test prompting
#' should return a random set of responses
#' @noRd
setupTestAgent<-function(agentName="testAgent"){

  return(list(name="testAgent",model="crap",type="completion"))
}

#' getting available openai models
#' Returns a list of available models.
#' @noRd
#
getModels <- function (api_key){
  # Define the URL for the models endpoint
  url <- "https://api.openai.com/v1/models"

  # Make the GET request to fetch the list of models
  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    usable_models <-c()
    # Parse the response to extract the list of models
    models <- httr::content(response, "text", encoding = "UTF-8")
    models <- jsonlite::fromJSON(models)
    # Get available models
    for (i in 1:length(models$data$id)){
      curr_mod <- models$data$id[i]
      if (grepl("gpt",curr_mod) & (!grepl("personal",curr_mod)) & (!grepl('instruct',curr_mod))){
        usable_models <- c(usable_models,curr_mod)
      }
    }
    return (usable_models)


  } else {
    usable_models<-strsplit("gpt-4, gpt-4-0314, gpt-4-32k, gpt-4-32k-0314, gpt-3.5-turbo, gpt-3.5-turbo-0301, gpt-4o-mini, gpt-4o",", ")[[1]]
    return (usable_models)
  }
}

