#' Executes the code in the received response from the agent
#'
#' The function extracts and executes the code in the response. If required it can
#' try to correct errors in the code via different strategies.
#'
#' @param response response to be parsed for code and executed
#' @param prompt prompt for the response, if correction="none" it is not needed
#' @param agent  AI agent, if correction="none" it is not needed
#' @param context  context for the prompt, if correction="none" it is not needed
#' @param correction "none" no code correction is needed. "selfcorrect" feedback the errors to LLM and ask for fix.
#' "sampleMore" sample responses for the #prompt until an executable code is returned or number of attempts reached.
#' "correctThenSample" first try self-correct "attempt" number of times.
#' If no executable code is returned. It will sample new responses "attempt" number of times or until executable code
#' @param attempts Numeric value denoting how many times the code should be sent back for fixing.
#' @param output.file Optional output file created holding parsed code
#' @param ... arguments to sendPrompt()
#' @return A list containing the following elements:
#' \item{init.response}{A character vector representing the initial prompt response.}
#' \item{init.blocks}{A list of initial blocks.}
#' \item{final.blocks}{A list of final blocks.}
#' \item{code.works}{A boolean value indicating whether the code works.}
#' \item{exec.result}{A character string representing the execution results.}
#' \item{tried.attempts}{An integer representing the number of attempts.}
#' @seealso \code{\link{selfcorrect}},\code{\link{sampleResponse}}
#' @examples
#' \dontrun{
#'
#' resp.list <- runCodeInResponse(agent,prompt,context=rbionfoExp,correction="sampleMore",attempt=2)
#' }
#' @export
runCodeInResponse<-function(
  response,
  prompt=NULL,
  agent=NULL,
  context = NULL,
  correction=c("none","selfcorrect","sampleMore","sampleThenCorrect","correctThenSample"),
  attempts = 3,
  output.file = NULL,
  ...
){

  # check optional arguments
  if(any( !is.null(prompt),!is.null(agent),!is.null(context))){

    if( (is.null(prompt)+is.null(agent)+is.null(context))>1 ){
      stop("\nif any of the following arguments is provided, all of them must be provided:\n",
           "'prompt','agent' and 'context'\n",
           "no NULL values are allowed in that case for those arguments"
      )
    }
  }



  # Clean the code backtick structure and install.packages calls
  initial.response<-response
  response<-clean_code_blocks(response)



  # Parse the code
  blocks <- extractCode(text=response,delimiter="```")

  #execute response
  if(is.null(output.file)){
      res<-executeCode(blocks$code, output = "eval",output.file = output.file )

  }else{
      res<-executeCode(blocks$code, output = "html",output.file = output.file )

  }

  first.res<-list(init.response=initial.response,
                  init.blocks=blocks,
                  final.response=initial.response,
                  final.blocks=blocks,
                  code.works=!(is.list(res) & ("error" %in% names(res) )),
                  exec.result=res,
                  tried.attempts=1)

  # if code works or no correction asked
  if(correction=="none" | first.res$code.works){
    return(first.res)

  }else if(correction=="selfcorrect"){

    res.list<-selfcorrect(agent,prompt,
                       context=context,attempts,output.file,
                       responseWithError=list(response,res),
                       ...)

    return(res.list)
  }else if(correction=="sampleMore"){
    res.list<-sampleResponse(agent,prompt,
                        context=context,attempts,output.file,
                        responseWithError=list(response,res),
                        ...)
    return(res.list)
  }else if(correction=="sampleThenCorrect"){

    res.list<-sampleResponse(agent,prompt,
                        context=context,attempts,output.file,
                        responseWithError=list(response,res),
                        ...)
    if(!res.list$code.works){
      res.list<-selfcorrect(agent,prompt,
                               context=context,attempts,output.file,
                               responseWithError=list(response,res),
                               ...)
    }

    return(res.list)
  }else if(correction=="correctThenSample"){

    res.list<-selfcorrect(agent,prompt,
                          context=context,attempts,output.file,
                          responseWithError=list(response,res),
                          ...)

    if(!res.list$code.works){
      res.list<-sampleResponse(agent,prompt,
                            context=context,attempts,output.file,
                            responseWithError=list(response,res),
                            ...)
    }

    return(res.list)
  }


}
