#' Extract file headers from files in prompt
#'
#' This function extracts file headers  from
#' files mentioned in user prompts.
#' @param filenames list containing filenames mentioned in prompt
#'
#' @examples
#' \dontrun{
#' fileHeaderPrompt(filenames)
#' }
#' @export
fileHeaderPrompt<-function(filenames){

  output="\nhere are first few lines of the file(s).\n"
  for(filename in unique(filenames)){
    print(filename)
    output=paste0(output,"\n",filename,":\n")

    if(grepl("\\.(xls|xlsx)$", filename, ignore.case = TRUE)){
      requireNamespace("readxl",quietly = TRUE)

      data <- readxl::read_excel(filename,  n_max=2)

      # Convert the data frame to a string with \n and \t separators
      data_string <- apply(data, 1, function(row) paste(row, collapse = "\t"))
      data_string <- paste(data_string, collapse = "\n")

      # add header
      header<-paste(colnames(data),collapse = "\t")
      data_string<-paste0(header,"\n",data_string)

      output<-paste0(output,data_string,"\n\n")

    }else{
      data_string<-paste(readLines(filename,n=2),collapse = "\n")
      output<-paste0(output,data_string,"\n\n")

    }

  }
  return(output)
}
