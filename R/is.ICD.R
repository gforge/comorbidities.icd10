#' Check if icd code is valid after pre-processing
#' 
#' The codes need to be preprocessed and the . removed. If the . is
#' left then the cmrbdt.finder will have issues.
#' 
#' @param codes The ICD codes of interest that have the needed format
#'  for the functions to work. That is without any ., etc.
#' @param preprocess_fn Any pre-processing function that is needed to
#'  prepare the codes.
#' @return \code{vector} Returns a vector with all the codes
#' @examples
#' is.ICD(c("M161", "M16.1", "1998-01-01", "4521"))
#' 
#' @importFrom stringr str_trim
#' @export
is.ICD <- function(codes, preprocess_fn){
  if (!missing(preprocess_fn)){
    if (is.character(preprocess_fn)){
      if (!exists(preprocess_fn))
        stop("The pre-processing function '", preprocess_fn, "' does not seem to exist")
      preprocess_fn <- get(preprocess_fn)
    }
    
    codes <- preprocess_fn(codes)
  }
  
  codes <- str_trim(codes)
  
  # No identification codes rely on more than 4 letters and 
  # there are plenty of local variations in the last letters
  # therefore it is better to remove this
  codes <- ifelse(nchar(codes) > 4, 
                  substr(codes, 1, 4),
                  codes)
  return(grepl(paste0("^([A-Za-z][0-9]{2,4}[0-9AJKBX]{0,1}",
                      "|[EVev0-9][0-9]{1,3})$"), codes))
}