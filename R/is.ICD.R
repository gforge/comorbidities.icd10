#' Check if icd code is valid
#' 
#' @param codes The ICD codes of interest that have the needed format
#'  for the functions to work. That is without any ., etc.
#' @param preprocess_fn Any pre-processing function that is needed to
#'  prepare the codes.
#' @return \code{vector} Returns a vector with all the codes
#' @examples
#' is.ICD(c("M161", "M16.1", "1998-01-01", "4521"))
#' @export
is.ICD <- function(codes, preprocess_fn){
  if (!missing(preprocess_fn)){
    if (is.character(preprocess_fn)){
      if (!exists(preprocess_fn))
        stop("The pre-processing function '", preprocess_fn, "'' does not seem to exist")
      preprocess_fn <- get(preprocess_fn)
    }
  }
  
  return(grepl("^([A-Za-z][0-9]{2}[^\\.]|[EV0-9][0-9]+)$", codes))
}