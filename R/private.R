#' Function for converting na to 0
#' 
#' @param input.frame Input data frame
#' @return output.data.frame
apply.convert.na <- function(input.frame) {
  # TODO: the NAs should actually just be skipped for performance
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  for (i in 1:NCOL(input.frame)){
    output.frame[,i] <- ifelse(is.na(input.frame[,i]), 
                               0, input.frame[,i])
  }
  return(output.frame)
}

#' Checks if it is ICD-9 or ICD-10
#' 
#' Identifies if first letter is a number or V and then suggests ICD-9. 
#' There are V-codes in ICD-10 although these are only transportation related
#' disease causes such as biking and automobile accidents. It is therefore better
#' to sort any of these codes into the ICD-9 in case they actually belong there.
#' 
#' @param code A string with the ICD-code
#' @return \code{boolean} Returns TRUE if the code is deemed to be and ICD-10 code
#'  otherwise it returns FALSE.
prIsICD10Code <- function(code){
  # Probably faster than grep("^[0-9v]", code, ignore.case=TRUE)
  if (substr(code, 1,1) %in% c(0:9, "v","V")){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#' A internal function for generating an output vector containing the score elements
#' 
#' Throws errors if the output has an invalid format
#' 
#' @param out A vector or NULL
#' @param score The score list with element names
#' @return \code{vector} A named vector
prGetOutVector <- function(out, score){
  # Create a new output vector if it is
  # missing or has length 0, i.e. out == NULL
  if (missing(out) || 
        length(out) == 0){
    out <- rep(FALSE, times=length(score))
    names(out) <- names(score)
    return (out)
  }
  
  if(length(out) != length(score)){
    stop("You have provided a list from previous runs that does not seem to originate",
         " from the score calculation, it has the length of '", length(out), "'",
         " while it should have the length '", length(score), "'")
  }
  
  if(!all(names(score) %in% names(out))){
    stop("You have provided a list from previous runs that does not seem to originate",
         " from the score calculation, it has the correct length",
         " but lacks the elements: ", 
         paste0("'",
                names(score)[!names(score) %in% names(out)],
                "'",
                collapse="','"))
    
  }
  
  # The out vector seems to be correctly formatted
  # hence return it
  return(out)
  
}

#' Loops through score codes and checks for match
#' 
#' Fills in the out vector with the positive findings
#' 
#' @param out The out vector, see \code{\link{prGetOutVector}}
#' @param icdCode The code that we want to test for match
#' @param score_codes A list with different icd groups of interest.
#'  Each icd-group should also have an icd-9 or icd-10 indicator
#' @param code_is_icd10 True if the \code{icdCode} is of icd-10 format
#' @param include_acute Under some circumstances you want to avoid acute
#'  diagnoses to be included, for instance if a knee arthroplasty is
#'  performed the myocardial infarction has most probably occurred 
#'  post-operatively and should not be part of the comorbidity calculation.
#'  Set this to \code{TRUE} if you want the function to avoid acute
#'  diagnosis according to the \code{acute_identifier_regex} regular
#'  expression, default is \code{FALSE}.
#' @param acute_identifier_regex The regular expression used if \code{avoid_acute}
#'  is set to \code{TRUE}.
#' @return \code{vector} Returns the out with TRUE for those groups
#'  where a match was found.
prCheckCodes <- function(out, icdCode, 
                         score_codes, code_is_icd10,
                         include_acute = FALSE,
                         acute_identifier_regex){
  # TODO: Check which codes are non-unique and need to loop further
  #       there should be substantial benefit if the code stepped out
  #       from the function as it hits the first match
  for (key_disease in names(score_codes)){
    # Don't check the disease if it has already been found
    if (out[key_disease] == 0){
      for (regex_str in score_codes[[key_disease]][[ifelse(code_is_icd10, 
                                                           "icd10", "icd9")]]){
        if (grepl(regex_str, icdCode, ignore.case=TRUE)){
          # Check if acute match
          if (acute & !missing(acute_identifier_regex)){
            if(grepl(acute_identifier_regex, icdCode, ignore.case=TRUE))
              next;
          }

          out[key_disease] <- TRUE
          break;
        }
      }
    }
  }
  
  return(out)
}
