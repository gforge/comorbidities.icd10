#' Checks if it is ICD-9 or ICD-10
#' 
#' Identifies if first letter is a number or V and then suggests ICD-9. 
#' There are V-codes in ICD-10 although these are only transportation related
#' disease causes such as biking and automobile accidents. It is therefore better
#' to sort any of these codes into the ICD-9 in case they actually belong there.
#' 
#' @param codes A vector with ICD-codes
#' @param icd_ver A vector with icd versions, same length as \code{code} argument.
#'  If FALSE then all are checked otherwise only those that are FALSE are checked
#' @return \code{boolean} Returns TRUE if the code is deemed to be and ICD-10 code
#'  otherwise it returns FALSE.
pr.get.icd.ver <- function(codes, icd_ver = rep(FALSE, times=length(codes))){
  if (length(icd_ver) != length(codes)){
    if (length(icd_ver) == 1 && icd_ver[1] == FALSE){
      icd_ver <- rep(FALSE, times=length(codes))
    }else{
      stop("Your the length of the ICD version indicator (", length(icd_ver),")",
           " does not match with the number of provided codes (", length(codes), ")")
    }
  }
  
  return(as.character(ifelse(icd_ver == FALSE,
                             10 - 1*(substr(codes, 1,1) %in% c(0:9, "v","V")),
                             icd_ver)))
}

#' A internal function for generating an output vector containing the score elements
#' 
#' Throws errors if the output has an invalid format
#' 
#' @param out A vector or NULL
#' @param score The score list with element names
#' @return \code{vector} A named vector
pr.get.out.vector <- function(out, score){
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

#' Match codes with regular expressions
#' 
#' The function loops through all the codes and the different
#' regular expressions in search for a match.n 
#' 
#' @param icdCode The vector with all the icdCodes that should be tested
#' @param icd_ver The ICD-version, currently either 9 or 10
#' @param out The out vector, see \code{\link{pr.get.out.vector}}
#' @param comorbidity_regex The regular expression list with all the comorbidities
#' @param include_acute If acute diagnosis are to be included, e.g. myocardial
#'  infarction.
#' @param acute_regex A list with vectors containing  regular expressions that identify the 
#'  acute diagnoses that should be excluded if \code{include_acute} is set to TRUE
#'  for the specific code. If not provided the code will not check for acute diagnoses.
#' @return \code{vector} Returns the out with TRUE for those groups
#'  where a match was found.
#' @seealso \code{\link{pr.elixhauser_Quan2005_regex}},
#'  \code{\link{pr.charlson_Quan2005_regex}},
#'  \code{\link{pr.charlson_Armitage2010_regex}},
#'  \code{\link{pr.charlson_Sundarajan2004_regex}}
pr.regex.code.match <- 
  function(icdCode, icd_ver, out, comorbidity_regex, 
           include_acute = rep(TRUE, length(icdCode)), 
           acute_regex)
{
  if (!missing(acute_regex)
      && length(icdCode) != length(include_acute)){
    stop("Each icdCode needs to have an indicator for if the disease is acute or not",
         " if you intend to separate acute codes from the search.",
         " Currently you have '", length(icdCode), "' codes ",
         " while you have only provided '", length(include_acute), "' acute identifiers")
  }
  
  # If there is a mix of icd-10 and icd-9 codes for the given subject
  # then the loop is slightly more complicated
  if (length(unique(icd_ver))==1){
    
    # Loop through all the disease categories
    for (key_disease in names(comorbidity_regex)){
      
      # TODO: Order the regular expression according to disease frequency
      # in order to increase speed. I.e. a frequent disease in a category
      # should be prioritized as the chances for a match is greater
      for (regex_str in comorbidity_regex[[key_disease]][[paste0("icd",icd_ver[1])]]){
        # Loop through all the codes
        for (code_i in 1:length(icdCode)){
          if (is.na(icdCode[code_i])){ next }
          
          # TODO: An alternative could be to do any(grep(regex_str, icdCode))
          # but there is a risk that this would actually slow down the code 
          # as it currently stops on the first match. This should probably be
          # more closely investigated for performance.
          if (grepl(regex_str, icdCode[code_i], ignore.case=TRUE)){
            
            # Check if this is an acute diagnosis that is to be
            # skipped if we should not include acute diagnosis
            if (!missing(acute_regex) &&
                  !include_acute[code_i]){
              out[key_disease] <- TRUE
              for (ar in acute_regex[[paste0("icd",icd_ver[1])]]){
                if (grepl(ar, icdCode[code_i], ignore.case=TRUE)){
                  out[key_disease] <- FALSE
                  break
                }
              }
            }else{
              out[key_disease] <- TRUE
            }
            
            # Don't investigate the other icd-codes if 
            # the comorbidity group has already been found
            if (out[key_disease]){
              break
            }
          }
        }
        
        # Go to next disease if the current
        # comorbidity group is already found
        if (out[key_disease]) { next }
      }
    }
  }else{
    # Loop through all the codes
    for (code_i in 1:length(icdCode)){
      if (is.na(icdCode[code_i])){ next }
      
      # Loop through all the comorbidity groups
      for (key_disease in names(comorbidity_regex)){
        
        # Exit loop if comorbidity group has already been found by another icd-code
        if (out[key_disease]) { break }
        
        for (regex_str in comorbidity_regex[[key_disease]][[paste0("icd",icd_ver[code_i])]]){
          if (grepl(regex_str, icdCode[code_i], ignore.case=TRUE)){
            # Check if this is an acute diagnosis that is to be
            # skipped if we should not include acute diagnosis
            if (!missing(acute_regex) &&
                  !include_acute[code_i]){
              out[key_disease] <- TRUE
              for (ar in acute_regex[[paste0("icd",icd_ver[code_i])]]){
                if (grepl(ar, icdCode[code_i], ignore.case=TRUE)){
                  out[key_disease] <- FALSE
                  break
                }
              }
            }else{
              out[key_disease] <- TRUE
            }

            # Exit loop if disease found if this comorbidity group
            # has been found through the current regular expression
            if (out[key_disease]){ break }
          }
        }
      }
    }
  }
  
  return(out)
}