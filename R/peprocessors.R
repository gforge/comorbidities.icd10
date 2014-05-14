#' Translator from Swedish ICD-9 to regular ICD-9
#' 
#' Sweden decided to make a few adaptations to the ICD-9
#' system. One of them was to change the last 4th digit into
#' a letter, A:H,W,X, corresponding to the numbers 0-9. 
#' 
#' @param icd A vector or a single string with the ICD-9-code
#' @param icd_ver An indicator of the ICD-version corresponding to each code
#' @return \code{vector} Returns a string/vector with the ICD-9 
#'  codes.
#' @export
#' @examples
#' preproc.Swedich.ICD9(c("418X", "401A"))
#' @family preprocessor functions
preproc.Swedich.ICD9 <- function(icd, icd_ver){
  if (missing(icd_ver)) { 
    icd_ver = rep(9, times=length(icd))
  }else if (length(icd_ver) == 1){
    if (icd_ver == FALSE){
      icd_ver <- 
        pr.get.icd.ver(icd)
    }else{
      icd_ver = rep(icd_ver, times=length(icd))
    }
  }else if (length(icd_ver) == length(icd)){
    if (any(icd_ver == FALSE)){
      icd_ver[icd_ver == FALSE] <- 
        pr.get.icd.ver(icd[icd_ver == FALSE])
    }
  }else{
    stop("You have provided invalid icd version information.",
         " The function expects either a single element or",
         " a vector with the same number of elements as the icd-string.",
         " The icd_ver you've provided are: ", paste(icd_ver, collapse=", "),
         " of length ", length(icd_ver), 
         " while the icd codes: ", paste(icd, collapse=", "),
         " have a length of ", length(icd))
  }
  
  if (all(icd_ver != 9)){
    return(icd)
  }
  
  icd[icd_ver ==  9] <-
    sapply(icd[icd_ver ==  9], 
           USE.NAMES=FALSE,
           FUN=function(code){
             translator <- c(A = "0",
                             B = "1",
                             C = "2",
                             D = "3",
                             E = "4",
                             F = "5",
                             G = "6",
                             H = "7",
                             W = "8",
                             X = "9")
             if(nchar(code) > 3){
               letter = toupper(substr(code, 4, 4))
               if (!letter %in% names(translator))
                 stop("Error - the Swedish ICD-9 string seems invalid: ", code,
                      " as it does not contain a valid letter at",
                      " the fourth position (", letter, ")")
               if (nchar(code) > 5){
                 return(paste0(substr(code, 1, 3), 
                               translator[letter],
                               substring(code, 5)))
               }
               
               return(paste0(substr(code, 1, 3), 
                             translator[letter]))
             }
             
             return(code)
           })
  
  return(icd)
}

#' Dot stripper
#' 
#' ICD-codes have frequently \code{.} in them,
#' e.g. \code{M16.1}. These need to be stripped
#' for the code to work.
#' 
#' @param icd The ICD-codes to be stripped
#' @param icd_ver Currently not used - this parameter is only for compatibility
#'  reasons as other preprocessing functions may require the knowledge of actual
#'  icd version.
#' 
#' @return \code{vector/matrix} Codes that are stripped from any .
#' @examples
#' preproc.strip.dot(c("M16.1", "M161"))
#' preproc.strip.dot(matrix(c("M16.1", "M162", NA, "M16.9"), ncol=2, nrow=2))
#' @export
#' @family preprocessor functions
preproc.strip.dot <- function(icd, icd_ver){
  if (is.data.frame(codes)) {codes <- as.matrix(codes)}
  return(gsub(".", "", codes, fixed=TRUE))
}