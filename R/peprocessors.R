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
    attr(icd, "icd_ver") <- icd_ver
    return(icd)
  }

  icd[icd_ver ==  9] <-
    sapply(icd[icd_ver ==  9],
           USE.NAMES=FALSE,
           FUN=function(code){
             if (!grepl("^([0-9V]{1,3}$|[0-9V]{3,3}[A-Z])", code, ignore.case = TRUE))
               stop("The code '", code , "' is suppose to be",
                    " of Swedish ICD-9 format but it does not",
                    " fit the regular expression '^([0-9V]{1,3}$|[0-9V]{3,3}[A-Z])'",
                    " indicating that it does not start with a number",
                    " and or that there is no letter at the fourth position")

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
               if (letter %in% names(translator)){
                 if (nchar(code) > 5){
                   return(paste0(substr(code, 1, 3),
                                 translator[letter],
                                 substring(code, 5)))
                 }
               }

               return(paste0(substr(code, 1, 3),
                             translator[letter]))
             }

             return(code)
           })

  # Shorten to max. 4 characters as no more are needed
  icd <- ifelse(nchar(icd) > 4 &
                  icd_ver ==  9,
                substr(icd, 1, 4),
                icd)

    # Return the icd_version info as this may be used
  # for other purposes and save time
  attr(icd, "icd_ver") <- icd_ver

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
  if (is.data.frame(icd)) {icd <- as.matrix(icd)}

  icd <- gsub(".", "", icd, fixed=TRUE)
  if (!missing(icd_ver)){
    attr(icd, "icd_ver") <- icd_ver
  }
  return(icd)
}

#' Code splitter
#'
#' As the number of codes per admission frequently may differs between
#' visits and patients it can be efficient to store them in a string
#' that is separated by a " " character os something similar. In case
#' you want something other than a space you need to encapsulate the
#' function.
#'
#' @param icd The ICD-codes to be stripped
#' @param icd_ver Currently not used - this parameter is only for compatibility
#'  reasons as other preprocessing functions may require the knowledge of actual
#'  icd version.
#' @param split_str The string that is the splitter
#' @param trim If the string has trailing spaces these should usualy be removed
#'
#' @return \code{vector} A vector with only one code per entry. The attr(,"icd_ver")
#'  now contains the corresponding icd_version.
#' @examples
#' preproc.code.splitter(icd=c("M161", "M161 ", "M161 J445"),
#'                   icd_ver=c(10, 10, 10))
#' @export
#' @family preprocessor functions
#' @importFrom stringr str_trim
preproc.code.splitter <- function(icd, icd_ver,
                                  split_str = " ",
                                  trim = TRUE){
  if (trim){ icd <- str_trim(icd) }
  code_list <- strsplit(icd, split_str)
  icd <- unlist(code_list)

  if (!missing(icd_ver)){
    # Convert the icd_versions to match to the split strings
    times <- sapply(code_list, length)
    icd_ver <- rep(icd_ver, times=times)
    attr(icd, "icd_ver") <- icd_ver
  }

  return(icd)
}
