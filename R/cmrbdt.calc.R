#' The main comorbidity calculator function
#' 
#' This function is the main comorbidity calculator.
#' You should try to use this function whenever you
#' want to get a full comorbidity count. By providing
#' the individual functions you can easily switch code identifier,
#' weights, and code pre-processing.
#' 
#' The function returns a list with:
#' \itemize{
#'  \item{score/ct} If you have provided a weight function you
#'   will have a score item, otherwise it is just a simple count
#'   of how many comorbidity groups that have been identified. The
#'   name of the item is either \code{"score"} or \code{"ct"} in order
#'   to avoid mistakes.
#'  \item{cmrbdt} This is a matrix with comorbidity TRUE/FALSE for
#'   each group of the comorbidity by \code{id} as returned by \code{\link[plyr]{ddply}}.
#'  \item{cmrbdt.weighted} If you have provided a comorbidity weighting
#'   function then this will also be included in the returned list. This
#'   matrix is also by \code{id} as returned by \code{\link[plyr]{ddply}}.
#' }
#' 
#' @param ds The data.frame/matrix/vector that is to be analyzed for
#'  matching icd-codes.
#' @param id_column The id of the \code{ds} parameter. If included in the
#'  ds then provide only column names, otherwise this should be in the format
#'  of a vector/matrix/data.frame matching the size of the \code{ds} input. You
#'  can have multiple columns as ID-parameters.
#' @param icd_columns If the \code{ds} contains more than just the ICD-code columns
#'  then you need to specify the ICD-columns, either by name or numbers.
#' @param icd_ver_column The ICD-version number if you don't want auto-detect. This can
#'  either be a single number if they are all of the same ICD-version or you can have a
#'  column in the \code{ds} that signals the version, alternatively this can be a vector
#'  of the same length as the \code{ds}.
#'  As auto-detect may fail try to specify this if you can. For those that you are
#'  uncertain you can simple set the value to \code{FALSE} and the software will attempt
#'  to autodetect those specific instances.
#' @param include_acute_column Certain codes may indicate a non-chronic
#'  disease such as heart infarction, stroke or similar. Under some
#'  circumstances these should be ignored, e.g. when studying predictors
#'  for hip arthroplasty re-operations codes during the admission
#'  for the surgery should not include myocardial infarction as this
#'  is most likely a postoperative condition not available for scoring
#'  prior to the surgery.
#' @param icd_code_preprocess_fn Sometimes the codes need to be 
#'  pre-processed prior to feeding them into the algorithm. For instance
#'  the ICD-columns may be crammed into one single column where each 
#'  code is separated by a ' '. When this is the case the pre-processing
#'  allows a split prior to calling the \code{codefinder_fn}, e.g. splitting
#'  'M161 E110' could need a function as \code{function(code){unlist(strsplit(code, " "))}}
#'  - \emph{note} the unlist, your function should return a vector and not a list.
#' @param codefinder_fn This is one of the codefinder functions that you want
#'  to apply. The codefinder is at the heart of the algorithm and does the 
#'  actual comorbidity identidication. See below for a list of available functions.
#' @param codefinder_hierarchy_fn This functions applies any hierarchy needed in order
#'  to retain logic, e.g. complicated diabetes and uncomplicated diabetes should not
#'  co-exist in one and the same patient. You can provide here any of the \code{hierarchy.*()}
#'  functions. E.g. if you are using Elixhausers Quan 2005 version you provide the 
#'  function \code{\link{hierarchy.elixhauser_Quan2005}}.
#' @param cmrbdt_weight_fn The comorbidity weight function that you want to apply 
#'  to the current calculation. E.g. you can use the \code{\link{weight.Charlsons.org}}
#'  if you want to apply the traditional Charlson comorbidity score or you can write 
#'  your own function.
#' @param country_code The two-letter \code{ISO 3166-1 alpha-2}
#'  code indicating the country of interest (the same as the top-level
#'  internet domain name of that country). As certain countries
#'  have adapted country-specific ICD-coding there may be minor 
#'  differences between countries. Currently only Swedish (SE) and
#'  US codes are implemented. The function defaults to 'US'.
#' @seealso 
#' \itemize{
#'  \item{\code{\link{codefinder.numeric.ahrq_2010v3.5}}: }{Numeric funciton for 
#'   identifying AHRQ codes, \emph{note} that this is not the latest version! Works only 
#'   with ICD-9 codes.}
#'  \item{\code{\link{codefinder.numeric.elixhauser_Elixhauser1998}}: }{Numeric function
#'   for identifying the original Elixhauser codes from 1998, \emph{note} that newer versions
#'   code versions are available. Works only with ICD-9 codes.}
#'  \item{\code{\link{codefinder.numeric.charlson_Deyo1992}}: }{Numeric function for
#'   identifying Deyo's original translation of the Elixhauser comorbidity groups. Works only 
#'   with ICD-9 codes.}
#'  \item{\code{\link{codefinder.regex.charlson_Sundarajan2004}}: }{A function based
#'   on regular expressions for identifying Sundarajan's codeset for Charlsons index, \emph{note} 
#'   that the Quan article that they wrote one year later is an update to the current code
#'   set.}
#'  \item{\code{\link{codefinder.regex.charlson_Quan2005}}: }{A function based
#'   on regular expressions for identifying Quan's codeset for Charlsons index. This
#'   is currently (written 2014-05-07) the most up-to-date version of the Charlson code set unless
#'   the Royal College of Surgeons attempt at changing the Charlson counts.}
#'  \item{\code{\link{codefinder.regex.charlson_Armitage2010}}: }{A function based
#'   on regular expressions for identifying an adaptation and simplification of the
#'   Charlsons index. \emph{Note} that this is no longer the Charlsons but an adaptation with
#'   only 14 comorbidity groups.}
#'  \item{\code{\link{codefinder.regex.elixhauser_Quan2005}}: }{A function based
#'   on regular expressions for identifying Quan's codeset for Charlsons index. This
#'   is currently (written 2014-05-07) the most up-to-date version of the Elixhauser code set unless
#'   the AHRQ is included although the AHRQ has never been updated to ICD-10.}
#' }
#' @importFrom plyr ddply
#' @export
#' @example inst/examples/cmrbdt.calc_xmpl.R
cmrbdt.calc <- function(ds, 
                        id_column,
                        icd_columns,
                        icd_ver_column,
                        include_acute_column,
                        icd_code_preprocess_fn,
                        codefinder_fn,
                        codefinder_hierarchy_fn,
                        cmrbdt_weight_fn,
                        country_code = 'US'){
  # Extract only what we need from the data set
  if (!missing(icd_columns)){
    icd_codes <- ds[,icd_columns, drop=FALSE]
  }else{
    icd_codes <- ds
  }
  icd_codes <- as.data.frame(icd_codes)
  colnames(icd_codes) <- sprintf("ICD_codes_no_%d",
                                 1:NCOL(icd_codes))

  # Get the ID-column if any has been provided
  if (missing(id_column)){
    # Skips the ID, probably not entirely optimal for the function's 
    # performance but it is not really intended for use without the ID
    id_column <- 1:NROW(ds)
  }else{
    org_id_names <- id_column
    if (length(id_column) == 1){
      id_column <- ds[,id_column, drop=FALSE]
      if (is.numeric(id_column)) {org_id_names <- colnames(ds)[id_column]}
    }else if (NROW(id_column) !=  NROW(ds)){
      if (is.numeric(id_column) &&
             all(id_column %in% 1:NCOL(ds))){
        id_column <- ds[, id_column, drop=FALSE]
        org_id_names <- colnames(org_id_names)[id_column]
      } else if (is.character(id_column) &&
           all(id_column %in% colnames(ds))){
        id_column <- ds[, id_column, drop=FALSE]
      } else {
        stop("You have provided ID columns (",
             paste(id_column, collapse=", "),
             ,") that the software fails to identify among the provided ds columns")
      }
    }else{
      if (is.null(colnames(id_column))){
        if (NCOL(id_column) == 1){
          org_id_names <- "ID"
        }else{
          org_id_names <- sprintf("ID_no_%d",
                                  1:NCOL(id_column))
        }
      }else{
        org_id_names <- colnames(id_column)
      }
    }
  }
  id_column <- as.data.frame(id_column)
  colnames(id_column) <- sprintf("ID_no_%d",
                                 1:NCOL(id_column))

  if (missing(icd_ver_column)){
    # Set auto-detect on all
    icd_ver_column <- rep(FALSE, times=NROW(ds))
  }else if (length(icd_ver_column) == 1){
    if (icd_ver_column %in% colnames(ds) ||
          icd_ver_column %in% 1:NCOL(ds)){
      icd_ver_column <- ds[,icd_ver_column, drop=FALSE]
    }else{
      stop("You have provided an ICD version column identifyer (", icd_ver_column, ")",
           " that is neither a column name in the ds provided dataset",
           " or a numerical representation of that column")
    }
  }else if (length(icd_ver_column) != NROW(ds)){
    stop("You have provided ICD-versions for the ds dataset",
         " unfortunately the length don't match. You have provided",
         " '", length(icd_ver_column), "' icd version indicators",
         " while the ds dataset has '", NROW(ds), "' rows.")
  }
  icd_ver_column <- as.matrix(icd_ver_column, ncol=1)
  
  if (missing(include_acute_column)){
    include_acute_column <- rep(TRUE, times=NROW(ds))
  }else if (length(include_acute_column) == 1){
    if (include_acute_column %in% colnames(ds) ||
          include_acute_column %in% 1:NCOL(ds)){
      include_acute_column <- ds[,include_acute_column, drop=FALSE]
    }else{
      stop("You have provided an ICD acute column identifyer (", include_acute_column, ")",
           " that is neither a column name in the ds provided dataset",
           " or a numerical representation of that column")
    }
  }else if (length(include_acute_column) != NROW(ds)){
    stop("You have provided ICD-acute column for the ds dataset",
         " unfortunately the length don't match. You have provided",
         " '", length(include_acute_column), "' icd acute indicators",
         " while the ds dataset has '", NROW(ds), "' rows.")
  }
  include_acute_column <- as.matrix(include_acute_column, ncol=1)
  
  # Get the codefinder function
  if (is.character(codefinder_fn)){
    if (!exists(codefinder_fn))
      stop("Could not identify the codefinder function that you want to use,",
           " i.e. the function '", codefinder_fn, "' is not defined")
    codefinder_fn <- get(codefinder_fn)
  }
  
  if (!missing(codefinder_hierarchy_fn) &&
        is.character(codefinder_hierarchy_fn)){
    if (!exists(codefinder_hierarchy_fn))
      stop("Could not identify the codefinder_hierarchy_fn function that you want to use,",
           " i.e. the function '", codefinder_hierarchy_fn, "' is not defined")
    codefinder_hierarchy_fn <- get(codefinder_hierarchy_fn)
  }else{
    codefinder_hierarchy_fn <- NULL
  }

  if (!missing(icd_code_preprocess_fn) &&
        is.character(icd_code_preprocess_fn)){
    if (!exists(icd_code_preprocess_fn))
      stop("Could not identify the icd_code_preprocess_fn function that you want to use,",
           " i.e. the function '", icd_code_preprocess_fn, "' is not defined")
    icd_code_preprocess_fn <- get(icd_code_preprocess_fn)
  }else{
    icd_code_preprocess_fn <- NULL
  }

  if (!missing(cmrbdt_weight_fn) &&
        is.character(cmrbdt_weight_fn)){
    if (!exists(cmrbdt_weight_fn))
      stop("Could not identify the cmrbdt_weight_fn function that you want to use,",
           " i.e. the function '", cmrbdt_weight_fn, "' is not defined")
    cmrbdt_weight_fn <- get(cmrbdt_weight_fn)
  }
  
  # Merge the different sections into one
  # in order to get the ddply to work
  d2a <- cbind(id_column,
               icd_codes)
  d2a$icd_ver <- icd_ver_column
  d2a$include_acute <- include_acute_column
  ret <- 
    list(cmrbdt =
           ddply(d2a, 
                 colnames(id_column),
                 codefinder_fn = codefinder_fn,
                 codefinder_hierarchy_fn = codefinder_hierarchy_fn,
                 icd_code_preprocess_fn = icd_code_preprocess_fn,
                 icd_cols = NCOL(id_column) + 1:NCOL(icd_codes),
                 .fun=function(x,
                          icd_cols, 
                          codefinder_fn,
                          codefinder_hierarchy_fn,
                          icd_code_preprocess_fn){
                   out <- NULL
                   for (i in 1:nrow(x)){
                     codes <- x[i, icd_cols]
                     if (!is.null(icd_code_preprocess_fn)){
                       codes <- icd_code_preprocess_fn(codes)
                     }
                     out <- codefinder_fn(icd_codes=codes,
                                          out=out,
                                          include_acute=rep(x[i, "include_acute"], times=length(codes)),
                                          icd_ver=rep(x[i, "icd_ver"], times=length(codes)))
                   }
                   
                   if (!is.null(codefinder_hierarchy_fn)){
                     out <- codefinder_hierarchy_fn(out)
                   }
                   
                   return(out)
                 }))
  
  # Apply the weight or just simply count the number of comorbidities
  id_col_nos <- c(1:NCOL(id_column))
  if (!missing(cmrbdt_weight_fn)){
    ret[["cmrbdt.weighted"]] <- 
      t(apply(ret[["cmrbdt"]], 1, 
              FUN=function(x) {
                return(c(x[id_col_nos],
                         cmrbdt_weight_fn(x[-id_col_nos])))
              }))
    ret[["score"]] <- rowSums(ret[["cmrbdt.weighted"]][,-id_col_nos])
  }else{
    ret[["ct"]] <- as.integer(rowSums(ret[["cmrbdt"]][,-id_col_nos]))
  }
  
  return(ret)
}
