#' Applies the comorbidity hierarchy to the comorbidity groups
#' 
#' The hierarchy functions generally remove less complicated 
#' diseases when more complicated diseases are present. For instance
#' if a metastasis code is present then the solid tumor code is obsolete.
#' 
#' @param out A \code{matrix}/\code{data.frame}/\code{vector} with the comorbidity
#'  groups outputted from the \code{cmrbdt.finder.*()} functions
#' @return \code{out} Returns the same as the input but with the hierarchy applied
#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.numeric.ahrq_2010v3.5}}
#' @export
#' @examples
#' # AHRQ example
#' out <- cmrbdt.finder.numeric.ahrq_2010v3.5("31323")
#' out[c("DM.COMP", "DM.UNCOMP")] <-TRUE
#' hierarchy.ahrq_2010v3.5(out)
#' 
hierarchy.ahrq_2010v3.5 <- function(out){
  if (is.null(dim(out))){
    # You can't have both uncomplicated diabetes and
    # complicated diabetes at the same time
    if (out["DM.COMP"]) { out["DM.UNCOMP"] <- FALSE }
    
    # If a solid tumor has generated metastasis then it belongs in that group and not
    # the pure solid tumor group
    if (out["METS"]) { out["SOLID.TUMOR"] <- FALSE}
  }else{
    # Same as for vectors but in a matrix format
    out[out[,"DM.COMP"]==TRUE,"DM.UNCOMP"] <- FALSE
    out[out[,"METS"]==TRUE,"SOLID.TUMOR"] <- FALSE
  }
  return(out)
}

#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.numeric.elixhauser_Elixhauser1998}}
#' @export
hierarchy.elixhauser_Elixhauser1998 <- function(out){
  # Just a wrapper - the hierarchy is identical
  return(hierarchy.ahrq_2010v3.5(out))
}

#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.regex.elixhauser_Quan2005}}
#' @export
hierarchy.elixhauser_Quan2005 <- function(out){
  # Just a wrapper - the hierarchy is identical
  return(hierarchy.ahrq_2010v3.5(out))
}

#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.numeric.ahrq_2010v3.5}}
#' @export
#' @examples
#' 
#' # Deyo example
#' out <- cmrbdt.finder.numeric.charlson_Deyo1992("31323")
#' out[grep("LIVER", names(out))] <-TRUE
#' hierarchy.charlson_Deyo1992(out)
hierarchy.charlson_Deyo1992 <- function(out){
  if (is.null(dim(out))){
    # You can't have both uncomplicated diabetes and
    # complicated diabetes at the same time
    if (out["DM.COMP"]) { out["DM"] <- FALSE }
    
    # You can't have both uncomplicated diabetes and
    # complicated diabetes at the same time
    if (out["SEVERE.LIVER"]) { out["MILD.LIVER"] <- FALSE }

    # If a solid tumor has generated metastasis then it belongs in that group and not
    # the pure solid tumor group
    if (out["METASTASIS"]) { out["MALIGNANCY"] <- FALSE}
  }else{
    # Same as for vectors but in a matrix format
    out[out[,"DM.COMP"]==TRUE,"DM"] <- FALSE
    out[out[,"SEVERE.LIVER"] == TRUE, "MILD.LIVER"] <- FALSE 
    out[out[,"METASTASIS"]==TRUE,"MALIGNANCY"] <- FALSE
  }
  return(out)
}

#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.regex.charlson_Quan2005}}
#' @export
hierarchy.charlson_Quan2005 <- function(out){
  # Just a wrapper - the hierarchy is identical
  return(hierarchy.charlson_Deyo1992(out))
}

#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.regex.charlson_Sundarajan2004}}
#' @export
hierarchy.charlson_Sundarajan2004 <- function(out){
  # Just a wrapper - the hierarchy is identical
  return(hierarchy.charlson_Deyo1992(out))
}


#' @rdname hierarchy
#' @seealso \code{\link{cmrbdt.finder.regex.charlson_Armitage2010}}
#' @export
#' @examples
#' 
#' # Armitage example
#' out <- cmrbdt.finder.regex.charlson_Armitage2010("I252")
#' out[grep("LIVER", names(out))] <-TRUE
#' hierarchy.charlson_Armitage2010(out)
hierarchy.charlson_Armitage2010 <- function(out){
  if (is.null(dim(out))){
    # If a solid tumor has generated metastasis then it belongs in that group and not
    # the pure solid tumor group
    if (out["METASTASIS"]) { out["CANCER"] <- FALSE}
  }else{
    out[out[,"METASTASIS"]==TRUE,"CANCER"] <- FALSE
  }
  return(out)
}

