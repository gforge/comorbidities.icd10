#' Score points for Charlson's original comorbidity weights
#' 
#' The following function adds the weights of the Charlson
#' original weighting system where comorbidities were score
#' between 1 and 6.
#'  
#' @param input.frame This is a \code{data.frame} or \code{matrix} with
#'  the comorbidity groups as returned by any of the Charlson functions
#' @return Weighted comorbidities
#' @examples
#' out <- cmrbdt.finder.regex.charlson_Quan2005("I252")
#' weight.Charlsons.org(out)
#' @export
weight.Charlsons.org <- function(input.frame) {
  output.frame <- input.frame*1
  # Set all columns that have 2 points
  for (var in c("PLEGIA", "DM.COMP",
                "MALIGNANCY", "RENAL")){
    multiplier <- 2
    if (is.null(dim(output.frame))){
      output.frame[var] <- output.frame[var] * multiplier
    }else{
      output.frame[,var] <- output.frame[,var] * multiplier
    }
  }
  
  # Set all columns that have 3 points
  for (var in c("SEVERE.LIVER")){
    multiplier <- 3
    if (is.null(dim(output.frame))){
      output.frame[var] <- output.frame[var] * multiplier
    }else{
      output.frame[,var] <- output.frame[,var] * multiplier
    }
  }
  
  # Set all columns that have 6 points
  for (var in c("METASTASIS", "HIV")){
    multiplier <- 6
    if (is.null(dim(output.frame))){
      output.frame[var] <- output.frame[var] * multiplier
    }else{
      output.frame[,var] <- output.frame[,var] * multiplier
    }
  }
  
  return(output.frame)
}