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
  output.frame <- input.frame
  # Set all columns that have 2 points
  for (var in c("PLEGIA", "DM.COMP",
                "MALIGNANCY", "RENAL"))
    output.frame[,var] <- output.frame[,var] *2
  
  # Set all columns that have 3 points
  for (var in c("SEVERE.LIVER"))
    output.frame[,var] <- output.frame[,var] *3
  
  # Set all columns that have 6 points
  for (var in c("METS", "HIV"))
    output.frame[,var] <- output.frame[,var] *6
  
  return(output.frame)
}