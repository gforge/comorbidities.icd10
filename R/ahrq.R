#' Produces a comorbidity count based on the AHRQ comorbidity index v3.6, based
#' on the Elixhauser method
#' 
#' This function take as input a data frame structured such that each row
#' contains a list of ICD-9-CM codes attributed to a single patient. The
#' function goes from row to row comparing the ICD-9-CM codes a patient has
#' with the AHRQ comorbidity index diagnosis codes.  If a patient has a
#' diagnosis (as indicated by ICD-9-CM code) that belongs to one of the AHRQ
#' diagnoses, then the patient is considered to have this diagnosis.
#' Regardless of how many different ICD-9-CM codes a patient has corresponding
#' to a particular comorbidity category, a comorbidity is only counted once.
#' 
#' The value returned consists of a vector and a data frames. The vector is the
#' total comorbidity count.  In the data frame returned each row in the data
#' frame is devoted to a particular patient, and each column is a diagnosis.
#' The data frame codes a 0 if the patient does not have that diagnosis and 1
#' if the patient does have that diagnosis.
#' 
#' The ICD-9-CM codes must be 5 characters long and have no decimal poinst.If
#' any codes are less than 5 digits, then they must be led by zeroes or
#' followed by the letter "X" to create a 5 character code.  No more than 2 "X"
#' ' s can be used at the tail end, or a meaningful ICD-9-CM code cannot be
#' derived.  If your ICD-9-CM codes contain decimals or some other filler
#' character, then they must be converted to the format above for this function
#' to work properly.
#' 
#' @param input.frame This is a data frame with 5 character ICD-9-CM codes
#' without decimal points. This can also be provided as a vector or matrix.
#' @return A list of one vector and one data frames \item{COMORBIDITY.CT}{A
#' vector containing the number of comorbidtities for each patient}
#' \item{COMORBIDITIES}{A data frame denoting which diagnoses each patient has
#' with a 1}
#' @author Paul Gerrard, Max Gordon
#' @references Agency for Healthcare Research and Quality (2010) CREATION OF
#' FORMAT LIBRARY FOR COMORBIDITY GROUPS COMORBIDITY SOFTWARE, VERSION 3.6
#' Available at:
#' http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2011.txt Last
#' accessed: 10/2/11
#' @keywords package
#' @export
#' @examples
#' 
#' x <- matrix(NA, nrow = 3, ncol = 2)
#' x[1,] <- c("41000", "42800")
#' x[2,] <- c("57220", "1961X")
#' x[3,1] <- "042XX"
#' x <- as.data.frame(x)
#' ahrq(x)
ahrq <- function(input.frame) {
  warning("This function is deprecated - use cmrbdt.calc instead")
  warning("The current function has not yet been properly tested, please validate output.")
  if(is.vector(input.frame)) input.frame <- data.frame(codes=input.frame)
	ret <- pr.ahrq.points(input.frame)
  ahrq.data <- list(COMORBIDITY.CT = rowSums(ret), 
                    COMORBIDITIES = ret)
	return(ahrq.data)
}

#' Score points for AHRQ
#' 
#' The following function develops a matrix with 
#' rows devoted to respondents and each column a comorbidity.
#'  
#' @param input.frame This is a data frame with 5 digit ICD-9-CM codes as XXX.XX
#'  This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the one column per 
#'  comorbidity. No comorbidity is indicated by 0 while 1 indicates 
#'  existing comorbidity.
#' @seealso \code{\link{ahrq}}
pr.ahrq.points <- function(input.frame) {
  output.frame <- NULL
  for (i in 1:NROW(input.frame)){
    # The rbind() may be somewhat slower than setting up the 
    # the full matrix but I think the gain from separating the 
    # the identification function from the logic clearly outweighs 
    # this cost.
    output.frame <- rbind(output.frame,
                          cmrbdt.finder.numeric.ahrq_2010v3.5(icd_codes = input.frame[i, ]))
  }
    
  output.frame <- hierarchy.ahrq_2010v3.5(output.frame)
  
  output.frame <- as.data.frame(output.frame)
  
  return(output.frame)
}