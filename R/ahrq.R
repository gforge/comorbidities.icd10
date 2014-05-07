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
#' x <- matrix(0, nrow = 3, ncol = 2)
#' x[1,1] <- "41000"
#' x[1,2] <- "42800"
#' x[2,1] <- "57220"
#' x[2,2] <- "1961X"
#' x[3,1] <- "042XX"
#' x <- as.data.frame(x)
#' ahrq(x)
ahrq <- function(input.frame) {
  warning("The current function has not yet been properly tested, please validate output.")
  if(is.vector(input.frame)) input.frame <- data.frame(codes=input.frame)
	ret <- pr.ahrq.preprocess.icd9(input.frame)
  ret <- pr.ahrq.points(ret)
  ahrq.data <- list(COMORBIDITY.CT = rowSums(ret), 
                    COMORBIDITIES = ret)
	return(ahrq.data)
}

#' Convert icd9 codes to numeric values
#'  
#' @param input.frame This is a data frame with 5 character ICD-9-CM codes
#'  without decimal points. This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the same number of columns
#'  and rows as the \code{input.frame}
#' @seealso\code{\link{ahrq}}
pr.ahrq.preprocess.icd9 <- function(input.frame) { 
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  # TODO: this loop is probably rather slow
  for (i in 1:NROW(input.frame)){
    for (j in 1:NCOL(input.frame)) {
      output.frame[i,j] <- pr.ahrq.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Converts an icd9 code to numeric format
#'  
#' @param icd.code A string numeric code
#' @return \code{float} Returns a decimal code XXX.XX 
#' @seealso \code{\link{ahrq}}
pr.ahrq.ICD9.5digit <- function(icd.code){
  if (is.na(icd.code)) {
    return(NA)
  }
  
  if (is.numeric(icd.code)){
    if (icd.code != floor(icd.code))
      stop("The software wants icd.codes provided in numeric format without",
           " decimals if ICD-9 is provided in numeric format, otherwise it does",
           " no know how to deal with the code. The code should be in the format ",
           " 58510 and not 585.1 or anything similar. You have provided the code: ", icd.code)
    return(icd.code)
  }
  
  if (class(icd.code) == "factor")
    icd.code <- as.character(icd.code)
  
  if (nchar(icd.code) < 5)
    icd.code <- paste0(icd.code, paste0(rep("0", length.out=5-nchar(icd.code)),
                                        collapse=""))
  
  icd.code = toupper(icd.code)
  icd9.1 <- substr(icd.code, 1, 1)
  icd9.3 <- substr(icd.code, 1, 3)
  icd9.4 <- substr(icd.code, 4, 4)
  icd9.5 <- substr(icd.code, 5, 5)
  if (icd9.4 == "X") {icd9.4 <- 0}
  if (icd9.5 == "X") {icd9.5 <- 0}
  icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
  if (icd9.1 == "V") {icd9.result <- pr.ahrq.preprocess.v.codes(icd9.result)}
  
  return(as.integer(icd9.result))
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param v.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{ahrq}}
pr.ahrq.preprocess.v.codes <- function(v.code) {
  if (any(nchar(nchar(v.code) < 5))){
    for (i in which(nchar(v.code) < 5)){
      v.code[i] <- paste0(v.code[i], paste(rep(0, times=5-nchar(v.code[i]))))
      
    }
  }
  v.code <- 
    sapply(v.code, USE.NAMES=FALSE,
           FUN=function(code){
             icd9.2.5 <- as.numeric(substr(code, 2, 5))
             #Valvular disease
             if (icd9.2.5 == 4220) {return(09320L)} 
             if (icd9.2.5 == 4330) {return(09320L)}
             #PVD
             if (icd9.2.5 == 4340) {return(44000L)} 
             #Renal Failure
             if (icd9.2.5 == 4200) {return(58530L)} 
             if (icd9.2.5 == 4510) {return(58530L)} 
             if ((icd9.2.5 >= 5600) & (icd9.2.5 <= 5632)) {return(58530L)}  
             if (icd9.2.5 == 5680) {return(58530L)} 
             if (icd9.2.5 == 4511) {return(58530L)}  
             if (icd9.2.5 == 4512) {return(58530L)}  
             #Liver Diseae
             if (icd9.2.5 == 4270) {return(07022L)}
             #Obsesity
             if ((icd9.2.5 >= 8530) & (icd9.2.5 <= 8539)) {return(02780L)}
             if ((icd9.2.5 >= 8541) & (icd9.2.5 <= 8545)) {return(02780L)}  			
             if (icd9.2.5 == 8554) {return(02780L)}
             return(code)
           })
             
  return (v.code)
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
#' @seealso\code{\link{ahrq}}
pr.ahrq.points <- function(input.frame) {
  output.frame <- NULL
  for (i in 1:NROW(input.frame)){
    # The rbind() may be somewhat slower than setting up the 
    # the full matrix but I think the gain from separating the 
    # the identification function from the logic clearly outweighs 
    # this cost.
    output.frame <- rbind(output.frame,
                          codefinder.numeric.ahrq_2010v3.5(icd_codes = input.frame[i, ]))
  }
    
  output.frame <- hierarchy.ahrq_2010v3.5(output.frame)
  
  output.frame <- as.data.frame(output.frame)
  
  return(output.frame)
}