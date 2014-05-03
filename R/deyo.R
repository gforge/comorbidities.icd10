#' Produces a Charlson Score based on the Deyo ICD-9-CM adaptation
#' 
#' This function take as input a data frame structured such that each row
#' contains a list of ICD-9-CM codes attributed to a single patient. The
#' function goes from row to row comparing the ICD-9-CM codes a patient has
#' with the Deyo comorbidity index diagnosis codes.  If a patient has a
#' diagnosis (as indicated by ICD-9-CM code) that belongs to one of the Deyo
#' diagnoses, then the patient is considered to have this diagnosis.
#' Regardless of how many different ICD-9-CM codes a patient has corresponding
#' to a particular comorbidity category, a comorbidity is only counted once.
#' 
#' The value returned consists of a vector and two data frames. The vector is
#' the total Charlson score.  In the first data frame returned each row in the
#' data frame is devoted to a particular patient, and each column is a
#' diagnosis.  The data frame codes a 0 if the patient does not have that
#' diagnosis and 1 if the patient does have that diagnosis.  The second data
#' frame, which codes the point value of that particular diagnosis in the
#' Charlson score rather than a 1.
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
#' without decimal points.
#' @return A list of one vector and 2 data frames \item{CHARLSON.SCORE}{A
#' vector containing the Charlson score for each patient}
#' \item{COMORBIDITIES}{A data frame denoting which diagnoses each patient has
#' with a 1} \item{COMORBIDITIES.POINTS}{A data frame denoting which diagnoses
#' each patient has with the point value of that diagnosis}
#' @author Paul Gerrard, Max Gordon
#' @references Deyo RA, Cherkin DC, Ciol MA. (1992) Adapting a clinical
#' comorbidity index for use with ICD-9-CM administrative databases. J Clin
#' Epidemiol. 45(6):613-9.
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
#' deyo(x)
deyo <- function(input.frame) {
  if(is.vector(input.frame)) input.frame <- data.frame(codes=input.frame)
  ret <- pr.deyo.preprocess.icd9(input.frame)
  ret <- pr.deyo.comorbidities(ret)
  scores <- pr.deyo.convert.to.points(ret)
  
  deyo.data <- list(CHARLSON.SCORE = rowSums(scores), 
                    COMORBIDITIES = ret, 
                    COMORBIDITIES.POINTS = scores)
  return(deyo.data)
}

#' Convert icd9 codes to numeric values and convert V434X to 44390
#'  
#' @param input.frame This is a data frame with 5 character ICD-9-CM codes
#'  without decimal points. This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the same number of columns
#'  and rows as the \code{input.frame}
#' @seealso \code{\link{ahrq}}
#' 
#' @author Max
pr.deyo.preprocess.icd9 <- function(input.frame) {
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  for (i in 1:NROW(input.frame)){
    for (j in 1:NCOL(input.frame)) {
      output.frame[i,j] <- pr.deyo.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Convert to 5 digit ICD-code
#' 
#' @param icd.code \code{string} indicating the code 
#' @return \code{float} Returns a float value XXX.XX
#' @seealso \code{\link{deyo}}
pr.deyo.ICD9.5digit <- function(icd.code){
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
  icd9.3 <- substr(icd.code, 1, 3)
  icd9.4 <- substr(icd.code, 4, 4)
  icd9.5 <- substr(icd.code, 5, 5)
  if (icd9.4 == "X") {icd9.4 <- 0}
  if (icd9.5 == "X") {icd9.5 <- 0}
  icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
  if (icd9.result == "V4340") {icd9.result <- 44390}
  return(as.integer(icd9.result))
}

#' Score points for Deyo Charlson's index
#' 
#' The following function develops a matrix with 
#' rows devoted to respondents and each column a comorbidity.
#' The number in each column is the point value of having the comorbidity.
#'  
#' @param input.frame This is a data frame with 5 digit ICD-9-CM codes as XXX.XX
#'  This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the one column per 
#'  comorbidity. No comorbidity is indicated by 0 while 1 indicates 
#'  existing comorbidity.
#' @seealso \code{\link{deyo}}
pr.deyo.comorbidities <- function(input.frame) {
  output.frame <- NULL
  for (i in 1:NROW(input.frame)){
    # The rbind() may be somewhat slower than setting up the 
    # the full matrix but I think the gain from separating the 
    # the identification function from the logic clearly outweighs 
    # this cost.
    output.frame <- rbind(output.frame,
                          pr.charlson_Deyo1992_numeric(icdCode = input.frame[i, ]))
  }
  
  # You can't have both uncomplicated diabetes and
  # complicated diabetes at the same time
  output.frame[output.frame[,"dm.comp"]==TRUE, "dm"] <- 0
    
  # If a solid tumor has generated metastasis then it belongs in that group and not
  # the pure solid tumor group
  output.frame[output.frame[,"mets"]==TRUE, "malignancy"] <- 0
  
  output.frame <- as.data.frame(output.frame)
  # Change the names to upper case as in original script
  colnames(output.frame) <- toupper(colnames(output.frame))
  return(output.frame)
}

#' Score points for Deyo's comorbidity weights
#' 
#' The following function adds the weights of the Charlson
#' original weight system.
#'  
#' @param input.frame This is a data frame with 5 digit ICD-9-CM codes as XXX.XX
#'  This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the one column per 
#'  comorbidity. No comorbidity is indicated by 0 while 1 indicates 
#'  existing comorbidity.
#' @seealso \code{\link{deyo}}
pr.deyo.convert.to.points <- function(input.frame) {
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