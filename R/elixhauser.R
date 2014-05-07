#' Produces an Elixhauser comorbidity count
#' 
#' This function take as input a data frame structured such that each row
#' contains a list of ICD-9-CM codes attributed to a single patient. The
#' function goes from row to row comparing the ICD-9-CM codes a patient has
#' with the Elixhauser comorbidity index diagnosis codes.  If a patient has a
#' diagnosis (as indicated by ICD-9-CM code) that belongs to one of the
#' Elixhauser diagnoses, then the patient is considered to have this diagnosis.
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
#' without decimal points
#' @return A list of one vector and one data frames \item{COMORBIDITY.CT}{A
#' vector containing the number of comorbidtities for each patient}
#' \item{COMORBIDITIES}{A data frame denoting which diagnoses each patient has
#' with a 1}
#' @author Paul Gerrard, Max Gordon
#' @references Elixhauser A, Steiner C, Harris DR, Coffey RM. (1998)
#' Comorbidity measures for use with administrative data.  Med Care. 36:8-27.
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
#' elixhauser(x)
elixhauser <- function(input.frame) {
  if(is.vector(input.frame)) input.frame <- data.frame(codes=input.frame)
  # Convert icd9 codes to numeric values and convert v codes
  ret <- pr.elixhauser.preprocess.icd9(input.frame)
  ret <- pr.elixhauser.points.30(ret)
  elixhauser.data <- list(COMORBIDITY.CT = rowSums(ret), 
                          COMORBIDITIES = ret)
  return(elixhauser.data)
}

pr.elixhauser.preprocess.icd9 <- function(input.frame) { 
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  for (i in 1:NROW(input.frame)){
    for (j in 1:NCOL(input.frame)) {
      output.frame[i,j] <- pr.elixhauser.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Convert to 5 digit ICD-code
#' 
#' @param icd.code \code{string} indicating the code 
#' @return \code{float} Returns a float value XXX.XX
#' @seealso \code{\link{elixhauser}}
pr.elixhauser.ICD9.5digit <- function(icd.code){ 
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
  if (icd9.1 == "V") {icd9.result <- pr.elixhauser.process.v.codes(icd9.result)}
  
  return(as.integer(icd9.result))
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param v.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{elixhauser}}
pr.elixhauser.process.v.codes <- function(v.code) {
  if (class(v.code) == "factor") 
    v.code <- as.character(v.code)
  
  if (any(nchar(nchar(v.code) < 5))){
    for (i in which(nchar(v.code) < 5)){
      v.code[i] <- paste0(v.code[i], paste(rep(0, times=5-nchar(v.code[i]))))
      
    }
  }
  v.code <- 
    sapply(v.code, USE.NAMES=FALSE,
           FUN=function(code){
      icd9.2.5 <- as.integer(substr(code, 2, 5))
      if (icd9.2.5 == 4500L) {return(42610L)}
      if (icd9.2.5 == 5330L) {return(42610L)}
      if (icd9.2.5 == 4220L) {return(09320L)}
      if (icd9.2.5 == 4330L) {return(09320L)}
      if (icd9.2.5 == 4340L) {return(44000L)}
      if (icd9.2.5 == 4200L) {return(40311L)}
      if (icd9.2.5 == 4510L) {return(40311L)}
      if (icd9.2.5 == 5600L) {return(40311L)}
      if (icd9.2.5 == 5680L) {return(40311L)}
      if (icd9.2.5 == 4270L) {return(07032L)}
      if (icd9.2.5 == 1271L) {return(53170L)}
      if ((icd9.2.5 >= 1000L) & (icd9.2.5 <= 1090L)) {return(14000L)}
      if (icd9.2.5 == 1071L) {return(20000L)}
      if (icd9.2.5 == 1072L) {return(20000L)}
      if (icd9.2.5 == 1079L) {return(20000L)}
      if (icd9.2.5 == 1130L) {return(29110L)}
      return(code)
    })
  
  return (v.code)
}

#' Score points for Elixhauser's comorbidity count
#' 
#' The following function develops a matrix with 
#' rows devoted to respondents and each column a comorbidity.
#'  
#' @param input.frame This is a data frame with 5 digit ICD-9-CM codes as XXX.XX
#'  This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the one column per 
#'  comorbidity. No comorbidity is indicated by 0 while 1 indicates 
#'  existing comorbidity.
#' @seealso \code{\link{elixhauser}}
pr.elixhauser.points.30 <- function(input.frame) {
  output.frame <- NULL
  for (i in 1:NROW(input.frame)){
    # The rbind() may be somewhat slower than setting up the 
    # the full matrix but I think the gain from separating the 
    # the identification function from the logic clearly outweighs 
    # this cost.
    output.frame <- rbind(output.frame,
                          codefinder.numeric.elixhauser_Elixhauser1998(icd_codes = input.frame[i, ]))
  }
  
  # You can't have both uncomplicated diabetes and
  # complicated diabetes at the same time
  output.frame[output.frame[,"dm.comp"]==1,"dm.uncomp"] <- 0
    
  # If a solid tumor has generated metastasis then it belongs in that group and not
  # the pure solid tumor group
  output.frame[output.frame[,"mets"]==1, "solid.tumor"] <- 0
  
  output.frame <- as.data.frame(output.frame)
  # Change the names to upper case as in original script
  colnames(output.frame) <- toupper(colnames(output.frame))
  
  return(output.frame)
}
