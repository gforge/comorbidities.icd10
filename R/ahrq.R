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
	ret <- prAHRQ.apply.icd9(input.frame)
  ret <- apply.convert.na(ret)
  ret <- prAHRQ.points(ret)
	POINTS <- total.points(ret)
  ahrq.data <- list(COMORBIDITY.CT = POINTS, 
                    COMORBIDITIES = ret)
	return(ahrq.data)
}

#' Convert icd9 codes to numeric values and convert v codes
#'  
#' @param input.frame This is a data frame with 5 character ICD-9-CM codes
#'  without decimal points. This can also be provided as a vector or matrix.
#' @return \code{matrix} Returns a matrix with the same number of columns
#'  and rows as the \code{input.frame}
#' @seealso\code{\link{ahrq}}
prAHRQ.apply.icd9 <- function(input.frame) { 
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      output.frame[i,j] <- prAHRQ.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Converts an icd9 code to numeric format
#'  
#' @param icd.code A string numeric code
#' @return \code{float} Returns a decimal code XXX.XX 
#' @seealso \code{\link{ahrq}}
prAHRQ.ICD9.5digit <- function(icd.code){
  # NA is converted to 0
  if (is.na(icd.code)) {
    return(0)
  }
  
  if (is.numeric(icd.code)){
    if (icd.code != ceil(icd.code))
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
  if (icd9.1 == "V") {icd9.result <- prAHRQ.process.v.codes(icd9.result)}
  
  return(as.integer(icd9.result))
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param icd.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{ahrq}}
prAHRQ.process.v.codes <- function(v.code) {
  icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
  #Valvular disease
  if (icd9.2.5 == 4220) {v.code <- 09320} 
  if (icd9.2.5 == 4330) {v.code <- 09320}
  #PVD
  if (icd9.2.5 == 4340) {v.code <- 44000} 
  #Renal Failure
  if (icd9.2.5 == 4200) {v.code <- 58530} 
  if (icd9.2.5 == 4510) {v.code <- 58530} 
  if ((icd9.2.5 >= 5600) & (icd9.2.5 <= 5632)) {v.code <- 58530}  
  if (icd9.2.5 == 5680) {v.code <- 58530} 
  if (icd9.2.5 == 4511) {v.code <- 58530}  
  if (icd9.2.5 == 4512) {v.code <- 58530}  
  #Liver Diseae
  if (icd9.2.5 == 4270) {v.code <- 07022}
  #Obsesity
  if ((icd9.2.5 >= 8530) & (icd9.2.5 <= 8539)) {v.code <- 02780}
  if ((icd9.2.5 >= 8541) & (icd9.2.5 <= 8545)) {v.code <- 02780}  			
  if (icd9.2.5 == 8554) {v.code <- 02780}
  
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
prAHRQ.points <- function(input.frame) {
  #create lists of comorbidities
  ahrq.list <- 
    list(
      chf = c(39891L,
              seq(from=42800L, to=42899L, by=1L), 
              40201L,40211L,40291L, 40401L,40411L,40491L, 40403L,40413L,40493L),
      valve = c(seq(from=9320L, to=9324L, by=1L),
                seq(from=39400L, to=39719L, by=1L),
                39790L,
                seq(from=42400L, to=42499L, by=1L),
                seq(from=74630L, to=74669L, by=1L)),
      pulm.circ = c(seq(from =41511L, to=41519L, by=1L),
                    seq(from=41600L, to=41699L, by=1L), 41790L),
      pvd = c(seq(from=44000L, to=44099L, by=1L),
              seq(from=44000L, to=44199L, by=1L),
              seq(from =44200L, to=44299L, by=1L),
              seq(from =44310L, to=44399L, by=1L),
              44421L,44122L,44710L,44900L,55710L,55790L),
      htn = c(40110L,40190L,
              seq(from =64200L, to=64204L, by=1L),
              40100L,43720L,
              seq(from =64220L, to=64224L, by=1L),
              40200L,40210L,40290L,40509L,40519L,40599L, 
              40201L,40211L,40291L, 40300L,40310L,40390L,
              40501L,40511L,40591L,
              seq(from=64210L, to=64214L, by=1L),
              40301L,40311L,40391L,40400L,40410L,40490L,
              40401L,40411L,40491L, 40402L,40412L,40492L, 
              40403L,40413L,40493L, 
              seq(from =64270L, to=64274L, by=1L),
              seq(from =64290L, to=64294L, by=1L)),
      paralysis = c(seq(from =34200L, to=34499L, by=1L),
                    seq(from=43820L, to=43853L, by=1L),
                    78072L),
      neuro.other = c(seq(from=33000L, to=33199L, by=1L),
                      33200L,33340L,33350L,33370L,33371L,33372L,
                      33379L,33385L,33394L,
                      seq(from=33400L, to=33599L, by=1L),
                      33800L,34000L,
                      seq(from=34110L, to=34199L, by=1L),
                      seq(from=34500L, to=34511L, by=1L),
                      seq(from =34520L, to=34539L, by=1L),
                      seq(from=34540L, to=34591L, by=1L),
                      34700L,34701L,34710L,34711L,
                      seq(from =64940L, to=64944L, by=1L),
                      78670L,
                      seq(from =78670L, to=78673L, by=1L),
                      78030L,78031L,78032L,78039L,78097L,78430L),
      chronic.pulm = c(seq(from=49000L, to=49289L, by=1L),
                       seq(from=49300L, to=49392L, by=1L),
                       seq(from =49400L, to=49419L, by=1L),
                       seq(from=49500L, to=50599L, by=1L),
                       50640L),
      dm.uncomp = c(seq(from=25000L,to=25033L,by=1L),
                    seq(from=64800L, to=64804L, by=1L),
                    seq(from=24900L, to=24931L, by=1L)),
      dm.comp = c(seq(from=25040L, to=25093L, by=1L),
                  77510L,
                  seq(from=24940L, to=24991L, by=1L)),
      hypothyroid = c(seq(from=24300L, to=24429L, by=1L),
                      24480L,24490L),
      renal = c(58530L,58540L,58550L,58560L,58590L, 40301L,40311L,
                40391L,40402L,40412L,40492L, 40403L,40413L,40493L),
      liver = c(7022L,7023L,7032L,7033L,7044L,7054L,45600L,45610L,45620L,
                45621L,57100L,57120L,57130L,
                seq(from=57140L, to=57149L, by=1L),
                57150L,57160L,57180L,57190L,57230L,57280L),
      pud = c(53141L,53151L,53161L,53170L,53171L,53191L,53241L,53251L,
              53261L,53270L,53271L,53291L,53341L,53351L,53361L,53370L,
              53371L,53391L,53441L,53451L,53461L,53470L,53471L,53491L),
      hiv = c(seq(from=4200L, to=4499L, by=1L)),
      lymphoma = c(seq(from=20000L,to=20238L, by=1L),
                   seq(from=20250L,to=20301L, by=1L),
                   23860L,27330L,
                   seq(from=20302L,to=20382L, by=1L)),
      mets = c(seq(from=19600L,to=19919L, by=1L),
               seq(from=20970L,to=20975L, by=1L),
               20979L,78951L),
      solid.tumor = c(seq(from=14000L,to=17299L, by=1L),
                      seq(from=17400L,to=17599L, by=1L),
                      seq(from=17900L,to=19589L, by=1L),
                      seq(from=20900L,to=20924L, by=1L),
                      seq(from=20925L,to=20939L, by=1L),
                      seq(from=20931L,to=20936L, by=1L),
                      seq(from=25801L,to=25803L, by=1L)),
      rheum = c(70100L,
                seq(from=71000L,to=71099L, by=1L),
                seq(from=71400L,to=71499L, by=1L),
                seq(from=72000L,to=72099L, by=1L),
                72500L),
      coag = c(seq(from=28600L,to=28699L, by=1L),
               28710L,
               seq(from=28730L,to=28759L, by=1L),
               seq(from=64930L,to=64934L, by=1L),
               28984L),
      obesity = c(27800L,27801L,27803L,
                  seq(from=64910L,to=64914L, by=1L),
                  79391L),
      wt.loss = c(seq(from=26000L,to=26399L, by=1L),
                  78321L,78322L),
      lytes = c(seq(from=27600L,to=27699L, by=1L)),
      anemia.loss = c(28000L,seq(from=64820L,to=64824L, by=1L)),
      anemia.def = c(seq(from=28010L,to=28199L, by=1L),
                     seq(from=28521L,to=28529L, by=1L),
                     28590L),
      etoh = c(seq(from=29100L,to=29139L, by=1L),
               29150L,29180L,29181L,29182L,29189L,29190L,
               seq(from=30300L,to=30393L, by=1L),
               seq(from=30500L,to=30503L, by=1L)),
      drugs = c(29200L,
                seq(from=29282L,to=29289L, by=1L),
                29290L,
                seq(from=30400L,to=30493L, by=1L),
                seq(from=30520L,to=30593L, by=1L),
                seq(from=64830L,to=64834L, by=1L)),
      psychoses = c(seq(from=29500L,to=29899L, by=1L),
                    29910L,29911L),
      depression = c(30040L,30112L,30900L,30910L,31100L))
  
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=29)
  # Using the names for columns limits the risk of mixing groups 
  colnames(output.frame) <- names(ahrq.list)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      for (k in name(ahrq.list)){
        if (input.frame[i, j] %in% ahrq.list[[k]]) {
          output.frame[i,k] <- 1
        }
      }
    }
  }
  
  #Apply the elixhauser hierarchy
  # You can't have both uncomplicated diabetes and
  # complicated diabetes at the same time
  output.frame[output.frame[,"dm.comp"]==1,"dm.uncomp"] <- 0
    
  # If a solid tumor has generated metastasis then it belongs in that group and not
  # the pure solid tumor group
  output.frame[output.frame[,"mets"]==1,"solid.tumor"] <- 0
  
  output.frame <- as.data.frame(output.frame)
  
  # Change the names to upper case as in original script
  colnames(output.frame) <- toupper(colnames(output.frame))
  
  return(output.frame)
}

