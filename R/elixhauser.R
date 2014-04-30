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
  # Convert icd9 codes to numeric values and convert v codes
  ret <- prElixhauser.apply.icd9(input.frame)
  ret <- apply.convert.na(ret)
  ret <- points.elixhauser.30(ret)
  elixhauser.data <- list(COMORBIDITY.CT = rowSums(ret), 
                          COMORBIDITIES = ret)
  return(elixhauser.data)
}

prElixhauser.apply.icd9 <- function(input.frame) { 
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  for (i in 1:NROW(input.frame)){
    for (j in 1:NCOL(input.frame)) {
      output.frame[i,j] <- prElixhauser.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Convert to 5 digit ICD-code
#' 
#' @param icd.code \code{string} indicating the code 
#' @return \code{float} Returns a float value XXX.XX
#' @seealso \code{\link{elixhauser}}
prElixhauser.ICD9.5digit <- function(icd.code){ 
  # NA is converted to 0
  if (is.na(icd.code)) {
    return(0)
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
  if (icd9.1 == "V") {icd9.result <- prElixhauser.process.v.codes(icd9.result)}
  
  return(as.integer(icd9.result))
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param icd.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{elixhauser}}
prElixhauser.process.v.codes <- function(v.code) {
  icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
  if (icd9.2.5 == 4500L) {v.code <- 42610L}
  if (icd9.2.5 == 5330L) {v.code <- 42610L}
  if (icd9.2.5 == 4220L) {v.code <- 09320L}
  if (icd9.2.5 == 4330L) {v.code <- 09320L}
  if (icd9.2.5 == 4340L) {v.code <- 44000L}
  if (icd9.2.5 == 4200L) {v.code <- 40311L}
  if (icd9.2.5 == 4510L) {v.code <- 40311L}
  if (icd9.2.5 == 5600L) {v.code <- 40311L}
  if (icd9.2.5 == 5680L) {v.code <- 40311L}
  if (icd9.2.5 == 4270L) {v.code <- 07032L}
  if (icd9.2.5 == 1271L) {v.code <- 53170L}
  if ((icd9.2.5 >= 1000L) & (icd9.2.5 <= 1090L)) {v.code <- 14000L}
  if (icd9.2.5 == 1071L) {v.code <- 20000L}
  if (icd9.2.5 == 1072L) {v.code <- 20000L}
  if (icd9.2.5 == 1079L) {v.code <- 20000L}
  if (icd9.2.5 == 1130L) {v.code <- 29110L}
  
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
points.elixhauser.30 <- function(input.frame) {
  #create lists of comorbidities
  elixhauser.list <-
    list(chf = c(39891L,40211L,40291L,40411L,40413L,40491L,40493L,
                 seq(from=42800L, to=42899L, by=1L)),
         arrhythmia = c(42610L,42611L,42613L,
                        seq(from=42620L, to=42653L, by=1L),
                        seq(from=42660L, to=42689L, by=1L),
                        42700L,42720L,42731L,42760L,42790L,78500L),
         valve = c(seq(from=9320L, to=9324L, by=1L),
                   seq(from=39400L, to=39719L, by=1L),
                   seq(from=42400L, to=42491L, by=1L),
                   seq(from=74630L, to=74669L, by=1L)),
         pulm.circ = c(seq(from=41600L, to=41690L, by=1L), 
                       41790L),
         pvd = c(seq(from=44000L, to=44099L, by=1L),
                 44120L,44140L,44170L,44190L,
                 seq(from=44310L, to=44399L, by=1L),
                 44710L,55710L,55790L),
         htn = c(40110L,40190L),
         htn.comp = c(40210L,40290L, # Separated hypertension into complicated and un-complicated
                      40410L,40490L,
                      40511L,40519L,
                      40591L,40599L),
         paralysis = c(seq(from =34200L, to=34212L, by=1L),
                       seq(from=34290L, to=34499L, by=1L)),
         neuro.other = c(33190L,33200L,33340L,33350L,
                         seq(from=33400L, to=33599L, by=1L),
                         34000L,
                         seq(from=34110L, to=34199L, by=1L),
                         seq(from=34500L, to=34511L, by=1L),
                         seq(from=34540L, to=34551L, by=1L),
                         seq(from=34580L, to=34591L, by=1L),
                         34810L,34830L,78030L,78430L),
         chronic.pulm = c(seq(from=49000L, to=49289L, by=1L),
                          seq(from=49300L, to=49391L, by=1L),
                          49400L,
                          seq(from=49500L, to=50599L, by=1L),
                          50640L),
         dm.uncomp = c(seq(from=25000L,to=25033L,by=1L)),
         dm.comp = c(seq(from=25040L, to=25073L, by=1L),
                     seq(from=25090L, to=25093L, by=1L)),
         hypothyroid = c(seq(from=24300L, to=24429L, by=1L),
                         24480L,24490L),
         renal = c(40311L,40391L,40412L,40492L,58500L,58600L),
         liver = c(7032L,7033L,7054L,45600L,45610L,
                   45620L,45621L,57100L,57120L,57130L,
                   seq(from=57140L, to=57149L, by=1L),
                   57150L,57160L,57180L,57190L,57230L,57280L),
         pud = c(53170L,53190L,53270L,53290L,53370L,53390L,53470L,53490L),
         hiv = c(seq(from=4200L, to=4499L, by=1L)),
         lymphoma = c(seq(from=20000L,to=20238L, by=1L),
                      seq(from=20250L,to=20301L, by=1L),
                      seq(from=20380L,to=20381L, by=1L),
                      23860L,27330L),
         mets = c(seq(from=19600L,to=19919L, by=1L)),
         solid.tumor = c(seq(from=14000L,to=17299L, by=1L),
                         seq(from=17400L,to=17599L, by=1L),
                         seq(from=17900L,to=19589L, by=1L)),
         rheum = c(70100L,
                   seq(from=71000L,to=71099L, by=1L),
                   seq(from=71400L,to=71499L, by=1L),
                   seq(from=72000L,to=72099L, by=1L),
                   72500L),
         coag = c(seq(from=28600L,to=28699L, by=1L),
                  28710L,
                  seq(from=28730L,to=28759L, by=1L)),
         obesity = c(27800L),
         wt.loss = c(seq(from=26000L,to=26399L, by=1L)),
         lytes = c(seq(from=27600L,to=27699L, by=1L)),
         anemia.loss = c(28000L),
         anemia.def = c(seq(from=28010L,to=28199L, by=1L),
                        28590L),
         etoh = c(29110L,29120L,29150L,29180L,29190L,
                  seq(from=30390L,to=30393L, by=1L),
                  seq(from=30500L,to=30503L, by=1L)),
         drugs = c(29200L,
                   seq(from=29282L,to=29289L, by=1L),
                   29290L,
                   seq(from=30400L,to=30493L, by=1L),
                   seq(from=30520L,to=30593L, by=1L)),
         psychoses = c(seq(from=29500L,to=29899L, by=1L),
                       seq(from=29910L,to=29911L, by=1L)),
         depression = c(30040L,30112L,30900L,30910L,31100L))
  
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=length(elixhauser.list))
  # Using the names for columns limits the risk of missing 
  colnames(output.frame) <- names(elixhauser.list)
  for (i in 1:NROW(input.frame)){
    for (j in 1:NCOL(input.frame)) {
      for (disease_group in names(elixhauser.list)){
        if (input.frame[i, j] %in% elixhauser.list[[disease_group]]) {
          output.frame[i,disease_group] <- 1
        }
      }
    }
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
