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
  interim.frame.1 <- prElixhauser.apply.icd9(input.frame)
  interim.frame.2 <- apply.convert.na(interim.frame.1)
  interim.frame.3 <- points.elixhauser.30(interim.frame.2)
  POINTS <- total.points(interim.frame.3)
  elixhauser.data <- list(POINTS, interim.frame.3)
  names(elixhauser.data) <- c("COMORBIDITY.CT", "COMORBIDITIES")
  return(elixhauser.data)
}

prElixhauser.apply.icd9 <- function(input.frame) { 
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      output.frame[i,j] <- prElixhauser.ICD9.5digit(input.frame[i,j])
    }
  }
  return(output.frame)
}

#' Convert to 5 digit ICD-code
#' 
#' @param icd.code \code{string} indicating the code 
#' @return \code{float} Returns a float value XXX.XX
#' @seealso \code{\link{elixhauser}
prElixhauser.ICD9.5digit <- function(icd.code){ 
  # NA is converted to 0
  if (is.na(icd.code)) {
    return(0)
  }
  
  if (is.numeric(icd.code)){
    if (icd.code > 10^4)
      return(icd.code/100)
    return(icd.code)
  }
  
  icd9.1 <- substr(icd.code, 1, 1)
  icd9.3 <- substr(icd.code, 1, 3)
  icd9.4 <- substr(icd.code, 4, 4)
  icd9.5 <- substr(icd.code, 5, 5)
  if (icd9.4 == "X") {icd9.4 <- 0}
  if (icd9.5 == "X") {icd9.5 <- 0}
  icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
  if (icd9.1 == "V") {icd9.result <- prElixhauser.process.v.codes(icd9.result)}
  
  return(as.numeric(icd9.result)/100)
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param icd.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{elixhauser}}
prElixhauser.process.v.codes <- function(v.code) {
  icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
  if (icd9.2.5 == 4500) {v.code <- 42610}
  if (icd9.2.5 == 5330) {v.code <- 42610}
  if (icd9.2.5 == 4220) {v.code <- 09320}
  if (icd9.2.5 == 4330) {v.code <- 09320}
  if (icd9.2.5 == 4340) {v.code <- 44000}
  if (icd9.2.5 == 4200) {v.code <- 40311}
  if (icd9.2.5 == 4510) {v.code <- 40311}
  if (icd9.2.5 == 5600) {v.code <- 40311}
  if (icd9.2.5 == 5680) {v.code <- 40311}
  if (icd9.2.5 == 4270) {v.code <- 07032}
  if (icd9.2.5 == 1271) {v.code <- 53170}
  if ((icd9.2.5 >= 1000) & (icd9.2.5 <= 1090)) {v.code <- 14000}
  if (icd9.2.5 == 1071) {v.code <- 20000}
  if (icd9.2.5 == 1072) {v.code <- 20000}
  if (icd9.2.5 == 1079) {v.code <- 20000}
  if (icd9.2.5 == 1130) {v.code <- 29110}
  
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
  chf <- c(398.91,402.11,402.91,404.11,404.13,404.91,404.93,
           seq(from=428, to=428.9, by=0.01))
  arrhythmia <- c(426.1,426.11,426.13,
                  seq(from=426.2, to=426.53, by=0.01),
                  seq(from=426.6, to=426.89, by=0.01),
                  427,427.2,427.31,427.6,427.9,785)
  valve <- c(seq(from=93.2, to=93.24, by=0.01),
             seq(from=394, to=397.1, by=0.01),
             seq(from=424, to=424.91, by=0.01),
             seq(from=746.3, to=746.6, by=0.01)) 
  pulm.circ <- c(seq(from=416, to=416.9, by=0.01), 417.9) 
  pvd <- c(seq(from=440, to=440.9, by=0.01),
           441.2,441.4,441.7,441.9,
           seq(from=443.1, to=443.9, by=0.01),
           447.1,557.1,557.9)
  htn <- c(401.1,401.9,402.1,402.9,404.1,404.9,405.11,405.19,405.91,405.99) 
  paralysis <- c(seq(from =342, to=342.12, by=0.01),
                 seq(from=342.9, to=344.9, by=0.01)) 
  neuro.other <- c(331.9,332,333.4,333.5,
                   seq(from=334, to=335.9, by=0.01),
                   340,
                   seq(from=341.1, to=341.9, by=0.01),
                   seq(from=345, to=345.11, by=0.01),
                   seq(from=345.4, to=345.51, by=0.01),
                   seq(from=345.8, to=345.91, by=0.01),
                   348.1,348.3,780.3,784.3)
  chronic.pulm <- c(seq(from=490, to=492.8, by=0.01),
                    seq(from=493, to=493.91, by=0.01),
                    494,
                    seq(from=495, to=505, by=0.01),
                    506.4)
  dm.uncomp <- c(seq(from=250,to=250.33,by=0.01))
  dm.comp <- c(seq(from=250.4, to=250.73, by=0.01),
               seq(from=250.9, to=250.93, by=0.01))
  hypothyroid <- c(seq(from=243, to=244.2, by=0.01),
                   244.8,244.9)
  renal <- c(403.11,403.91,404.12,404.92,585,586)
  liver <- c(70.32,70.33,70.54,456,456.1,456.2,456.21,571,571.2,571.3,
             seq(from=571.4, to=571.49, by=0.01),571.5,571.6,571.8,571.9,572.3,572.8)
  pud <- c(531.7,531.9,532.7,532.9,533.7,533.9,534.7,534.9) 
  hiv <- c(seq(from=42, to=44.9, by=0.01)) 
  lymphoma <- c(seq(from=200,to=202.38, by=0.01),
                seq(from=202.5,to=203.01, by=0.01),
                seq(from=203.8,to=203.81, by=0.01),
                238.6,273.3)
  mets <- c(seq(from=196,to=199.1, by=0.01))
  solid.tumor <- c(seq(from=140,to=172.9, by=0.01),
                   seq(from=174,to=175.9, by=0.01),
                   seq(from=179,to=195.8, by=0.01))
  rheum <- c(701,
             seq(from=710,to=710.9, by=0.01),
             seq(from=714,to=714.9, by=0.01),
             seq(from=720,to=720.9, by=0.01),
             725)
  coag <- c(seq(from=286.0,to=286.9, by=0.01),
            287.1,
            seq(from=287.3,to=287.5, by=0.01))
  obesity <- c(278)
  wt.loss <- c(seq(from=260,to=263.9, by=0.01))
  lytes <- c(seq(from=276,to=276.9, by=0.01))
  anemia.loss <- c(280)
  anemia.def <- c(seq(from=280.1,to=281.9, by=0.01),
                  285.9)
  etoh <- c(291.1,291.2,291.5,291.8,291.9,
            seq(from=303.9,to=303.93, by=0.01),
            seq(from=305,to=305.03, by=0.01))
  drugs <- c(292,
             seq(from=292.82,to=292.89, by=0.01),
             292.9,
             seq(from=304,to=304.93, by=0.01),
             seq(from=305.2,to=305.93, by=0.01))
  psychoses <- c(seq(from=295,to=298.9, by=0.01),
                 seq(from=299.1,to=299.11, by=0.01))
  depression <- c(300.4,301.12,309,309.1,311)
  
  # 30 different groups - each line contains 3 groups
  elixhauser.list <- list(chf = chf, arrhythmia = arrhythmia, valve = valve, # 3
                          pulm.circ = pulm.circ, pvd = pvd, htn = htn, # 6
                          paralysis = paralysis, neuro.other = neuro.other, chronic.pulm = chronic.pulm, # 9
                          dm.uncomp = dm.uncomp, dm.comp = dm.comp, hypothyroid = hypothyroid, # 12
                          renal = renal, liver = liver, pud = pud, # 15
                          hiv = hiv, lymphoma = lymphoma, mets = mets, # 18
                          solid.tumor = solid.tumor, rheum = rheum, coag = coag, # 21 
                          obesity = obesity, wt.loss = wt.loss, lytes = lytes, # 24
                          anemia.loss = anemia.loss, anemia.def = anemia.def, etoh = etoh, # 27
                          drugs = drugs, psychoses = psychoses, depression = depression) # 30
  
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=30)
  # Using the names for columns limits the risk of missing 
  colnames(output.frame) <- names(elixhauser.list)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
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
