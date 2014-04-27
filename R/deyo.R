#######################
##Deyo-Charlson index##
#######################

#Combining the above functions, deyo takes a 5 digit ICD-9-CM code and produces a list of 3 items:
#1. the total Charlson Score 
#2. a binary data frame of which comorbidites patients have - 1 for having it and 0 if not
#3. The point value data frame of comorbidites - 1-6 for having it, and 0 for not
deyo <- function(input.frame) {
  interim.frame.1 <- prDeyo.apply.icd9(input.frame)
  interim.frame.2 <- apply.convert.na(interim.frame.1)
  interim.frame.3 <- prDeyo.comorbidities(interim.frame.2)
  interim.frame.4 <- prDeyo.convert.to.points(interim.frame.3)
  POINTS <- total.points(interim.frame.4)
  deyo.data <- list(POINTS, interim.frame.3,interim.frame.4)
  names(deyo.data) <- c("CHARLSON.SCORE", "COMORBIDITIES", "COMORBIDITIES.POINTS")
  return(deyo.data)
}

# Convert icd9 codes to numeric values and convert V434X to 44390 
prDeyo.apply.icd9 <- function(input.frame) {
  ICD9.5digit.deyo <- function(icd.code){
    if (is.na(icd.code)) {icd.code <- "00000"}
    icd9.3 <- substr(icd.code, 1, 3)
    icd9.4 <- substr(icd.code, 4, 4)
    icd9.5 <- substr(icd.code, 5, 5)
    if (icd9.4 == "X") {icd9.4 <- 0}
    if (icd9.5 == "X") {icd9.5 <- 0}
    icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
    if (icd9.result == "V4340") {icd9.result <- 44390}
    return(as.numeric(icd9.result)/100)
  }
  
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      output.frame[i,j] <- ICD9.5digit.deyo(input.frame[i,j])
    }
  }
  return(output.frame)
}

# The following function develops a matrix with rows devoted to respondetns and each 
# column a comorbidity.  The number in each column is the point value of having the comorbidity
prDeyo.comorbidities <- function(input.frame) {
  #create lists of comorbidities
  mi <- c(seq(from=410, to=410.9, by=0.01),
          412)
  chf <- c(seq(from=428, to=428.9, by=0.01))
  pvd <- c(443.9, 441, 441.9, 785.4) #v code v43.4 not included in this list
  cvd <- c(seq(from=430, to=438, by=0.01))
  dementia <- c(seq(from=290, to=290.9, by=0.01))
  copd <- c(seq(from=490, to=496, by=0.01), 
            seq(from=500, to=505, by=0.01), 506.4)
  rheum <- c(710, 710.1, 710.4, 
             seq(from =714, to=714.2, by=0.01),
             714.81, 725)
  pud <- c(seq(from=531, to=534.9, by=0.01))
  mild.liver <- c(571.2, 571.5, 571.6, 
                  seq(from=571.4, to=571.49, by=0.01))
  dm <- c(seq(from=250,to=250.3,by=0.01),
          250.7)
  dm.comp <- c(seq(from=250.4, to=250.6, by=0.01)) #2 point items start here
  plegia <- c(344.1, 
              seq(from=342, to=342.9, by=0.01))
  renal <- c(seq(from=582, to=582.9, by=0.01), 
             seq(from=583, to=583.7, by=0.01),
             585, 586,
             seq(from=588, to=588.9, by=0.01))
  malignancy <- c(seq(from=140, to=172.9, by=0.01), 
                  seq(from=174, to=195.8, by=0.01), 
                  seq(from=200, to=208.9, by=0.01))
  severe.liver <- c(seq(from=572.2, to=572.8, by=0.01),
                    seq(from=456, to=456.21, by=0.01)) # 3 point item
  mets <- c(seq(from=196, to=199.1, by=0.01)) # 6 point items
  hiv <- c(seq(from=42, to=44.93, by=0.01))
  
  deyo.list <- list(mi = mi, chf = chf, pvd = pvd, # 3
                    cvd = cvd, dementia = dementia, copd = copd, # 6
                    rheum = rheum, pud = pud, mild.liver = mild.liver, # 9
                    dm = dm, dm.comp = dm.comp, plegia = plegia, # 12
                    renal = renal, malignancy = malignancy, severe.liver = severe.liver, # 15
                    mets = mets, hiv = hiv) # 17
  
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=17)
  # Using the names for columns limits the risk of missing 
  colnames(output.frame) <- names(elixhauser.list)
  
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      for (k in names(deyo.list)) {
        if (input.frame[i, j] %in% deyo.list[[k]]) {
          output.frame[i,k] <- 1
        }
      }
    }
  }
  
  # You can't have both uncomplicated diabetes and
  # complicated diabetes at the same time
  output.frame[output.frame[,"dm.comp"]==1, "dm"] <- 0
    
  # If a solid tumor has generated metastasis then it belongs in that group and not
  # the pure solid tumor group
  output.frame[output.frame[,"mets"]==1, "malignancy"] <- 0
  
  output.frame <- as.data.frame(output.frame)
  # Change the names to upper case as in original script
  colnames(output.frame) <- toupper(colnames(output.frame))
  return(output.frame)
}

# Convert the frame of point values to a frame of 0 for not having and 1 for having
prDeyo.convert.to.points <- function(input.frame) {
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- input.frame
  output.frame[,11] <- output.frame[,11] *2
  output.frame[,12] <- output.frame[,12] *2
  output.frame[,13] <- output.frame[,13] *2
  output.frame[,14] <- output.frame[,14] *2
  output.frame[,15] <- output.frame[,15] *3
  output.frame[,16] <- output.frame[,17] *6
  output.frame[,16] <- output.frame[,17] *6
  return(output.frame)
}
