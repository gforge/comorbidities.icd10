###################################################
##AHRQ comorbidites v3.6 index (newer elixhauser)##
###################################################

# ahrq_v3.6() takes a 5 digit ICD-9-CM code and produces a list of 2 items:
#1. the total count of elixhauser comorbidities 
#2. a binary data frame of which comorbidites patients have - 1 for having it and 0 if not
ahrq <- function(input.frame) {
	interim.frame.1 <- prAHRQ.apply.icd9(input.frame)
	interim.frame.2 <- apply.convert.na(interim.frame.1)
	interim.frame.3 <- prAHRQ.points(interim.frame.2)
	POINTS <- total.points(interim.frame.3)
	ahrq.data <- list(POINTS, interim.frame.3)
	names(ahrq.data) <- c("COMORBIDITY.CT", "COMORBIDITIES")
	return(ahrq.data)
}

# Convert icd9 codes to numeric values and convert v codes
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

prAHRQ.ICD9.5digit <- function(icd.code){ 
  if (is.na(icd.code)) {icd.code <- "00000"}
  icd9.1 <- substr(icd.code, 1, 1)
  icd9.3 <- substr(icd.code, 1, 3)
  icd9.4 <- substr(icd.code, 4, 4)
  icd9.5 <- substr(icd.code, 5, 5)
  if (icd9.4 == "X") {icd9.4 <- 0}
  if (icd9.5 == "X") {icd9.5 <- 0}
  icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
  if (icd9.1 == "V") {icd9.result <- prAHRQ.process.v.codes(icd9.result)}
  
  return(as.numeric(icd9.result)/100)
}

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

# The following function develops a matrix with rows devoted to respondents and each 
# column a comorbidity.
prAHRQ.points <- function(input.frame) {
  #create lists of comorbidities
  chf <- c(398.91,
           seq(from=428, to=428.9, by=0.01), 
           402.01,402.11,402.91, 404.01,404.11,404.91, 404.03,404.13,404.93) 
  valve <- c(seq(from=93.2, to=93.24, by=0.01),
             seq(from=394, to=397.1, by=0.01),
             397.9,
             seq(from=424, to=424.99, by=0.01),
             seq(from=746.3, to=746.6, by=0.01)) 
  pulm.circ <- c(seq(from =415.11, to=415.19, by=0.01),
                 seq(from=416, to=416.9, by=0.01), 417.9) 
  pvd <- c(seq(from=440, to=440.9, by=0.01),
           seq(from=440.0, to=441.9, by=0.01),
           seq(from =442.0, to=442.9, by=0.01),
           seq(from =443.1, to=443.9, by=0.01),
           444.21,441.22,447.1,449.0,557.1,557.9)
  htn <- c(401.1,401.9,
           seq(from =642.00, to=642.04, by=0.01),
           401.0,437.2,
           seq(from =642.20, to=642.24, by=0.01),
           402.0,402.1,402.9,405.09,405.19,405.99, 
           402.01,402.11,402.91, 403.0,403.1,403.9,
           405.01,405.11,405.91,
           seq(from=642.10, to=642.14, by=0.01),
           403.01,403.11,403.91,404.0,404.1,404.9,
           404.01,404.11,404.91, 404.02,404.12,404.92, 
           404.03,404.13,404.93, 
           seq(from =642.7, to=642.74, by=0.01),
           seq(from =642.9, to=642.94, by=0.01))  
  paralysis <- c(seq(from =342, to=344.9, by=0.01),
                 seq(from=438.20, to=438.53, by=0.01),
                 780.72) 
  neuro.other <- c(seq(from=330.0, to=331.9, by=0.01),
                   332,333.4,333.5,333.7,333.71,333.72,
                   333.79,333.85,333.94,
                   seq(from=334.0, to=335.9, by=0.01),
                   338.0,340.0,
                   seq(from=341.1, to=341.9, by=0.01),
                   seq(from=345.0, to=345.11, by=0.01),
                   seq(from =345.2, to=345.3, by=0.01),
                   seq(from=345.4, to=345.91, by=0.01),
                   347.0,347.01,347.10,347.11,
                   seq(from =649.4, to=649.44, by=0.01),
                   786.7,
                   seq(from =786.7, to=786.73, by=0.01),
                   780.30,780.31,780.32,780.39,780.97,784.3)
  chronic.pulm <- c(seq(from=490, to=492.8, by=0.01),
                    seq(from=493, to=493.92, by=0.01),
                    seq(from =494.0, to=494.1, by=0.01),
                    seq(from=495, to=505, by=0.01),
                    506.4)
  dm.uncomp <- c(seq(from=250,to=250.33,by=0.01),
                 seq(from=648.0, to=648.04, by=0.01),
                 seq(from=249.0, to=249.31, by=0.01))
  dm.comp <- c(seq(from=250.4, to=250.93, by=0.01),
               775.1,
               seq(from=249.4, to=249.91, by=0.01))
  hypothyroid <- c(seq(from=243, to=244.2, by=0.01),
                   244.8,244.9)
  renal <- c(585.3,585.4,585.5,585.6,585.9, 403.01,403.11,
             403.91,404.02,404.12,404.92, 404.03,404.13,404.93) 
  liver <- c(70.22,70.23,70.32,70.33,70.44,70.54,456,456.1,456.2,
             456.21,571,571.2,571.3,
             seq(from=571.4, to=571.49, by=0.01),
             571.5,571.6,571.8,571.9,572.3,572.8)
  pud <- c(531.41,531.51,531.61,531.7,531.71,531.91,532.41,532.51,
           532.61,532.7,532.71,532.91,533.41,533.51,533.61,533.7,
           533.71,533.91,534.41,534.51,534.61,534.7,534.71,534.91) 
  hiv <- c(seq(from=42, to=44.9, by=0.01)) 
  lymphoma <- c(seq(from=200,to=202.38, by=0.01),
                seq(from=202.5,to=203.01, by=0.01),
                238.6,273.3,
                seq(from=203.02,to=203.82, by=0.01))
  mets <- c(seq(from=196,to=199.1, by=0.01),
            seq(from=209.7,to=209.75, by=0.01),
            209.79,789.51)
  solid.tumor <- c(seq(from=140,to=172.9, by=0.01),
                   seq(from=174,to=175.9, by=0.01),
                   seq(from=179,to=195.8, by=0.01),
                   seq(from=209.0,to=209.24, by=0.01),
                   seq(from=209.25,to=209.3, by=0.01),
                   seq(from=209.31,to=209.36, by=0.01),
                   seq(from=258.01,to=258.03, by=0.01))
  rheum <- c(701,
             seq(from=710,to=710.9, by=0.01),
             seq(from=714,to=714.9, by=0.01),
             seq(from=720,to=720.9, by=0.01),
             725) 
  coag <- c(seq(from=286.0,to=286.9, by=0.01),
            287.1,
            seq(from=287.3,to=287.5, by=0.01),
            seq(from=649.3,to=649.34, by=0.01),
            289.84)
  obesity <- c(278.0,278.01,278.03,
               seq(from=649.1,to=649.14, by=0.01),
               793.91) 
  wt.loss <- c(seq(from=260,to=263.9, by=0.01),
               783.21,783.22)
  lytes <- c(seq(from=276,to=276.9, by=0.01))
  anemia.loss <- c(280,seq(from=648.2,to=648.24, by=0.01))
  anemia.def <- c(seq(from=280.1,to=281.9, by=0.01),
                  seq(from=285.21,to=285.29, by=0.01),
                  285.9) 
  etoh <- c(seq(from=291.0,to=291.3, by=0.01),
            291.5,291.8,291.81,291.82,291.89,291.9,
            seq(from=303.0,to=303.93, by=0.01),
            seq(from=305,to=305.03, by=0.01))
  drugs <- c(292,
             seq(from=292.82,to=292.89, by=0.01),
             292.9,
             seq(from=304,to=304.93, by=0.01),
             seq(from=305.2,to=305.93, by=0.01),
             seq(from=648.3,to=648.34, by=0.01))
  psychoses <- c(seq(from=295,to=298.9, by=0.01),
                 299.1,299.11)
  depression <- c(300.4,301.12,309,309.1,311)
  
  ahrq.list <- list(chf = chf, valve = valve, pulm.circ = pulm.circ, # 3
                    pvd = pvd, htn = htn, paralysis = paralysis, # 6
                    neuro.other = neuro.other, chronic.pulm = chronic.pulm, dm.uncomp = dm.uncomp, # 9
                    dm.comp = dm.comp, hypothyroid = hypothyroid, renal = renal, # 12
                    liver = liver, pud = pud, hiv = hiv, # 15
                    lymphoma = lymphoma, mets = mets, solid.tumor = solid.tumor, # 18
                    rheum = rheum, coag = coag, obesity = obesity, # 21
                    wt.loss = wt.loss, lytes = lytes, anemia.loss = anemia.loss, # 24
                    anemia.def = anemia.def, etoh = etoh, drugs = drugs, # 27
                    psychoses = psychoses, depression = depression) # 29
  
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

