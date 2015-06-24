#' @rdname cmrbdt.finder.numeric
#' @references Elixhauser A, Steiner C, Harris DR, Coffey RM. (1998)
#' Comorbidity measures for use with administrative data.  Med Care. 36:8-27.
#' @export
#' @examples
#' cmrbdt.finder.numeric.elixhauser_Elixhauser1998(34190)
cmrbdt.finder.numeric.elixhauser_Elixhauser1998 <- 
  function(icd_codes, 
           out,
           country_code = 'US',
           include_acute = rep(TRUE, length(icd_codes)),
           icd_ver = 9){
  if (any(icd_ver != 9)) { stop("Only ICD-9 version is supported for the Deyo 1992")}
  if (!missing(country_code) && country_code != "US") { stop("Only US country code is currently supported")}
  if (!missing(include_acute) && any(include_acute == FALSE)) { stop("Excluding acute codes from episodes for the  Deyo 1992 is not yet implemented")}
  
  # Pre-process the comorbidity codes to match the searched codes
  icd_codes <- pr.elixhauser.ICD9.5digit(icd_codes)
  
  #create lists of comorbidities
  elixhauser.list <-
    list(CHF = c(39891L,40211L,40291L,40411L,40413L,40491L,40493L,
                 seq(from=42800L, to=42899L, by=1L)),
         ARRHYTHMIA = c(42610L,42611L,42613L,
                        seq(from=42620L, to=42653L, by=1L),
                        seq(from=42660L, to=42689L, by=1L),
                        42700L,42720L,42731L,42760L,42790L,78500L),
         VALVE = c(seq(from=9320L, to=9324L, by=1L),
                   seq(from=39400L, to=39719L, by=1L),
                   seq(from=42400L, to=42491L, by=1L),
                   seq(from=74630L, to=74669L, by=1L)),
         PULM.CIRC = c(seq(from=41600L, to=41690L, by=1L), 
                       41790L),
         PVD = c(seq(from=44000L, to=44099L, by=1L),
                 44120L,44140L,44170L,44190L,
                 seq(from=44310L, to=44399L, by=1L),
                 44710L,55710L,55790L),
         HTN.UNCOMP = c(40110L,40190L),
         HTN.COMP = c(40210L,40290L, # Separated hypertension into complicated and un-complicated
                      40410L,40490L,
                      40511L,40519L,
                      40591L,40599L),
         PARALYSIS = c(seq(from =34200L, to=34212L, by=1L),
                       seq(from=34290L, to=34499L, by=1L)),
         NEURO.OTHER = c(33190L,33200L,33340L,33350L,
                         seq(from=33400L, to=33599L, by=1L),
                         34000L,
                         seq(from=34110L, to=34199L, by=1L),
                         seq(from=34500L, to=34511L, by=1L),
                         seq(from=34540L, to=34551L, by=1L),
                         seq(from=34580L, to=34591L, by=1L),
                         34810L,34830L,78030L,78430L),
         CHRONIC.PULM = c(seq(from=49000L, to=49289L, by=1L),
                          seq(from=49300L, to=49391L, by=1L),
                          49400L,
                          seq(from=49500L, to=50599L, by=1L),
                          50640L),
         DM.UNCOMP = c(seq(from=25000L,to=25033L,by=1L)),
         DM.COMP = c(seq(from=25040L, to=25073L, by=1L),
                     seq(from=25090L, to=25093L, by=1L)),
         HYPOTHYROID = c(seq(from=24300L, to=24429L, by=1L),
                         24480L,24490L),
         RENAL = c(40311L,40391L,40412L,40492L,58500L,58600L),
         LIVER = c(7032L,7033L,7054L,45600L,45610L,
                   45620L,45621L,57100L,57120L,57130L,
                   seq(from=57140L, to=57149L, by=1L),
                   57150L,57160L,57180L,57190L,57230L,57280L),
         PUD = c(53170L,53190L,53270L,53290L,53370L,53390L,53470L,53490L),
         HIV = c(seq(from=4200L, to=4499L, by=1L)),
         LYMPHOMA = c(seq(from=20000L,to=20238L, by=1L),
                      seq(from=20250L,to=20301L, by=1L),
                      seq(from=20380L,to=20381L, by=1L),
                      23860L,27330L),
         METS = c(seq(from=19600L,to=19919L, by=1L)),
         SOLID.TUMOR = c(seq(from=14000L,to=17299L, by=1L),
                         seq(from=17400L,to=17599L, by=1L),
                         seq(from=17900L,to=19589L, by=1L)),
         RHEUM = c(70100L,
                   seq(from=71000L,to=71099L, by=1L),
                   seq(from=71400L,to=71499L, by=1L),
                   seq(from=72000L,to=72099L, by=1L),
                   72500L),
         COAG = c(seq(from=28600L,to=28699L, by=1L),
                  28710L,
                  seq(from=28730L,to=28759L, by=1L)),
         OBESITY = c(27800L),
         WT.LOSS = c(seq(from=26000L,to=26399L, by=1L)),
         LYTES = c(seq(from=27600L,to=27699L, by=1L)),
         ANEMIA.LOSS = c(28000L),
         ANEMIA.DEF = c(seq(from=28010L,to=28199L, by=1L),
                        28590L),
         ETOH = c(29110L,29120L,29150L,29180L,29190L,
                  seq(from=30390L,to=30393L, by=1L),
                  seq(from=30500L,to=30503L, by=1L)),
         DRUGS = c(29200L,
                   seq(from=29282L,to=29289L, by=1L),
                   29290L,
                   seq(from=30400L,to=30493L, by=1L),
                   seq(from=30520L,to=30593L, by=1L)),
         PSYCHOSES = c(seq(from=29500L,to=29899L, by=1L),
                       seq(from=29910L,to=29911L, by=1L)),
         DEPRESSION = c(30040L,30112L,30900L,30910L,31100L))
  
  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, elixhauser.list)
  
  # Just return the empty vector if there is nothing to check
  if (is.na(icd_codes) || 
        icd_codes %in% c(0, '')) {return(out)}
  
  # Do the actual test loop
  for (k in names(elixhauser.list)) {
    if (any(icd_codes %in% elixhauser.list[[k]])) {
      out[k] <- TRUE
      next;
    }
  }
  
  return(out)
}


#' Convert to 5 digit ICD-code
#' 
#' @param codes \code{string/vector} indicating the code 
#' @return \code{integer} Returns a integer value XXXXX
#' @seealso \code{\link{elixhauser}}, \code{\link{cmrbdt.finder.numeric.elixhauser_Elixhauser1998}}
#' @keywords internal
pr.elixhauser.ICD9.5digit <- function(codes){ 
  if (!is.null(dim(codes)) &&
        (length(dim(codes)) != 2 ||
           !1 %in% dim(codes))) {stop("This function can only handle single strings or vectors")}
  
  sapply(codes, USE.NAMES=FALSE, 
         FUN=function(icd.code){
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
  })
}

#' Converts icd9 codes with V at the first letter into numeric format
#'  
#' @param v.code A \code{string/vector} with icd-code(s) that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{elixhauser}}
#' @keywords internal
pr.elixhauser.process.v.codes <- function(v.code) {
  if (!is.null(dim(v.code))){ stop("The function cannot complex data structures,",
                                   " only vectors or single strings are allowed")}
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
