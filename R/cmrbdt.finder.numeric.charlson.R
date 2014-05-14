#' @rdname cmrbdt.finder.numeric
#' @references R. A. Deyo, D. C. Cherkin, and M. A. Ciol, 
#'  "Adapting a clinical comorbidity index for use with 
#'  ICD-9-CM administrative databases" Journal of Clinical Epidemiology, 
#'  vol. 45, no. 6, pp. 613-619, Jun. 1992.
#'  @export
#' @examples
#' cmrbdt.finder.numeric.charlson_Deyo1992(c(9320, 41000))
cmrbdt.finder.numeric.charlson_Deyo1992 <- 
  function(icd_codes, 
           out,
           country_code,
           include_acute = rep(TRUE, length(icd_codes)),
           icd_ver = 9){
  if (any(icd_ver != 9)) { stop("Only ICD-9 version is supported for the Deyo 1992")}
  if (!missing(country_code) && country_code != "US") { stop("Only US country code is currently supported")}
  if (!missing(include_acute) && any(include_acute == FALSE)) { stop("Excluding acute codes from episodes for the  Deyo 1992 is not yet implemented")}
  
  # Pre-process the comorbidity codes to match the searched codes
  icd_codes <- pr.deyo.ICD9.5digit(icd_codes)
  
  #create lists of comorbidities
  deyo.list <- 
    list(
      MI = c(seq(from=41000L, to=41099L, by=1L),
             41200L),
      CHF = c(seq(from=42800L, to=42899L, by=1L)),
      PVD = c(seq(44100L, 44199L, by=1L), # This should include everything under 441
              seq(44390L, 44399L, by=1L), 
              78540L), #v code v43.4 not included in this list
      CVD = c(seq(from=43000L, to=43799L, by=1L),
              43813L, 43814L), # Note table cross saying that only late effects were included from the 438 group
      DEMENTIA = c(seq(from=29000L, to=29099L, by=1L)),
      COPD = c(seq(from=49000L, to=49699L, by=1L), 
               seq(from=50000L, to=50599L, by=1L), 
               50640L),
      RHEUM = c(71000L, 71010L, 71040L, 
                seq(from=71400L, to=71429L, by=1L),
                71481L, 72500L),
      PUD = c(seq(from=53100L, to=53499L, by=1L)),
      MILD.LIVER = c(57120L, 57150L, 57160L, 
                     seq(from=57140L, to=57149L, by=1L)),
      DM = c(seq(from=25000L,to=25039L,by=1L),
             25070L),
      DM.COMP = c(seq(from=25040L, to=25069L, by=1L)), #2 point items start here
      PLEGIA = c(34410L, 
                 seq(from=34200L, to=34299L, by=1L)),
      RENAL = c(seq(from=58200L, to=58299L, by=1L), 
                seq(from=58300L, to=58379L, by=1L),
                seq(from=58500L, to=58599L, by=1L), # Multiple in group
                58600L, # Only one in this group
                seq(from=58800L, to=58899L, by=1L)),
      MALIGNANCY = c(seq(from=14000L, to=17299L, by=1L), 
                     seq(from=17400L, to=19589L, by=1L), 
                     seq(from=20000L, to=20899L, by=1L)),
      SEVERE.LIVER = c(seq(from=57220L, to=57289L, by=1L),
                       seq(from=45600L, to=45621L, by=1L)), # 3 point item
      METASTASIS = c(seq(from=19600L, to=19919L, by=1L)), # 6 point items
      HIV = c(seq(from=4200L, to=4493L, by=1L)))
  
  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, deyo.list)
  
  # Just return the empty vector if there is nothing to check
  if (is.na(icd_codes) || 
        icd_codes %in% c(0, '')) {return(out)}
  
  # Do the actual test loop
  for (k in names(deyo.list)) {
    if (any(icd_codes %in% deyo.list[[k]])) {
        out[k] <- TRUE
        next;
    }
  }
  
  return(out)
}

#' Convert to 5 digit ICD-code
#' 
#' @param codes \code{string/vector} indicating the code 
#' @return \code{float} Returns a float value XXXXX
#' @seealso \code{\link{deyo}}, \code{\link{cmrbdt.finder.numeric.charlson_Deyo1992}}
#' @keywords internal
pr.deyo.ICD9.5digit <- function(codes){
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
    icd9.3 <- substr(icd.code, 1, 3)
    icd9.4 <- substr(icd.code, 4, 4)
    icd9.5 <- substr(icd.code, 5, 5)
    if (icd9.4 == "X") {icd9.4 <- 0}
    if (icd9.5 == "X") {icd9.5 <- 0}
    icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
    if (icd9.result == "V4340") {icd9.result <- 44390L}
    return(as.integer(icd9.result))
  })
}
