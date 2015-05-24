#' Finds the matching codes for the different comorbidity groups
#'
#' The functions loop through all the comorbidity groups in search
#' for a matching group to the provided \code{icd_codes}.
#'
#' The \code{_regex} indicates that these functions use regular expressions
#' for identification of codes of interest. The match is case-insensitive.
#' The function can also identify acute conditions and ignore those if specified.
#'
#' @param icd_codes The icd code of interest, either a number or a string vector
#' @param out If the function has been run previously there
#'  may already be matches for a particular group, if the
#'  out parameter is supplied with a vector equal to the
#'  number of Elixhauser comorbidities with their corresponding
#'  names only new findings will be appended.
#' @param icd_ver The icd version of interest. Currently only ICD-9 is supported
#'  by the numeric matching algorithms. Leave empty.
#' @param country_code The two-letter \code{ISO 3166-1 alpha-2}
#'  code indicating the country of interest (the same as the top-level
#'  internet domain name of that country). As certain countries
#'  have adapted country-specific ICD-coding there may be minor
#'  differences between countries. Currently only
#'  US codes are implemented for the numeric algorithms.
#' @param include_acute Certain codes may indicate a non-chronic
#'  disease such as heart infarction, stroke or similar. Under some
#'  circumstances these should be ignored, e.g. when studying predictors
#'  for hip arthroplasty re-operations codes during the admission
#'  for the surgery should not include myocardial infarction as this
#'  is most likely a postoperative condition not available for scoring
#'  prior to the surgery.
#' @param ver The version number for the \code{cmrbdt.finder.numeric.ahrq} function, see \code{\link{pr.ahrq.list}}, the default is the 3.7 version
#' @return \code{vector} Returns a vector with the names of each comorbidity
#'  group. If the entry is FALSE this correspond to that no code matched the
#'  other group otherwise it returns TRUE.
#' @references \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @rdname cmrbdt.finder.numeric
#' @seealso \code{\link{cmrbdt.calc}}
#' @examples
#' cmrbdt.finder.numeric.ahrq(9320)
#' @export
#' @family cmrbdtf.finder functions
cmrbdt.finder.numeric.ahrq <-
  function(icd_codes,
           out,
           country_code,
           include_acute = rep(TRUE, length(icd_codes)),
           icd_ver = 9,
           ver = "3.7"){
  if (any(icd_ver != 9)) { stop("Only ICD-9 version is supported for the AHRQ v3.5")}
  if (!missing(country_code) && country_code != "US") { stop("Only US country code is currently supported")}
  if (!missing(include_acute) && any(include_acute == FALSE)) { stop("Excluding acute codes from episodes for the AHRQ is not yet implemented")}

  # Pre-process the comorbidity codes to match the searched codes
  icd_codes <- pr.ahrq.ICD9.5digit(icd_codes)

  #create lists of comorbidities
  ahrq.list <-
    pr.ahrq.list(ver)

  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, ahrq.list)

  # Don't check if all codes are missing
  if (all(is.na(icd_codes))) {return(out)}

  # Do the actual test loop
  for (k in names(ahrq.list)) {
    if (any(icd_codes %in% ahrq.list[[k]])) {
      out[k] <- TRUE
      next;
    }
  }

  return(out)
}

#' Converts an icd9 code to numeric format
#'
#' @param codes A \code{string/vector} numeric code
#' @return \code{integer} Returns a integer code XXXXX
#' @seealso \code{\link{ahrq}}, \code{\link{cmrbdt.finder.numeric.ahrq}}
#' @keywords internal
pr.ahrq.ICD9.5digit <- function(codes){
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
    if (icd9.1 == "V") {icd9.result <- pr.ahrq.preprocess.v.codes(icd9.result)}

    return(as.integer(icd9.result))
  })
}

#' Converts icd9 codes with V at the first letter into numeric format
#'
#' @param v.code A string numeric icd-code that starts with a V
#' @return \code{int} Returns an int with 5 digits
#' @seealso \code{\link{ahrq}}
#' @keywords internal
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
             if ((icd9.2.5 >= 8530) & (icd9.2.5 <= 8539)) {return(027800L)}
             if ((icd9.2.5 >= 8540) & (icd9.2.5 <= 8545)) {return(998540L)}
             if (icd9.2.5 == 8554) {return(027800L)}

             return(code)
           })

  return (v.code)
}

#' Gets the core AHRQ list
#'
#' The function gets the list, uses the 3.5 version and updates
#' according to the codes specified in the different versions
#' as presented in "Changes to the Comorbidity Software" on the
#' \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' webpage.
#'
#' @param version The version number of interest
#' @return \code{list} A full AHRQ-list with all the numbers of interest
#' @keywords internal
pr.ahrq.list <- function(version){
  base3.5 <- list(
    CHF = c(39891L,
            seq(from=42800L, to=42899L, by=1L),
            40201L,40211L,40291L,
            40401L,40411L,40491L,
            40403L,40413L,40493L),
    VALVE = c(seq(from=9320L, to=9324L, by=1L),
              seq(from=39400L, to=39719L, by=1L),
              39790L,
              seq(from=42400L, to=42499L, by=1L),
              seq(from=74630L, to=74669L, by=1L)),
    PULM.CIRC = c(seq(from=41511L, to=41519L, by=1L),
                  seq(from=41600L, to=41699L, by=1L), 41790L),
    PVD = c(seq(from=44000L, to=44099L, by=1L),
            seq(from=44000L, to=44199L, by=1L),
            seq(from=44200L, to=44299L, by=1L),
            seq(from=44310L, to=44399L, by=1L),
            44421L, 44422L,
            44710L, 44900L,
            55710L,55790L),
    HTN.UNCOMP = c(40110L,40190L,
                   seq(from=64200L, to=64204L, by=1L),
                   seq(from=64270L, to=64274L, by=1L),
                   seq(from=64290L, to=64294L, by=1L)),
    HTN.COMP = c(40100L,43720L, # Reg. comp
                 seq(from=64220L, to=64224L, by=1L), # Pregnant
                 40200L,40210L,40290L,40509L,40519L,40599L, # No heart failure
                 40201L,40211L,40291L, # With heart failure
                 40300L,40310L,40390L, # No renal
                 40501L,40511L,40591L,# No renal
                 seq(from=64210L, to=64214L, by=1L),# No renal
                 40301L,40311L,40391L, # With renal failure
                 40400L,40410L,40490L, # No heart or renal
                 40401L,40411L,40491L, # With heart failure
                 40402L,40412L,40492L, # With renal failure
                 40403L,40413L,40493L, # With heart and renal failure
                 seq(from=64270L, to=64274L, by=1L), # Pregnant other
                 seq(from=64290L, to=64294L, by=1L)),# Pregnant other
    PARALYSIS = c(seq(from=34200L, to=34499L, by=1L),
                  seq(from=43820L, to=43853L, by=1L),
                  78072L),
    NEURO.OTHER = c(seq(from=33000L, to=33199L, by=1L),
                    33200L,33340L,33350L,33370L,33371L,33372L,
                    33379L,33385L,33394L,
                    seq(from=33400L, to=33599L, by=1L),
                    33800L,34000L,
                    seq(from=34110L, to=34199L, by=1L),
                    seq(from=34500L, to=34511L, by=1L),
                    seq(from=34520L, to=34539L, by=1L),
                    seq(from=34540L, to=34591L, by=1L),
                    34700L,34701L,34710L,34711L,
                    seq(from=64940L, to=64944L, by=1L),
                    seq(from=76870L, to=76873L, by=1L),
                    78030L,78031L,78032L,78039L,78097L,78430L),
    CHRONIC.PULM = c(seq(from=49000L, to=49289L, by=1L),
                     seq(from=49300L, to=49392L, by=1L),
                     seq(from=49400L, to=49419L, by=1L),
                     seq(from=49500L, to=50599L, by=1L),
                     50640L),
    DM.UNCOMP = c(seq(from=25000L,to=25033L,by=1L),
                  seq(from=64800L, to=64804L, by=1L),
                  seq(from=24900L, to=24931L, by=1L)),
    DM.COMP = c(seq(from=25040L, to=25093L, by=1L),
                77510L,
                seq(from=24940L, to=24991L, by=1L)),
    HYPOTHYROID = c(seq(from=24300L, to=24429L, by=1L),
                    24480L,24490L),
    RENAL = c(58530L,58540L,58550L,58560L,58600L,58590L, 40301L,40311L,
              40391L,40402L,40412L,40492L, 40403L,40413L,40493L),
    LIVER = c(7022L,7023L,7032L,7033L,7044L,7054L,45600L,45610L,45620L,
              45621L,57100L,57120L,57130L,
              seq(from=57140L, to=57149L, by=1L),
              57150L,57160L,57180L,57190L,57230L,57280L),
    PUD = c(53141L,53151L,53161L,53170L,53171L,53191L,53241L,53251L,
            53261L,53270L,53271L,53291L,53341L,53351L,53361L,53370L,
            53371L,53391L,53441L,53451L,53461L,53470L,53471L,53491L),
    HIV = c(seq(from=4200L, to=4499L, by=1L)),
    LYMPHOMA = c(seq(from=20000L,to=20238L, by=1L),
                 seq(from=20250L,to=20301L, by=1L),
                 23860L,27330L,
                 seq(from=20302L,to=20382L, by=1L)),
    METS = c(seq(from=19600L,to=19919L, by=1L),
             seq(from=20970L,to=20975L, by=1L),
             20979L,78951L),
    SOLID.TUMOR = c(seq(from=14000L,to=17299L, by=1L),
                    seq(from=17400L,to=17599L, by=1L),
                    seq(from=17900L,to=19589L, by=1L),
                    seq(from=20900L,to=20924L, by=1L),
                    seq(from=20925L,to=20939L, by=1L),
                    seq(from=20931L,to=20936L, by=1L),
                    seq(from=25801L,to=25803L, by=1L)),
    RHEUM = c(70100L,
              seq(from=71000L,to=71099L, by=1L),
              seq(from=71400L,to=71499L, by=1L),
              seq(from=72000L,to=72099L, by=1L),
              72500L),
    COAG = c(seq(from=28600L,to=28699L, by=1L),
             28710L,
             seq(from=28730L,to=28759L, by=1L),
             seq(from=64930L,to=64934L, by=1L),
             28984L),
    OBESITY = c(27800L,27801L,27803L,
                seq(from=64910L,to=64914L, by=1L),
                79391L),
    WT.LOSS = c(seq(from=26000L,to=26399L, by=1L),
                78321L,78322L),
    LYTES = c(seq(from=27600L,to=27699L, by=1L)),
    ANEMIA.LOSS = c(28000L,seq(from=64820L,to=64824L, by=1L)),
    ANEMIA.DEF = c(seq(from=28010L,to=28199L, by=1L),
                   seq(from=28521L,to=28529L, by=1L),
                   28590L),
    ETOH = c(seq(from=29100L,to=29139L, by=1L),
             29150L,29180L,29181L,29182L,29189L,29190L,
             seq(from=30300L,to=30393L, by=1L),
             seq(from=30500L,to=30503L, by=1L)),
    DRUGS = c(29200L,
              seq(from=29282L,to=29289L, by=1L),
              29290L,
              seq(from=30400L,to=30493L, by=1L),
              seq(from=30520L,to=30593L, by=1L),
              seq(from=64830L,to=64834L, by=1L)),
    PSYCHOSES = c(seq(from=29500L,to=29899L, by=1L),
                  29910L,29911L),
    DEPRESSION = c(30040L,30112L,30900L,30910L,31100L))

  # Floating point matches may cause issues
  if (is.numeric(version))
    version <- sprintf("%.1f", version)

  if (version == "3.5")
    return(base3.5)

  if (version %in% c("3.6", "3.7")){
    base3.5$NEURO.OTHER <- c(base3.5$NEURO.OTHER, 78033L)
    base3.5$OBESITY <- c(base3.5$OBESITY, 998540L) # See the V-code conversion
    if (version == "3.6")
      return(base3.5)
  }

  if (version %in% c("3.7")){
    # The update document is faulty: 41513 %in% base3.5$PULM.CIRC
    base3.5$LIVER <- c(base3.5$LIVER, seq(from=57350L, to=57359L, by=1L)) # Hepatopulmonary syndrome Liver disease

    # The update doc. is faulty c(28652L, 28653L, 28659L) %in% base3.5$COAG # Acquired hemophilia + Antiphospholipid antibody with hemorrhagic disorder Coagulation deficiency

    if (version == "3.7")
      return(base3.5)
  }

  stop("You have requested a AHRQ version, ", version ,", that has not yet been implemented!")
}
