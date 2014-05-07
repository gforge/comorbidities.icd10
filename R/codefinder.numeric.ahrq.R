#' Finds the matching codes for the different comorbidity groups
#' 
#' The functions loop through all the comorbidity groups in search
#' for a matching group to the provided \code{icd_codes}. 
#' 
#' The \code{_regex} indicates that these functions use regular expressions
#' for identification of codes of interest. The match is case-insensitive.
#' The function can also identify acute conditions and ignore those if specified. 
#' 
#' @param icd_codes The icd code of interest, either a number or a string
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
#' @return \code{vector} Returns a vector with the names of each comorbidity
#'  group. If the entry is FALSE this correspond to that no code matched the
#'  other group otherwise it returns TRUE.
#' @references http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp
#' @rdname codefinder.numeric
#' @examples
#' codefinder.numeric.ahrq_2010v3.5(9320)
#' 
#' @export
codefinder.numeric.ahrq_2010v3.5 <- 
  function(icd_codes, 
           out,
           country_code,
           include_acute = rep(TRUE, length(icd_codes)),
           icd_ver = 9){
  if (any(icd_ver != 9)) { stop("Only ICD-9 version is supported for the AHRQ v3.5")}
  if (!missing(country_code) && country_code != "US") { stop("Only US country code is currently supported")}
  if (!missing(include_acute) && any(include_acute == FALSE)) { stop("Excluding acute codes from episodes for the AHRQ is not yet implemented")}
  
  #create lists of comorbidities
  ahrq.list <- 
    list(
      chf = c(39891L,
              seq(from=42800L, to=42899L, by=1L), 
              40201L,40211L,40291L, 
              40401L,40411L,40491L, 
              40403L,40413L,40493L),
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
  
  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, ahrq.list)
 
  # Don't check if all codes are missing
  if (all(is.na(icd_codes))) {return(out)}
  
  # Do the actual test loop
  for (k in names(ahrq.list)) {
    if (any(icd_codes %in% ahrq.list[[k]])) {
      out[k] <- 1
      next;
    }
  }
  
  return(out)
}