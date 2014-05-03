#' @rdname pr_codefinder_numeric
#' @references R. A. Deyo, D. C. Cherkin, and M. A. Ciol, 
#'  "Adapting a clinical comorbidity index for use with 
#'  ICD-9-CM administrative databases" Journal of Clinical Epidemiology, 
#'  vol. 45, no. 6, pp. 613-619, Jun. 1992.
pr.charlson_Deyo1992_numeric <- 
  function(icdCode, 
           out,
           country_code,
           include_acute = rep(TRUE, length(icdCode)),
           icd_ver = 9){
  if (any(icd_ver != 9)) { stop("Only ICD-9 version is supported for the Deyo 1992")}
  if (!missing(country_code) && country_code != "US") { stop("Only US country code is currently supported")}
  if (!missing(include_acute) && any(include_acute == FALSE)) { stop("Excluding acute codes from episodes for the  Deyo 1992 is not yet implemented")}
  
  #create lists of comorbidities
  deyo.list <- 
    list(
      mi = c(seq(from=41000L, to=41099L, by=1L),
             41200L),
      chf = c(seq(from=42800L, to=42899L, by=1L)),
      pvd = c(seq(44100L, 44199L, by=1L), # This should include everything under 441
              seq(44390L, 44399L, by=1L), 
              78540L), #v code v43.4 not included in this list
      cvd = c(seq(from=43000L, to=43799L, by=1L),
              43813L, 43814L), # Note table cross saying that only late effects were included from the 438 group
      dementia = c(seq(from=29000L, to=29099L, by=1L)),
      copd = c(seq(from=49000L, to=49699L, by=1L), 
               seq(from=50000L, to=50599L, by=1L), 
               50640L),
      rheum = c(71000L, 71010L, 71040L, 
                seq(from=71400L, to=71429L, by=1L),
                71481L, 72500L),
      pud = c(seq(from=53100L, to=53499L, by=1L)),
      mild.liver = c(57120L, 57150L, 57160L, 
                     seq(from=57140L, to=57149L, by=1L)),
      dm = c(seq(from=25000L,to=25039L,by=1L),
             25070L),
      dm.comp = c(seq(from=25040L, to=25069L, by=1L)), #2 point items start here
      plegia = c(34410L, 
                 seq(from=34200L, to=34299L, by=1L)),
      renal = c(seq(from=58200L, to=58299L, by=1L), 
                seq(from=58300L, to=58379L, by=1L),
                seq(from=58500L, to=58599L, by=1L), # Multiple in group
                58600L, # Only one in this group
                seq(from=58800L, to=58899L, by=1L)),
      malignancy = c(seq(from=14000L, to=17299L, by=1L), 
                     seq(from=17400L, to=19589L, by=1L), 
                     seq(from=20000L, to=20899L, by=1L)),
      severe.liver = c(seq(from=57220L, to=57289L, by=1L),
                       seq(from=45600L, to=45621L, by=1L)), # 3 point item
      mets = c(seq(from=19600L, to=19919L, by=1L)), # 6 point items
      hiv = c(seq(from=4200L, to=4493L, by=1L)))
  
  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, deyo.list)
  
  # Just return the empty vector if there is nothing to check
  if (is.na(icdCode) || 
        icdCode %in% c(0, '')) {return(out)}
  
  # Do the actual test loop
  for (k in names(deyo.list)) {
    if (any(icdCode %in% deyo.list[[k]])) {
        out[k] <- TRUE
        next;
    }
  }
  
  return(out)
}