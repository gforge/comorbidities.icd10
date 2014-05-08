#' @rdname cmrbdt.finder.regex
#' @references J. N. Armitage and J. H. van der Meulen, 
#'  "Identifying co-morbidity in surgical patients using 
#'  administrative data with the Royal College of Surgeons 
#'  Charlson Score" British Journal of Surgery, vol. 97, 
#'  no. 5, pp. 772-781, May 2010.
#' @export
#' @examples
#' cmrbdt.finder.regex.charlson_Armitage2010("I252")
cmrbdt.finder.regex.charlson_Armitage2010 <- 
  function(icd_codes, 
           out,
           country_code,
           include_acute = rep(TRUE, times=length(icd_codes)),
           icd_ver = rep(FALSE, times=length(icd_codes))){
  
  # Based on Armitage JN, van der Meulen JH. 
  # -- Identifying co-morbidity in surgical patients using 
  # --- administrative data with the Royal College of Surgeons Charlson Score. 
  # -- British Journal of Surgery. 2010 Maj 1;97(5):772-781. 
  
  rcs_charlsons <- list()
  rcs_charlsons[['MI']] <- 
    list(icd10 = c('^I2([123]|52)'),
         icd9 = c('^41[02]'))
  
  rcs_charlsons[['CHF']] <- 
    list(icd10 = c('^I1[13]', '^I255', '^I4[23]', '^I50', '^I517'),
         icd9 = c('^39891', '^402(01|11|91)', '^404(01|03|[19][13])', '^42(5[456789]|8)'))
  
  rcs_charlsons[['PVD']] <- 
    list(icd10 = c('^I7[0123]', '^I77[01]', '^K55[189]', '^R02', '^Z95[89]'),
         icd9 = c('^0930', '^4373', '^44([01]|3[123456789]|71)', '^557[19]', '^V434'))
  
  rcs_charlsons[['CEVD']] <- 
    list(icd10 = c('^G4[56]', '^I6'),
         icd9 = c('^36234', '^43[012345678]'))
  
  rcs_charlsons[['DEM']] <- 
    list(icd10 = c('^A810','^F0([0123]|51)', '^G3[01]'),
         icd9 = c('^29(0|41)', '^3312'))
  
  rcs_charlsons[['COPD']] <- 
    list(icd10 = c('^I26', '^I27', '^J4[01234567]', '^J6([01234567]|84)', '^J70[13]'),
         icd9 = c('^416[89]', '^49', '^50([012345]|64|8[18])'))
  
  rcs_charlsons[['Rheum']] <- 
    list(icd10 = c('^M0[569]', '^M120', '^M3(15|[23456])'),
         icd9 = c('^4465', '^71(0[01234]|4[0128])', '^725'))
  
  rcs_charlsons[['LD']] <- 
    list(icd10 = c('^B18', '^I85', '^I864', '^I982', '^K7([01]|2[19]|6)', '^R162', '^Z944'),
         icd9 = c('^070([23][23]|[45]4|[69])', '^456[012]', '^572[2345678]', '^57([01]|3[3489])', '^V427'))
  
  rcs_charlsons[['DIAB']] <- 
    list(icd10 = c('^E1[01234]'),
         icd9 = c('^250'))
  
  rcs_charlsons[['PARA']] <- 
    list(icd10 = c('^G114', '^G8[123]'),
         icd9 = c('^3341', '^34([23]|4[01234569])'))
  
  rcs_charlsons[['RD']] <- 
    list(icd10 = c('^I1[23]', '^N0[13578]', '^N1(7[12]|[89])', '^N25', '^Z49', '^Z940', '^Z992'),
         icd9 = c('^403[019]1', '^404[019][23]', '^58(2|3[01234567]|[56]|80)', '^V4(20|51)', '^V56'))
  
  rcs_charlsons[['CANCER']] <- 
    list(icd10 = c('^C[01]', '^C2[0123456]', '^C3[01234789]', '^C4[01356789]', '^C5[012345678]', '^C6', '^C7[0123456]', '^C8[0123458]', '^C9[01234567]'),
         icd9 = c('^1[456]', '^17[012456789]', '^18', '^19[012345]', '^20[012345678]', '^2386'))
  
  rcs_charlsons[['METASTASIS']] <- 
    list(icd10 = c('^C7[789]'),
         icd9 = c( '^19[6789]'))
  
  rcs_charlsons[['HIV']] <- 
    list(icd10 = c('^B2[0124]'),
         icd9 = c( '^04[234]'))
  
  acute_icd_codes <- list(icd10= '^(I2[123]|J46|N17[12]|N19)',
                          # Used the translator from the Swedish National Board of Healthe and Welfare (Socialstyrelsen)
                          icd9 = '^(410|42(30|95|96|98)|4939|58[34][67]|5908|586|7919|5939)')
  
  # Speeds up only to check the codes that are possible
  icd_ver = pr.get.icd.ver(icd_codes, icd_ver)
  
  # Get a correctly formatted output vector
  out <- pr.get.out.vector(out, rcs_charlsons)
  
  # Do the actual test loop
  out <- pr.regex.code.match(out = out, 
                             icd_codes = icd_codes, 
                             cmrbdt.finder.regex.comorbidity = rcs_charlsons,
                             icd_ver =  icd_ver,
                             include_acute = include_acute,
                             cmrbdt.finder.regex.acute = acute_icd_codes)
  
  return(out)
}

