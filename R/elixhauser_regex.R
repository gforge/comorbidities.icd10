#' Finds the matching codes for the different comorbidity groups
#' 
#' The functions loop through all the comorbidity groups in search
#' for a matching group to the provided \code{icdCode}. 
#' 
#' The \code{_regex} indicates that these functions use regular expressions
#' for identification of codes of interest. The match is case-insensitive.
#' The function can also identify acute conditions and ignore those if specified. 
#' 
#' @param icdCode The icd code of interest, either a number or a string
#' @param out If the function has been run previously there
#'  may already be matches for a particular group, if the
#'  out parameter is supplied with a vector equal to the
#'  number of Elixhauser comorbidities with their corresponding
#'  names only new findings will be appended.
#' @param country_code The two-letter \code{ISO 3166-1 alpha-2}
#'  code indicating the country of interest (the same as the top-level
#'  internet domain name of that country). As certain countries
#'  have adapted country-specific ICD-coding there may be minor 
#'  differences between countries. Currently only Swedish (SE) and
#'  US codes are implemented. The function defaults to 'US'.
#' @param include_acute Certain codes may indicate a non-chronic
#'  disease such as heart infarction, stroke or similar. Under some
#'  circumstances these should be ignored, e.g. when studying predictors
#'  for hip arthroplasty re-operations codes during the admission
#'  for the surgery should not include myocardial infarction as this
#'  is most likely a postoperative condition not available for scoring
#'  prior to the surgery.
#' @param icd_ver The icd version of interest. If FALSE the software tries
#'  to automatically identify code version depending on the first letter, 
#'  \code{\link{prIsICD10Code}}
#' @return \code{vector} Returns a vector with the names of each comorbidity
#'  group. If the entry is FALSE this correspond to that no code matched the
#'  other group otherwise it returns TRUE.
#' @references H. Quan, V. Sundararajan, P. Halfon, A. Fong, B. Burnand, J.-C. Luthi, 
#' L. D. Saunders, C. A. Beck, T. E. Feasby, and W. A. Ghali, “Coding algorithms for 
#' defining comorbidities in ICD-9-CM and ICD-10 administrative data,” Med Care, 
#' vol. 43, no. 11, pp. 1130–1139, Nov. 2005.
#' @rdname pr_codefinder_regex
prElixhausers_Quan2005_regex <- function(icdCode, 
                              out,
                              country_code,
                              include_acute = TRUE,
                              icd_ver = FALSE){
  
  available_country_codes <- c('SE', 'US')
  if (missing(country_code)){
    country_code <- "US"
  }else if(!country_code %in% available_country_codes){
    stop("Your chosen country code, '", country_code, 
         "' is not yet implemented.",
         " If you check for discrepancies with the original classiciation",
         " the package maintainers will try to adapt the code,",
         " and add your country code to the list.",
         " Currently the following codes are available: ", 
         paste(available_country_codes, collapse=", "))
  }
  
  elixhausers <- list()
  #Congestive heart failure
  elixhausers[['CHF']] <- 
    list(icd10 = c('^I099', '^I1(10|3[02])', '^I255', '^I4(2[056789]|3)', '^I50', '^P290'),
         icd9 = switch(country_code,
                       US=c('^39891', '^402(01|11|91)', '^404(01|03|[19][13])', '^42(5[456789]|8)'),
                       # Changes due to the Swedish coding
                       SE= c('^3980', '^402', '^4040', '^42(5[456789]|8)')))
  
  # Cardiac arrhythmias
  elixhausers[['Arrhy']] <- 
    list(icd10 = c('^I44[123]', '^I456', '^I459',
                   '^I4[789]', '^R00[018]', '^T821',
                   '^Z[49]50'),
         icd9 = switch(country_code,
                       US=c('^426([079]|1[023])',
                            '^427[012346789]', # Maybe '^427[^5]' is faster
                            '^7850',
                            '^9960[14]',
                            '^V450', '^V533'),
                       #Swedish changes
                       SE = c('^426[079]', # The .1 codes are not valid for Sweden
                              '^427[012346789]', # Maybe '^427[^5]' is faster
                              '^7850',
                              #'^9960[14]',
                              '^V450', '^V533')))
  
  # Valvular disease
  elixhausers[['VD']] <- 
    list(icd10 = c('^A520', '^I0[5678]',
                   '^I09[18]', '^I3[456789]',
                   '^Q23[0123]', '^Z95[234]'),
         # No country_code difference
         icd9 = c('^0932', '^39[4567]', '^424',
                  '^746[3456]', '^V422', '^V433'))
  
  # Pulmonary circulation disorders
  elixhausers[['PCD']] <- 
    list(icd10 = c('^I2([67]|8[089])'),
         # No country code difference
         icd9 = c('^415[01]', '^416',
                  '^417[089]'))
  
  elixhausers[['PVD']] <- 
    list(icd10 = c('^I7([01]|3[189]|71|9[02])', '^K55[189]', '^Z95[89]'),
         # No country code difference
         icd9 = c('^0930', '^4373', '^44([01]|3[123456789])', '^4471', 
                  '^557[19]', '^V434'))
  
  # Hypertension, uncomplicated
  elixhausers[['HPTN_UC']] <- 
    list(icd10 = c('^I10'),
         # No country code difference
         icd9 = c('^401'))
  
  # Hypertension, complicated
  elixhausers[['HPTN_C']] <- 
    list(icd10 = c('^I1[1235]'),
         # No country code difference
         icd9 = c('^40[2345]'))
  
  # Paralysis
  elixhausers[['Para']] <- 
    list(icd10 = c('^G041', '^G114', '^G8(0[12]|[12]|3[012349])'),
         # No country code difference
         icd9 = c('^3341', '^34([23]|4[01234569])'))
  
  # Other neurological disorders
  elixhausers[['OthND']] <- 
    list(icd10 = c('^G1[0123]', '^G2[012]', '^G25[45]',
                   '^G31[289]',
                   '^G3[2567]',
                   '^G4[01]', '^G93[14]',
                   '^R470', '^R56'),
         # No country code difference
         icd9 = c('^3319', '^332[01]',
                  '^333[45]',
                  '^33([45]|62)',
                  '^34([015]|8[13])',
                  '^78[04]3'))
  
  # Chronic pulmonary disease
  elixhausers[['COPD']] <- 
    list(icd10 = c('^I27[89]', '^J4[01234567]', '^J6([01234567]|84)', '^J70[13]'),
         # No country code difference
         icd9 = c('^416[89]', '^49', '^50([012345]|64|8[18])'))
  
  
  # Diabetes, uncomplicated
  # Slightly different from charlsons
  elixhausers[['Diab_UC']] <- 
    list(icd10 = c('^E1[01234][019]'),
         # No country code difference
         icd9 = c('^250[0123]'))
  
  # Diabetes, complicated
  # Slightly different from charlsons
  elixhausers[['Diab_C']] <- 
    list(icd10 = c('^E1[01234][2345678]'),
         # No country code difference
         icd9 = c('^250[456789]'))
  
  # Hypothyroidism
  elixhausers[['Hptothy']] <- 
    list(icd10 = c('^E0[0123]', '^E890'),
         # No country code difference
         icd9 = c('^2409', '^24([34]|6[18])'))
  
  # Renal failure
  # Differs from Charlsons
  elixhausers[['RF']] <- 
    list(icd10 = c('^I120', '^I131', '^N1[89]', '^N250', '^Z49[012]', '^Z940', '^Z992'),
         icd9 = switch(counry_code,
                       US=c('^403[019]1', '^404[019][23]', '^58([56]|80)', 
                            '^V4(20|51)', '^V56'),
                       # Swe change
                       SE=c('^403', '^404', '^58([56]|80)', 
                            '^V4(20|51)', '^V56')))
  
  
  # Liver disease
  elixhausers[['LD']] <- 
    list(icd10 = c('^B18', '^I8(5|64)', '^I982', '^K7(0|1[13457]|[234]|6[023456789])', '^Z944'),
         icd9 = switch(country_code,
                       US=c('^070([23][23]|[45]4|[69])', '^456[012]', 
                            '^57([01]|2[2345678]|3[3489])', '^V427'),
                       SE=c('^070([23]|[45])', '^456[012]', 
                            '^57([01]|2[2345678]|3[3489])', '^V427')))
  
  
  # Peptic ulcer disease excluding bleeding
  elixhausers[['PUD_NB']] <- 
    list(icd10 = c('^K2[5678][79]'),
         # No country code difference
         icd9 = c('^53[1234][79]'))
  
  
  # AIDS/HIV
  elixhausers[['HIV']] <- 
    list(icd10 = c('^B2[0124]'),
         # No country code difference
         icd9 = c('^04[234]'))
  
  # Lymphoma
  elixhausers[['Lymp']] <- 
    list(icd10 = c('^C8[123458]',
                   '^C96', '^C90[02]'),
         # No country code difference
         icd9 = c('^20[012]', '^2030', '^2386'))
  
  # Metastatic cancer
  elixhausers[['METS']] <- 
    list(icd10 = c('^C7[789]', '^C80'),
         # No country code difference
         icd9 = c('^19[6789]'))
  
  # Solid tumor without metastasis
  elixhausers[['Tumor']] <- 
    list(icd10 = c('^C[01]', '^C2[0123456]',
                   '^C3[01234789]', '^C4[01356789]', '^C5[012345678]',
                   '^C6', '^C7[0123456]', '^C97'),
         icd9 = c('^1[456]', '^17[012456789]', '^18', '^19([012345])'))
  
  
  # Rheumatoid arthritis/collagen vascular diseases
  elixhausers[['Rheum_A']] <- 
    list(icd10 = c('^L94[013]', '^M0[568]', '^M12[03]',
                   '^M3(0|1[0123]|[2345])', '^M4(5|6[189])'),
         icd9 = switch(country_code,
                       US=c('^446', '^7010', '^71(0[0123489]|12|4|93)', 
                            '^72([05]|8(5|89)|930)'),
                       SE=c('^446', '^7010', '^71(0[0123489]|12|4|93)', 
                            '^72([05]|8[58]|93)')))
  
  # Coagulopathy
  elixhausers[['Coag']] <- 
    list(icd10 = c('^D6[5678]',
                   '^D69[13456]'),
         # No country code difference
         icd9 = c('^286', '^2871', '^287[345]'))
  
  # Obesity
  elixhausers[['Obesity']] <- 
    list(icd10 = c('^E66'),
         # No country code difference
         icd9 = c('^2780'))
  
  # Weight loss
  elixhausers[['WL']] <- 
    list(icd10 = c('^E4[0123456]', '^R634', '^R64'),
         # No country code difference
         icd9 = c('^26[0123]', '^7832', '^7994'))
  
  # Fluid and electrolyte disorders
  elixhausers[['Fluid']] <- 
    list(icd10 = c('^E222', '^E8[67]'),
         # No country code difference
         icd9 = c('^2536', '^276'))
  
  # Blood loss anemia
  elixhausers[['BLA']] <- 
    list(icd10 = c('^D500'),
         # No country code difference
         icd9 = c('^2800'))
  
  # Deficiency anemia
  elixhausers[['DA']] <-
    list(icd10 = c('^D50[89]', '^D5[123]'),
         # No country code difference
         icd9 = c('^280[123456789]', '^281'))
  
  # Alcohol abuse
  elixhausers[['Alcohol']] <- 
    list(icd10 = c('^F10', '^E52', '^G621', '^I426',
                   '^K292', '^K70[039]',
                   '^T51', '^Z502',
                   '^Z714', '^Z721'),
         # No country code difference
         icd9 = c('^2652', '^291[12356789]',
                  '^303[09]', '^3050', '^3575',
                  '^4255', '^5353', '^571[0123]', '^980', '^V113'))
  
  # Drug abuse
  elixhausers[['Drug']] <- 
    list(icd10 = c('^F1[12345689]',
                   '^Z715', '^Z722'),
         # No country code difference
         icd9 = c('^292', '^304', '^305[23456789]', '^V6542'))
  
  # Psychoses
  elixhausers[['Psycho']] <- 
    list(icd10 = c('^F2[0234589]',
                   '^F3([01]2|15)'),
         icd9 = switch(country_code,
                       US=c('^2938', 
                            '^296[0145]4',
                            '^29[578]'),
                       SE=c('^2938', 
                            #'^296[0145]4',
                            # Changes in Swedish version
                            '^296[08]',
                            '^29[578]')))

  
  # Depression
  elixhausers[['Dep']] <- 
    list(icd10 = c('^F204',
                   '^F31[345]', '^F3[23]',
                   '^F341', '^F4[13]2'),
         icd9 = switch(country_code,
                       US=c('^296[235]',
                            '^30(04|9)', '^311'),
                       SE=c('^296[135]',
                            '^30(04|9)', '^311')))

  # Speeds up only to check the codes that are possible
  if (icd_ver == FALSE){
    code_is_icd10 <- prIsICD10Code(icdCode)
  }else{
    code_is_icd10 <- icd_ver == 10
  }

  if (missing(out) || 
        length(out) == 0){
    out <- rep(FALSE, times=length(elixhausers))
    names(out) <- names(elixhausers)
  }else if(length(out) != length(elixhausers)){
    stop("You have provided a list from previous runs that does not seem to originate",
         " from the elixhauser calculation, it has the length of '", length(out), "'",
         " while it should have the length '", length(elixhausers), "'")
  }else if(!all(names(elixhausers) %in% names(out))){
      stop("You have provided a list from previous runs that does not seem to originate",
           " from the elixhauser calculation, it has the correct length",
           " but lacks the elements: ", 
           paste0("'",
                  names(elixhausers)[!names(elixhausers) %in% names(out)],
                  "'",
                  collapse="','"))
      
  }
  
  # TODO: Check which codes are non-unique and need to loop further
  #       there should be substantial benefit if the code stepped out
  #       from the function as it hits the first match
  for (key_disease in names(elixhausers)){
    for (regex_str in elixhausers[[key_disease]][[ifelse(code_is_icd10, 
                                                         "icd10", "icd9")]]){
      if (grepl(regex_str, icdCode, ignore.case=TRUE)){
        out[key_disease] <- TRUE
        break;
      }
    }
  }
  
  return(out)
}
