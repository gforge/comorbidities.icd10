#' @rdname cmrbdt.finder.regex
#' @references H. Quan, V. Sundararajan, P. Halfon, A. Fong, B. Burnand, J.-C. Luthi,
#' L. D. Saunders, C. A. Beck, T. E. Feasby, and W. A. Ghali, "Coding algorithms for
#' defining comorbidities in ICD-9-CM and ICD-10 administrative data" Med Care,
#' vol. 43, no. 11, pp. 1130-1139, Nov. 2005. - Charlson section
#' @export
#' @examples
#' cmrbdt.finder.regex.charlson_Quan2005("I212")
cmrbdt.finder.regex.charlson_Quan2005 <-
  local({
    # Based on Quan et al 2005
    charlsons_v2 <- list(US=list())
    charlsons_v2$US[['MI']] <-
      list(icd10 = c('^I2([12]|52)'),
           icd9 = c('^41[02]'))

    charlsons_v2$US[['CHF']] <-
      list(icd10 = c('^I099', '^I1(10|3[02])', '^I255', '^I4(2[056789]|3)', '^I50', '^P290'),
           icd9 =  c('^39891', '^402(01|11|91)', '^404(01|03|[19][13])', '^42(5[456789]|8)'))

    charlsons_v2$US[['PVD']] <-
      list(icd10 = c('^I7([01]|3[189]|71|9[02])', '^K55[189]', '^Z95[89]'),
           icd9 = c('^0930', '^4373', '^44([01]|3[123456789]|71)', '^557[19]', '^V434'))

    charlsons_v2$US[['CVD']] <-
      list(icd10 = c('^G4[56]', '^H340', '^I6'),
           icd9 = c('^36234', '^43[012345678]'))

    charlsons_v2$US[['DEMENTIA']] <-
      list(icd10 = c('^F0([0123]|51)', '^G3(0|11)'),
           icd9 = c('^29(0|41)', '^3312'))

    charlsons_v2$US[['COPD']] <-
      list(icd10 = c('^I27[89]', '^J4[01234567]', '^J6([01234567]|84)', '^J70[13]'),
           icd9 = c('^416[89]', '^49', '^50([012345]|64|8[18])'))

    charlsons_v2$US[['RHEUM']] <-
      list(icd10 = c('^M0[56]', '^M3(15|[234]|5[13]|60)'),
           icd9 = c('^4465', '^71(0[01234]|4[0128])', '^725'))

    charlsons_v2$US[['PUD']] <-
      list(icd10 = c('^K2[5678]'),
           icd9 = c('^53[1234]'))

    charlsons_v2$US[['MILD.LIVER']] <-
      list(icd10 = c('^B18', '^K7(0[01239]|1[3457]|[34]|6[023489])', '^Z944'),
           icd9 = c('^070([23][23]|[45]4|[69])', '^57([01]|3[3489])', '^V427'))

    charlsons_v2$US[['DM']] <-
      list(icd10 = c('^E1[01234][01689]'),
           icd9 = c('^250[012389]'))

    charlsons_v2$US[['DM.COMP']] <-
      list(icd10 = c('^E1[01234][23457]'),
           icd9 = c('^250[4567]'))

    charlsons_v2$US[['PLEGIA']] <-
      list(icd10 = c('^G041', '^G114', '^G8(0[12]|[12]|3[012349])'),
           icd9 = c('^3341', '^34([23]|4[01234569])'))

    charlsons_v2$US[['RENAL']] <-
      list(icd10 = c('^I120', '^I131', '^N0(3[234567]|5[234567])', '^N1[89]', '^N250', '^Z49[012]', '^Z940', '^Z992'),
           icd9 = c('^403[019]1', '^404[019][23]', '^58(2|3[01234567]|[56]|80)', '^V4(20|51)', '^V56'))

    charlsons_v2$US[['MALIGNANCY']] <-
      list(icd10 = c('^C[01]', '^C2[0123456]', '^C3[01234789]', '^C4[01356789]', '^C5[012345678]', '^C6', '^C7[0123456]', '^C8[123458]', '^C9[01234567]'),
           icd9 = c('^1[456]', '^17[012456789]', '^18', '^19[012345]', '^20[012345678]', '^2386'))

    charlsons_v2$US[['SEVERE.LIVER']] <-
      list(icd10 = c('^I8(5[09]|64)', '^I982', '^K7(04|[12]1|29|6[567])'),
           icd9 = c('^456[012]', '^572[2345678]'))

    charlsons_v2$US[['METASTASIS']] <-
      list(icd10 = c('^C7[789]', '^C80'),
           icd9 = c( '^19[6789]'))

    charlsons_v2$US[['HIV']] <-
      list(icd10 = c('^B2[0124]'),
           icd9 = c('^04[234]'))

    charlsons_v2$SE <- charlsons_v2$US

    # MI - no change

    charlsons_v2$SE$CHF$icd9 <-
      c(# Not with heart failure in Swedish system
        #'^3989',
        # '^402(01|11|91)',
        # '^404(01|03|[19][13])',
        '^42(5[456789]|8)')

    # PVD - no change

    charlsons_v2$SE$CVD$icd9 <-
      c('^36234', '^43[012345678]')

    charlsons_v2$SE$DEMENTIA$icd9 <-
      c('^29(0|41)', '^3312')

    # DEM - no change

    charlsons_v2$SE$COPD$icd9 <-
      c('^416[89]', '^49', '^50([012345]|64|81)')

    # RHEUM - no change

    # PUD - no change

    # LD - no change

    # DM - no change
    # DM.COMP - no change

    # PLEGIA - no change

    charlsons_v2$SE$MILD.LIVER$icd9 <-
      c('^070[23456]', '^57([01]|3[3489])', '^V427')

    charlsons_v2$SE$RENAL$icd9 <-
      c('^403[019]',
        '^404[019]',
        '^58(2|3[01234567]|[56]|80)', '^V4(20|51)', '^V56')

    charlsons_v2$SE$MALIGNANCY$icd9 <-
      c('^1[456]', '^17[012456789]', '^18',
        '^19[012345]', '^20[012345678]', '^2386')

    # SEVERE.LIVER - no change

    charlsons_v2$SE$METASTASIS$icd9 <-
      c('^19([6789])')

    charlsons_v2$SE$HIV$icd9 <-
      c('279K')

    acute_icd_codes <- list(icd10= '^(I2[123]|J46|N17[12]|N19)',
                        # Used the translator from the Swedish National Board of Healthe and Welfare (Socialstyrelsen)
                        icd9 = '^(410|42(30|95|96|98)|4939|58[34][67]|5908|586|7919|5939)')


    function(icd_codes,
             out,
             country_code = 'US',
             include_acute = rep(TRUE, times=length(icd_codes)),
             icd_ver = rep(FALSE, times=length(icd_codes))){

      # Speeds up only to check the codes that are possible
      icd_ver <- pr.get.icd.ver(icd_codes, icd_ver)

      # Check that the country code is available
      if (!country_code %in% names(charlsons_v2))
        stop("The requested country code '", country_code ,"''",
             " is not yet available for Charlsons.",
             " The only country codes available are:",
             " '", paste(names(charlsons_v2), collapse="', '", "'"))

      # Get a correctly formatted output vector
      out <- pr.get.out.vector(out, charlsons_v2[[country_code]])

      # Do the actual test loop
      out <- pr.regex.code.match(out = out,
                                 icd_codes = icd_codes,
                                 cmrbdt.finder.regex.comorbidity = charlsons_v2[[country_code]],
                                 icd_ver =  icd_ver,
                                 include_acute = include_acute,
                                 cmrbdt.finder.regex.acute = acute_icd_codes)

      return(out)
    }
  })


#' @rdname cmrbdt.finder.regex
#' @references V. Sundararajan, T. Henderson, C. Perry, A. Muggivan,
#' H. Quan, and W. A. Ghali, "New ICD-10 version of the Charlson
#' comorbidity index predicted in-hospital mortality" J Clin Epidemiol,
#' vol. 57, no. 12, pp. 1288-1294, Dec. 2004.
#' @export
#' @examples
#' cmrbdt.finder.regex.charlson_Sundarajan2004(c("430", "J40"))
cmrbdt.finder.regex.charlson_Sundarajan2004 <-
  local({
    # Create the charlsons regular expressions
    # to compare 2
    # v1 is based on Sundarajan et al 2004
    charlsons_v1 <- list(US = list())
    charlsons_v1$US[['MI']] <-
      list(icd9 = c('^41[02]'),
           icd10 = c('^I2([12]|52)'))

    charlsons_v1$US[['CHF']] <-
      list(icd9 = c('^428'),
           icd10 = c('^I50'))

    charlsons_v1$US[['PVD']] <-
      list(icd9 = c('^44(1|39)', '^7854', '^V434'),
           icd10 = c('^I7(1|90|39)', '^R02', '^Z95[89]'))

    charlsons_v1$US[['CVD']] <-
      list(icd9 = c('^43[012345678]'),
           icd10 = c('^I6([01234569]|7[012456789]|8[128])', '^G45[0124689]', '^G46'))

    charlsons_v1$US[['DEMENTIA']] <-
      list(icd9 = c('^290'), icd10 = c('^F0([012]|51)'))

    charlsons_v1$US[['COPD']] <-
      list(icd9 = c('^49[0123456]', '^50[012345]'),
           icd10 = c('^J4[01234567]', '^J6[01234567]'))

    charlsons_v1$US[['RHEUM']] <-
      list(icd9 = c('^710[014]', '^714[012]', '^71481', '^5171', '^725'),
           icd10 = c('^M3([24]|32|53)', '^M0(5[012389]|6[039])'))

    charlsons_v1$US[['PUD']] <-
      list(icd9 = c('^53[1234]'),
           icd10 = c('^K2[5678]'))

    charlsons_v1$US[['MILD.LIVER']] <-
      list(icd9 = c('^571[2456]'),
           icd10 = c('^K7(0[23]|17|3|4[023456])'))

    charlsons_v1$US[['DM']] <-
      list(icd9 = c('^250[01237]'),
           icd10 = c('^E1[0134][159]'))

    charlsons_v1$US[['DM.COMP']] <-
      list(icd9 = c('^250[456]'),
           icd10 = c('^E1[0134][234]'))

    charlsons_v1$US[['PLEGIA']] <-
      list(icd9 = c('^342', '^3441'),
           icd10 = c('^G041', '^G8(1|2[012])'))

    charlsons_v1$US[['RENAL']] <-
      list(icd9 = c('^58([2568]|3[01234567])'),
           icd10 = c('^N0([13]|5[23456]|7[234])', '^N1[89]', '^N25'))

    charlsons_v1$US[['MALIGNANCY']] <-
      list(icd9 = c('^1([4568]|7[0124569]|9[012345])', '^20[012345678]'),
           icd10 = c('^C[012356]', '^C4[01356789]', '^C7[0123456]', '^C8([12345]|8[379])', '^C9(0[01]|[12356]|4([01237]|51))'))

    charlsons_v1$US[['METASTASIS']] <-
      list(icd9 = c('^19[6789]'),
           icd10 = c('^C7[789]', '^C80'))

    charlsons_v1$US[['SEVERE.LIVER']] <-
      list(icd9 = c('^572[2348]'),
           icd10 = c('^K7(04|2[19]|6[67])'))

    charlsons_v1$US[['HIV']] <-
      list(icd9 = c('^04[234]'),
           icd10 = c('^B2[01234]'))

    # Set the Swedish version
    charlsons_v1$SE <- charlsons_v1$US

    # MI - no change

    # CHF - no change

    # PVD - no change

    # CVD - no change

    # DEMENTIA - no change

    # COPD - no change

    charlsons_v1$SE$RHEUM$icd9 <-
      c('^710[014]', '^714[0128]', '^5171', '^725')

    # PUD - no change

    # MILD.LIVER - no change

    # DM - no change

    # DM.COMP - no change

    # PLEGIA - no change

    # RENAL - no change

    charlsons_v1$SE$MALIGNANCY$icd9
    c('^1([4568]|7[0124569]|9[012345])', '^20[012345678]')

    charlsons_v1$SE$METASTASIS$icd9 <-
      c('^19([6789])')

    # SEVERE.LIVER - no change

    charlsons_v1$SE$HIV$icd9 <-
      c('279K')


    acute_icd_codes <- list(icd10= '^(I2[123]|J46|N17[12]|N19)',
                            # Used the translator from the Swedish National Board of Healthe and Welfare (Socialstyrelsen)
                            icd9 = '^(410|42(30|95|96|98)|4939|58[34][67]|5908|586|7919|5939)')

    function(icd_codes,
             out,
             country_code = 'US',
             include_acute = rep(TRUE, times=length(icd_codes)),
             icd_ver = rep(FALSE, times=length(icd_codes))){

      # Speeds up only to check the codes that are possible
      icd_ver <- pr.get.icd.ver(icd_codes, icd_ver)

      # Check that the country code is available
      if (!country_code %in% names(charlsons_v1))
        stop("The requested country code '", country_code ,"''",
             " is not yet available for Charlsons.",
             " The only country codes available are:",
             " '", paste(names(charlsons_v1), collapse="', '", "'"))

      # Get a correctly formatted output vector
      out <- pr.get.out.vector(out, charlsons_v1[[country_code]])

      # Do the actual test loop
      out <- pr.regex.code.match(out = out,
                                 icd_codes = icd_codes,
                                 cmrbdt.finder.regex.comorbidity = charlsons_v1[[country_code]],
                                 icd_ver =  icd_ver,
                                 include_acute = include_acute,
                                 cmrbdt.finder.regex.acute = acute_icd_codes)

      return(out)
    }
  })
