ahrq.list <- list()
# Congestive heart failure 
ahrq.list[["CHF"]] <- c("39891","4280 ","4289 ", "40211", "40491", "40493")


# Valvular disease 
ahrq.list[["VALVE"]] <- c("09320","09324","3940 ","3971 ","3979 ","4240 ","42499","7463 ","7466 ","V422 ","V433 ")


# Pulmonary circulation disorder 
ahrq.list[["PULM.CIRC"]] <- c("41511","41519","4160 ","4169 ","4179 ")


# Peripheral vascular disorder 
ahrq.list[["PVD"]] <- c("4400 ","4409 ","44100","4419 ","4420 ","4429 ","4431 ","4439 ","44421","44422","4471 ","449  ","5571 ","5579 ","V434 ")


# Hypertension, uncomplicated 
ahrq.list[["HTN.UNCOMP"]] <- c("4011 ","4019 ","64200","64204")

# Hypertension, complicated 
ahrq.list[["HTN.COMP"]] <- c("4010 ","4372 ","64220","64224","40200","40210","40290",  "40509","40519","40599","40201","40211","40291","40300","40310","40390","40501","40511","40591","64210","64214","40301","40311","40391","40400","40410","40490","40401","40411","40491","40402","40412","40492","40403","40413","40493","64270","64274","64290","64294")

#########################
# End Temporary Formats #
#########################


# Paralysis 
ahrq.list[["PARALYSIS"]] <- c("3420 ","3449 ","43820","43853","78072")


# Other neurological 
ahrq.list[["NEURO.OTHER"]] <- c("3300 ","3319 ","3320 ","3334 ","3335 ","3337 ","33371","33372","33379","33385","33394","3340 ","3359 ","3380 ","340  ","3411 ","3419 ","34500","34511","3452 ","3453 ","34540","34591","34700","34701","34710","34711","64940","64944","7687 ","76870","76873","7803 ","78031","78032","78039","78097","7843 ")


# Chronic pulmonary disease 
ahrq.list[["CHRONIC.PULM"]] <- c("490  ","4928 ","49300","49392","494  ","4941 ","4950 ","505  ","5064 ")


# Diabetes w/o chronic complications
ahrq.list[["DM.UNCOMP"]] <- c("25000","25033","64800","64804","24900","24931")


# Diabetes w/ chronic complications 
ahrq.list[["DM.COMP"]] <- c("25040","25093","7751 ","24940","24991")


# Hypothyroidism 
ahrq.list[["HYPOTHYROID"]] <- c("243  ","2442 ","2448 ","2449 ")


# Renal failure 
ahrq.list[["RENAL"]] <- c("5853 ","5854 ","5855 ","5856 ","5859 ","586  ","V420 ","V451 ","V560 ","V5632","V568 ","V4511","V4512", "40493", "40403", "40301")


# Liver disease 
ahrq.list[["LIVER"]] <- c("07022","07023","07032","07033","07044","07054","4560 ","4561 ","45620","45621","5710 ","5712 ","5713 ","57140","57149","5715 ","5716 ","5718 ","5719 ","5723 ","5728 ","V427 ")


# Chronic Peptic ulcer disease (includes bleeding only if obstruction is also present) 
ahrq.list[["PUD"]] <- c("53141","53151","53161","53170","53171","53191","53241","53251","53261","53270","53271","53291","53341","53351","53361","53370","53371","53391","53441","53451","53461","53470","53471","53491")


# HIV and AIDS 
ahrq.list[["HIV"]] <- c("042  ","0449 ")


# Lymphoma 
ahrq.list[["LYMPHOMA"]] <- c("20000","20238","20250","20301","2386 ","2733 ","20302","20382")


# Metastatic cancer 
ahrq.list[["METS"]] <- c("1960 ","1991 ","20970","20975","20979","78951")


# Solid tumor without metastasis 
ahrq.list[["SOLID.TUMOR"]] <- c("1400 ","1729 ","1740 ","1759 ","179  ","1958 ","20900","20924","20925","2093 ","20931","20936",   "25801","25803")


# Rheumatoid arthritis/collagen vascular diseases 
ahrq.list[["RHEUM"]] <- c("7010 ","7100 ","7109 ","7140 ","7149 ","7200 ","7209 ","725  ")


# Coagulation deficiency - note:  this comorbidity should be dropped when used with the AHRQ Patient Safety Indicators 
ahrq.list[["COAG"]] <- c("2860 ","2869 ","2871 ","2873 ","2875 ","64930","64934","28984")


# Obesity      
ahrq.list[["OBESITY"]] <- c("2780 ","27800","27801","64910","64914","V8530","V8539","V854 ","V8554","79391")


# Weight loss 
ahrq.list[["WT.LOSS"]] <- c("260  ","2639 ","78321","78322")


# Fluid and electrolyte disorders - note:  this comorbidity should be dropped when used with the AHRQ Patient Safety Indicators
ahrq.list[["LYTES"]] <- c("2760 ","2769 ")

# Blood loss anemia 
ahrq.list[["ANEMIA.LOSS"]] <- c("2800 ","64820","64824")


# Deficiency anemias 
ahrq.list[["ANEMIA.DEF"]] <- c("2801 ","2819 ","28521","28529","2859 ")


# Alcohol abuse 
ahrq.list[["ETOH"]] <- c("2910 ","2913 ","2915 ","2918 ","29181","29182","29189","2919 ","30300","30393","30500","30503")


# Drug abuse 
ahrq.list[["DRUGS"]] <- c("2920 ","29282","29289","2929 ","30400","30493","30520","30593","64830","64834")


# Psychoses 
ahrq.list[["PSYCHOSES"]] <- c("29500","2989 ","29910","29911")


# Depression 
ahrq.list[["DEPRESSION"]] <- c("3004 ","30112","3090 ","3091 ","311  ")

test_that("Check AHRQ matches codes from the SAS-script",{
  for (n in names(ahrq.list)){
    codes <- ahrq.list[[n]]
    codes <- gsub(" ", "0", codes)
    # Check one code at the time
    out <- t(sapply(codes,
                    function(code)
                      cmrbdt.finder.numeric.ahrq_2010v3.5(icd_codes=code)))
    found_codes <- out[,n]
    expect_true(all(found_codes), 
                info=sprintf("The script fails to properly identify from '%s' the codes '%s'",
                             n,
                             paste(codes[!found_codes],
                                   collapse="', '")))
  }
})
