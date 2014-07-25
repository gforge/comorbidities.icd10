charlsons_icd9_codes_2_test = list()

# Myocardial Infarction
charlsons_icd9_codes_2_test[['MI']] = c('410','412')

# Congestive Heart Failure
charlsons_icd9_codes_2_test[['CHF']] = c('39891','40201','40211','40291','40401','40403','40411','40413','40491','40493','4254','4255','4257','4258','4259','428')

# Periphral Vascular Disease
charlsons_icd9_codes_2_test[['PVD']] = c('0930','4373','440','441','4431','4432','4438','4439','4471','5571','5579','V434')

# Cerebrovascular Disease
charlsons_icd9_codes_2_test[['CVD']] = c('36234','430','431','432','433','434','435','436','437','438')

# Dementia
charlsons_icd9_codes_2_test[['DEMENTIA']] = c('290','2941','3312')

# Chronic Pulmonary Disease
charlsons_icd9_codes_2_test[['COPD']] = c('4168','4169','490','491','492','493','494','495','496','500','501','502','503','504','505','5064','5081','5088')

# Connective Tissue Disease-Rheumatic Disease
charlsons_icd9_codes_2_test[['RHEUM']] = c('4465','7100','7101','7102','7103','7104','7140','7141','7142','7148','725')

# Peptic Ulcer Disease
charlsons_icd9_codes_2_test[['PUD']] = c('531','532','533','534')

# Mild Liver Disease
charlsons_icd9_codes_2_test[['MILD.LIVER']] = c('07022','07023','07032','07033','07044','07054','0706','0709','570','571','5733','5734','5738','5739','V427')

# Diabetes without complications
charlsons_icd9_codes_2_test[['DM']] = c('2500','2501','2502','2503','2508','2509')

# Diabetes with complications
charlsons_icd9_codes_2_test[['DM.COMP']] = c('2504','2505','2506','2507')

# Paraplegia and Hemiplegia
charlsons_icd9_codes_2_test[['PLEGIA']] = c('3341','342','343','3440','3441','3442','3443','3444','3445','3446','3449')

# Renal Disease
charlsons_icd9_codes_2_test[['RENAL']] = c('40301','40311','40391','40402','40403','40412','40413','40492','40493','582','5830','5831','5832','5834','5836','5837','585','586','5880','V420','V451','V56')

# Cancer
charlsons_icd9_codes_2_test[['MALIGNANCY']] = c('140','141','142','143','144','145','146','147','148','149','150','151','152','153','154','155','156','157','158','159','160','161','162','163','164','165','170','171','172','174','175','176','179','180','181','182','183','184','185','186','187','188','189','190','191','192','193','194','195','200','201','202','203','204','205','206','207','208','2386')

# Moderate or Severe Liver Disease
charlsons_icd9_codes_2_test[['SEVERE.LIVER']] = c('4560','4561','4562','5722','5723','5724','5728')

# Metastatic Carcinoma
charlsons_icd9_codes_2_test[['METASTASIS']] = c('196','197','198','199')

# AIDS/HIV
charlsons_icd9_codes_2_test[['HIV']] = c('042','043','044')

test_that("Check Charlson matches to the regular expression Quan version",{
  for (n in names(charlsons_icd9_codes_2_test)){
    codes <- charlsons_icd9_codes_2_test[[n]]
    # Check one code at the time
    out <- t(sapply(codes,
                    function(code)
                      cmrbdt.finder.regex.charlson_Quan2005(icd_codes=code,
                                                            country_code="US",
                                                            icd_ver=9)))
    found_codes <- out[,n]
    expect_true(all(found_codes),
                info=sprintf("The script fails to properly identify ICD-%s from '%s' the codes '%s'",
                             9,
                             n,
                             paste(codes[!found_codes],
                                   collapse="', '")))
  }
})
