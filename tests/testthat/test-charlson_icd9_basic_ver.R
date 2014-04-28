charlsons_icd9_codes_2_test = list()

# Myocardial Infarction
charlsons_icd9_codes_2_test[['MI']] = c('410','41091', '412')

# Congestive Heart Failure
charlsons_icd9_codes_2_test[['CHF']] = c('428', '4282', '4281')

# Periphral Vascular Disease
charlsons_icd9_codes_2_test[['PVD']] = c('4439','441','4419','7854','V434')

# Cerebrovascular Disease
charlsons_icd9_codes_2_test[['CVD']] = c('430','431','432','433','433',
                                         '434','435','436','437','43813')

# Dementia
charlsons_icd9_codes_2_test[['DEMENTIA']] = c('290', '2901', '2909')

# Chronic Pulmonary Disease
charlsons_icd9_codes_2_test[['COPD']] = c('490','491','492','4922',
                                          '493','494','495','496','500',
                                          '501','502','503','504','505',
                                          '5064')

# Connective Tissue Disease-Rheumatic Disease
charlsons_icd9_codes_2_test[['RHEUM']] = c('7100','7101','7104',
                                           '7140','7141','7142',
                                           '71481','725')

# Peptic Ulcer Disease
charlsons_icd9_codes_2_test[['PUD']] = c('531','532','533','534')

# Mild Liver Disease
charlsons_icd9_codes_2_test[['MILD.LIVER']] = c('5712','5715','5716','5714')

# Diabetes without complications
charlsons_icd9_codes_2_test['DM'] = c('2500','2501','2502','2503','2507')

# Diabetes with complications
charlsons_icd9_codes_2_test['DM.COMP'] = c('2504','2505','2506')

# Paraplegia and Hemiplegia
charlsons_icd9_codes_2_test[['PLEGIA']] = c('342','3421','3429')

# Renal Disease
charlsons_icd9_codes_2_test[['RENAL']] = c('582','5825','5829',
                                           '5830','5831','5832','5834','5836','5837',
                                           '585','586','5880','5889')

# Cancer
charlsons_icd9_codes_2_test[['MALIGNANCY']] = c('140','141','142','143','144','145','146','147','148','149',
                                                '150','151','152','153','154','155','156','157','158','159',
                                                '160','161','162','163','164','165',
                                                '170','171','172','174','175','176','179',
                                                '180','181','182','183','184','185','186','187','188','189',
                                                '190','191','192','193','194','195',
                                                '200','201','202','203','204','205','206','207','208')

# Moderate or Severe Liver Disease
charlsons_icd9_codes_2_test[['SEVERE.LIVER']] = c('4560','4561','4562',
                                                  '5722','5723','5724',
                                                  '5728')

# Metastatic Carcinoma
charlsons_icd9_codes_2_test[['METS']] = c('196','197','198','199')

# AIDS/HIV
charlsons_icd9_codes_2_test[['HIV']] = c('042','043','044')

test_that("Check Charlson matches",{
  for (n in names(charlsons_icd9_codes_2_test)){
    test_df <- data.frame(Codes=charlsons_icd9_codes_2_test[[n]])
    out <- deyo(test_df)
    found_codes <- out$COMORBIDITIES[,n] == 1
    expect_true(all(found_codes), info=sprintf("The script fails to properly identify from '%s' the codes '%s'",
                                               n,
                                               paste(charlsons_icd9_codes_2_test[[n]][!found_codes],
                                                     collapse="', '")))
  }
})

