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
charlsons_icd9_codes_2_test[['METASTASIS']] = c('196','197','198','199')

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
    
    # Should not match other groups
    not_found_codes <- rowSums(out$COMORBIDITIES[,-grep(n, 
                                                        colnames(out),
                                                        fixed=TRUE)]) == 0
    expect_true(all(not_found_codes), 
                info=sprintf("The script matches codes to other groups than '%s' for the codes '%s'",
                             n,
                             paste(charlsons_icd9_codes_2_test[[n]][!not_found_codes],
                                   collapse="', '")))
  }
})


org_charlson_weights <-
  list(
    `2`= c('PLEGIA','RENAL','DM.COMP','MALIGNANCY'),
    `3`= c('SEVERE.LIVER'),
    `6`=c('METASTASIS','HIV'))

test_that("Check Charlson individual scores",{
  for (n in names(charlsons_icd9_codes_2_test)){
    test_df <- data.frame(Codes=charlsons_icd9_codes_2_test[[n]])
    out <- deyo(test_df)
    score_points <- 1
    for (group_points in names(org_charlson_weights)){
      if (n %in% org_charlson_weights[[group_points]]){
        score_points <- as.numeric(group_points)
        break;
      }
    }
    expect_true(all(out$COMORBIDITIES.POINTS[,n] == score_points), 
                label="Comorbidity weight test",
                info=sprintf("Comorbity weight for %s is expected to be %d while it actually is %s",
                             n,
                             score_points,
                             paste(unique(out$COMORBIDITIES.POINTS[,n]), collapse=",")))

  }
})

test_that("Check Charlson sum score",{
  t1 <- data.frame(icd1=charlsons_icd9_codes_2_test$MI[1],
                   icd2=charlsons_icd9_codes_2_test$CHF[1],
                   icd3=charlsons_icd9_codes_2_test$PUD[1])
  out <- deyo(t1)
  expect_equal(out$CHARLSON.SCORE, 1+1+1,
               label="3 same weights merged")

  t2 <- data.frame(icd1=charlsons_icd9_codes_2_test$PVD[1],
                   icd2=charlsons_icd9_codes_2_test$DEMENTIA[1],
                   icd3=charlsons_icd9_codes_2_test$METASTASIS[1],
                   icd4=charlsons_icd9_codes_2_test$HIV[1])
  out <- deyo(t2)
  expect_equal(out$CHARLSON.SCORE, 1+1+6+6, 
               label="4 different weights merged")
  
  t3 <- data.frame(icd1=charlsons_icd9_codes_2_test$PVD[1],
                   icd2=charlsons_icd9_codes_2_test$DEMENTIA[1],
                   icd3=charlsons_icd9_codes_2_test$METASTASIS[1],
                   icd4=NA)
  out <- deyo(t3)
  expect_equal(out$CHARLSON.SCORE, 1+1+6, 
               label="3 different weights and one missing merged")

  t4 <- data.frame(icd1=charlsons_icd9_codes_2_test$PVD[1],
                   icd2=charlsons_icd9_codes_2_test$DEMENTIA[1],
                   icd3=charlsons_icd9_codes_2_test$METASTASIS[1],
                   icd4=NA)
  out <- deyo(t4)
  expect_equal(out$CHARLSON.SCORE, 1+1+6, 
               label="3 different weights and one missing merged")

  t5 <- data.frame(icd1=charlsons_icd9_codes_2_test$PVD[1],
                   icd2=charlsons_icd9_codes_2_test$DEMENTIA[1],
                   icd3=charlsons_icd9_codes_2_test$METASTASIS[1],
                   icd4=NA,
                   icd5="715.96")
  out <- deyo(t5)
  expect_equal(out$CHARLSON.SCORE, 1+1+6, 
               label="3 different weights, one negative and one missing merged")
})


neg.icd9_codes <- 
  as.character(
    c(128.9, # 	HELMINTH INFECTION UNSPECIFIED
      136.21, # 	SPECIFIC INFECTION DUE TO ACANTHAMOEBA
      136.29, # 	OTHER SPECIFIC INFECTIONS BY FREE-LIVING AMEBAE
      288.04, # 	NEUTROPENIA DUE TO INFECTION
      323.41, # 	OTHER ENCEPHALITIS AND ENCEPHALOMYELITIS DUE TO OTHER INFECTIONS CLASSIFIED ELSEWHERE
      323.42, # 	OTHER MYELITIS DUE TO OTHER INFECTIONS CLASSIFIED ELSEWHERE
      326, #	LATE EFFECTS OF INTRACRANIAL ABSCESS OR PYOGENIC INFECTION
      379.60, # 	INFLAMMATION (INFECTION) OF POSTPROCEDURAL BLEB, UNSPECIFIED
      379.61, # 	INFLAMMATION (INFECTION) OF POSTPROCEDURAL BLEB, STAGE 1
      379.62, # 	INFLAMMATION (INFECTION) OF POSTPROCEDURAL BLEB, STAGE 2
      379.63, # 	INFLAMMATION (INFECTION) OF POSTPROCEDURAL BLEB, STAGE 3
      380.11, # 	ACUTE INFECTION OF PINNA
      380.13, # 	OTHER ACUTE INFECTIONS OF EXTERNAL EAR
      465.8, # 	ACUTE UPPER RESPIRATORY INFECTIONS OF OTHER MULTIPLE SITES
      465.9, # 	ACUTE UPPER RESPIRATORY INFECTIONS OF UNSPECIFIED SITE
      519.01, # 	INFECTION OF TRACHEOSTOMY
      733.93, # STRESS FRACTURE OF TIBIA OR FIBULA
      733.94, # STRESS FRACTURE OF THE METATARSALS
      733.95, # STRESS FRACTURE OF OTHER BONE
      733.96, # STRESS FRACTURE OF FEMORAL NECK
      806.9, # OPEN FRACTURE OF UNSPECIFIED VERTEBRA WITH SPINAL CORD INJURY
      807.00, # CLOSED FRACTURE OF RIB(S) UNSPECIFIED
      807.01, # CLOSED FRACTURE OF ONE RIB
      807.02, # CLOSED FRACTURE OF TWO RIBS
      807.03, # CLOSED FRACTURE OF THREE RIBS
      807.04, # CLOSED FRACTURE OF FOUR RIBS
      808.43, # MULTIPLE CLOSED PELVIC FRACTURES WITH DISRUPTION OF PELVIC CIRCLE
      808.44, # MULTIPLE CLOSED PELVIC FRACTURES WITHOUT DISRUPTION OF PELVIC CIRCLE
      808.49, # CLOSED FRACTURE OF OTHER SPECIFIED PART OF PELVIS
      808.51, # OPEN FRACTURE OF ILIUM
      808.52, # OPEN FRACTURE OF ISCHIUM
      808.53, # MULTIPLE OPEN PELVIC FRACTURES WITH DISRUPTION OF PELVIC CIRCLE
      808.54, # MULTIPLE OPEN PELVIC FRACTURES WITHOUT DISRUPTION OF PELVIC CIRCLE
      808.59, # OPEN FRACTURE OF OTHER SPECIFIED PART OF PELVIS
      808.8, # UNSPECIFIED CLOSED FRACTURE OF PELVIS
      808.9, # UNSPECIFIED OPEN FRACTURE OF PELVIS
      809.0, # FRACTURE OF BONES OF TRUNK CLOSED
      809.1, # FRACTURE OF BONES OF TRUNK OPEN
      810.00, # CLOSED FRACTURE OF CLAVICLE UNSPECIFIED PART
      810.01, # CLOSED FRACTURE OF STERNAL END OF CLAVICLE
      810.02, # CLOSED FRACTURE OF SHAFT OF CLAVICLE
      812.10, # FRACTURE OF UNSPECIFIED PART OF UPPER END OF HUMERUS OPEN
      812.11, # FRACTURE OF SURGICAL NECK OF HUMERUS OPEN
      812.12, # FRACTURE OF ANATOMICAL NECK OF HUMERUS OPEN
      812.13, # FRACTURE OF GREATER TUBEROSITY OF HUMERUS OPEN
      812.19, # OTHER OPEN FRACTURE OF UPPER END OF HUMERUS
      812.20, # FRACTURE OF UNSPECIFIED PART OF HUMERUS CLOSED
      812.21, # FRACTURE OF SHAFT OF HUMERUS CLOSED
      812.30, # FRACTURE OF UNSPECIFIED PART OF HUMERUS OPEN
      812.31, # FRACTURE OF SHAFT OF HUMERUS OPEN
      715.96, # Osteoarthrosis, unspecified whether generalized or localized, lower leg
      NA))
    
test_that("Check Charlson handling of negative scores",{
  test_df <- data.frame(Codes=neg.icd9_codes)
  out <- deyo(test_df)
  expect_true(all(out$COMORBIDITIES.POINTS == 0), 
              label="Identified comorbidities that should be negative",
              info=sprintf("Comorbity code(s) identified: %s",
                           paste(neg.icd9_codes[rowSums(out$COMORBIDITIES.POINTS) > 0],
                                 collapse=",")))
})
