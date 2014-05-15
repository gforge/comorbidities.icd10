elixhausers_codes_2_check = list()
#Congestive heart failure
elixhausers_codes_2_check[['CHF']] = list(
  icd9_basic = c('39891', '40211', 
                 '40291', '40411', '40413',
                 '40491', '40493', '4283'),
  
  icd9 = c('39891', '40201', '40211',
           '40291', '40401', '4283'),
  
  icd9_SE = c('3980', '4020', '4021',
           '4029', '4040', '4283'),
  
  icd10 = c('I099', 'I110', 'I130', 'I132',
            'I255', 'I420', 'I425', 'I4256', 'I426', 'I427', 'I429', 'I4344',
            'I5023',
            'P290'))

# Cardiac arrhythmias
elixhausers_codes_2_check[['ARRHYTHMIA']] = list(
  icd9_basic = c('42610', '42611', '42613','42627', '42660', '42689',
                 '4270', '4272', '42731', 
                 '42760', '4279', '7850',
                 'V450', 'V533'),
  
  icd9 = c('4260', '4267', '4269',
           '4270', '4271', '4272', '4273', '4274',
           '4276', '4277', '4278', '4279', '7850',
           'V450', 'V533'),
  
  icd10 = c('I441', 'I442', 'I443', 'I456', 'I459',
             'I47', 'I489', 'I49', 'R000',
             'R001', 'R008', 'T821',
             'Z450', 'Z950'))

# Valvular disease
elixhausers_codes_2_check[['VALVE']] = list(
  icd9_basic = c('0932', '394', '3955', '396', '397', '4242',
                 '7463', '7464', '7465', '7466', 'V422', 'V433'),
  
  icd9 = c('0932', '394', '3955', '396', '397', '4242',
           '7463', '7464', '7465', '7466', 'V422', 'V433'),

  icd10 = c('A520', 'I053', 'I06', 'I07', 'I08x', 'I091',
             'I098', 'I34', 'I35', 'I36', 'I377', 'I38', 'I39x',
             'Q230', 'Q231', 'Q232', 'Q233', 'Z952', 'Z953', 'Z954'))

# Pulmonary circulation disorders
elixhausers_codes_2_check[['PULM.CIRC']] = list(
  icd9_basic = c('4160', '4162', '4179'),

  icd9 = c('4150', '4151', '416x', '4170',
           '4178', '4179'),
  
  icd10 = c('I263', 'I272', 'I280', 'I288',
            'I289'))

# Peripheral vascular disorders
elixhausers_codes_2_check[['PVD']] = list(
  icd9_basic = c('4402', '4414',
                 '4431', '4432', '4433', '4434',
                 '4435', '4436', '4437', '4438', '4439', '4471', '5571',
                 '5579', 'V434'),

  icd9 = c('0930', '4373', '4402', '4414',
           '4431', '4432', '4433', '4434', '4435', '4436', '4437', '4438', '4439', '4471', '5571',
           '5579', 'V434'),
  
  icd10 = c('I704', 'I716', 'I731', 'I738',
             'I739', 'I771', 'I790',
             'I792', 'K551', 'K558',
             'K559', 'Z958', 'Z959'))

# Hypertension, uncomplicated
elixhausers_codes_2_check[['HTN.UNCOMP']] = list(
  icd9_basic = c('4011', '4019'),
  
  icd9 = c('4013'),
  
  icd10 = c('I104'))

# Hypertension, complicated
elixhausers_codes_2_check[['HTN.COMP']] = list(
  icd9_basic = c('40210', '40290', 
                 '40410', '40490', 
                 '40591','40599'),
  
  icd9 = c('4022', '4034', '404', '4058'),
  
  icd10 = c('I11', 'I123', 'I135', 'I159'))

# Paralysis
elixhausers_codes_2_check[['PARALYSIS']] = list(
  icd9_basic = c('3420', '34212', '3435',
                 '3440', '3441', '3442', 
                 '3443', '3444', '3445', '3446', '3449'),
  
  icd9 = c('3341', '3423', '3435',
           '3440', '3441', '3442', '3443', '3444', '3445', '3446', '3449'),
  
  icd10 = c('G041', 'G114', 'G801',
             'G802', 'G816', 'G824',
             'G830', 'G831', 'G832', 'G833', 'G834', 'G839'))

# Other neurological disorders
# NEUR => OthND
elixhausers_codes_2_check[['NEURO.OTHER']] = list(
  icd9_basic = c('3319', '3320', '3334',
                 '3335', '334', '3343', '3353',
                 '340', '3414', '3454',
                 '3481', '3483', '7803', '7843'),
  
  icd9 = c('3319', '3320', '3321', '3334',
           '3335', '334', '3343', '3353',
           '3362', '3403', '3414', '3454',
           '3481', '3483', '7803', '7843'),
  
  icd10 = c('G10', 'G119', 'G122', 'G138', 'G202', 'G212', 'G215', 
            'G223', 'G254', 'G255',
            'G312', 'G318', 'G319',
            'G32x', 'G352', 'G363', 'G366', 'G372',
            'G402', 'G415', 'G931',
            'G934', 'R470', 'R564'))

# Chronic pulmonary disease
elixhausers_codes_2_check[['CHRONIC.PULM']] = list(
  icd9_basic = c('490', '4902', '4912', '492', '4934',
                 '494', '496', '497', '498', '4999',
                 '5002', '5012', '5023', '503', '5045', '5059',
                 '5064'),
  
  icd9 = c('4168', '4169', '490', '4902', '4912', '492', '4934',
           '4942', '4945', '496', '497', '498', '4999',
           '5002', '5012', '5023', '503', '5045', '5059',
           '5064', '5081', '5088'),
  
  icd10 = c('I278', 'I279', 'J402', 'J412', 
            'J42', 'J433', 'J449', 'J45', 'J46', 'J472',
            'J602', 'J61', 'J623', 'J639', 'J64', 'J655', 'J666', 'J672', 'J684',
            'J701', 'J703'))

# Diabetes, uncomplicated
elixhausers_codes_2_check[['DM.UNCOMP']] = list(
  icd9_basic = c('2500', '2501', '2502', '2503'),
  
  icd9 = c('2500', '2501', '2502', '2503'),
  
  icd10 = c('E100', 'E101', 'E109',
            'E110', 'E111', 'E119',
            'E120', 'E121', 'E129',
            'E130', 'E131', 'E139',
            'E140', 'E141', 'E149'))

# Diabetes, complicated
elixhausers_codes_2_check[['DM.COMP']] = list(
  icd9_basic = c('2504', '2505', '2506', '25073', '2509'),
  
  icd9 = c('2504', '2505', '2506', '2507', '2508', '2509'),
  
  icd10 = c('E102', 'E103', 'E104', 'E105', 'E106', 'E107', 'E108',
             'E112', 'E113', 'E114', 'E115', 'E116', 'E117', 'E118', 'E122', 'E123', 'E124', 'E125', 'E126', 'E127', 'E128',
             'E132', 'E133', 'E134', 'E135', 'E136', 'E137', 'E138',
             'E142', 'E143', 'E144', 'E145', 'E146', 'E147', 'E148'))

# Hypothyroidism
elixhausers_codes_2_check[['HYPOTHYROID']] = list(
  icd9_basic = c('2431', '2442', '2448', '2449'),
  
  icd9 = c('2409', '2431', '2443', '2461', '2468'),
  
  icd10 = c('E002', 'E003', 'E004', 'E012', 'E022', 'E033', 'E890'))

# Renal failure
elixhausers_codes_2_check[['RENAL']] = list(
  icd9_basic = c('40311', '40391', 
                 '40492', '585', '586',
                 'V420', 'V451', 'V560'),
  
  icd9_SE = c('4030', '4031', '4039', 
              '4040', '4041', '4041',
              '4049', '5854', '5863',
              '5880', 'V420', 'V451', 'V564'),
  
  icd9 = c('40301', '40311', '40391', 
           '40402', '40413', '40412',
           '40492', '5854', '5863',
           '5880', 'V420', 'V451', 'V564'),
  
  icd10 = c('I120', 'I131', 'N182',
            'N192', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992'))

# Liver disease
elixhausers_codes_2_check[['LIVER']] = list(
  icd9_basic = c('07032', '07033', 
                 '07054', '4560', '4561', '4562', 
                 '5713', '5723', '5728', 'V427'),
  
  icd9_SE = c('0702', '0703', 
              '0704', '0705',
              '4560', '4561', '4562', '5702', '5713',
              '5722', '5723', '5724', '5725', '5726', '5727', '5728', '5733', '5734',
              '5738', '5739', 'V427'),
  
  icd9 = c('07022', '07032', 
           '07044', '07054',
           '4560', '4561', '4562', '5702', '5713',
           '5722', '5723', '5724', '5725', '5726', '5727', '5728', '5733', '5734',
           '5738', '5739', 'V427'),
  
  icd10 = c('B18x', 'I85x', 'I864', 'I982',
            'K70x', 'K711',
            'K713', 'K714', 'K715', 'K717',
            'K723', 'K733', 'K749', 'K760',
            'K762', 'K763', 'K764', 'K765', 'K766', 'K767', 'K768', 'K769', 'Z944'))

# Peptic ulcer disease excluding bleeding
elixhausers_codes_2_check[['PUD']] = list(
  icd9_basic = c('5317', '5319', '5327',
                 '5329', '5337', '5339',
                 '5347', '5349'),
  
  icd9 = c('5317', '5319', '5327',
           '5329', '5337', '5339',
           '5347', '5349'),

  icd10 = c('K257', 'K259', 'K267', 'K269',
             'K277', 'K279', 'K287',
             'K289'))

# AIDS/HIV
elixhausers_codes_2_check[['HIV']] = list(
  icd9_basic = c('0422', '0439', '0441'),
  
  icd9 = c('0422', '0439', '0441'),
  
  icd10 = c('B201', 'B212', 'B229', 'B243'))

# Lymphoma
# LYMPH => Lymp
elixhausers_codes_2_check[['LYMPHOMA']] = list(
  icd9_basic = c('2002', '2012', '2029', '2030', '2386'),
  
  icd9 = c('2002', '2012', '2029', '2030', '2386'),

  icd10 = c('C812', 'C823', 'C839', 
            'C841', 'C844', 'C853', 
            'C881', 'C961',
            'C900', 'C902'))

# Metastatic cancer
elixhausers_codes_2_check[['METS']] = list(
  icd9_basic = c('1967', '1977', '1991'),
  
  icd9 = c('1967', '1977', '1998'),
  
  icd10 = c('C771', 'C782', 'C799', 'C801'))

# Solid tumor without metastasis
# CANCER => Tumor
elixhausers_codes_2_check[['SOLID.TUMOR']] = list(
  icd9_basic = c('1402', '1412', '1438', '1444', '1459', '1461', '1477', '1489', '1491',
                 '1501', '1511', '1521', '1539', '1543', '1557', '1561', '1572', '1580', '1599',
                 '1601', '1619', '1621', '1637', '1644', '1652', '1666', '1671', '1688', '1691',
                 '1707', '1712', '1723', '1743', '1755', '1790',
                 '1808', '1810', '1820', '1831', '1844', '1852', '1867', '1871', '1880', '1894',
                 '1905', '1913', '1921', '1939', '1945', '1951'),
  
  icd9 = c('1402', '1412', '1438', '1444', '1459', '1461', '1477', '1489', '1491',
           '1501', '1511', '1521', '1539', '1543', '1557', '1561', '1572', '1580', '1599',
           '1601', '1619', '1621', '1637', '1644', '1652', '1666', '1671', '1688', '1691',
           '1707', '1712', '1723', '1743', '1755', '1761', '1777', '1789', '1790',
           '1808', '1810', '1820', '1831', '1844', '1852', '1867', '1871', '1880', '1894',
           '1905', '1913', '1921', '1939', '1945', '1951'),
  
  icd10 = c('C001', 'C010', 'C024', 'C035', 'C045', 'C055', 'C066', 'C076', 'C088', 'C092',
            'C102', 'C111', 'C121', 'C130', 'C142', 'C155', 'C163', 'C178', 'C184', 'C191',
            'C201', 'C217', 'C229', 'C234', 'C249', 'C251', 'C263',
            'C302', 'C310', 'C323', 'C334', 'C346',
            'C372', 'C380', 'C391',
            'C401', 'C412', 'C433', 'C453', 'C467', 'C479', 'C481', 'C490',
            'C507', 'C517', 'C525', 'C531', 'C546', 'C551', 'C561', 'C574', 'C583',
            'C602', 'C619', 'C629', 'C636', 'C645', 'C673', 'C681', 'C697',
            'C708', 'C718', 'C724', 'C735', 'C744', 'C752', 'C762',
            'C972'))

# Rheumatoid arthritis/collagen vascular diseases
# Rheum => Rheum_A
elixhausers_codes_2_check[['RHEUM']] = list(
  icd9_basic = c('7010', '7100', '7101', '7102', '7103', '7104', '7108', '7109',
                 '7148', '7203', '725'),

  icd9_SE = c('4462', '7010', '7100', '7101', '7102', '7103', '7104', '7108', '7109',
              '7112', '7148', '7193',
              '7203', '7258', '7285',
              '7288', '7293'),
  
  icd9 = c('4462', '7010', '7100', '7101', '7102', '7103', '7104', '7108', '7109',
           '7112', '7148', '7193',
           '7203', '7258', '7285',
           '72889', '72930'),
  
  icd10 = c('L940', 'L941', 'L943', 'M051',
            'M062', 'M083', 'M120', 'M123',
            'M303', 'M310', 'M311', 'M312', 'M313',
            'M322', 'M333', 'M346', 'M359',
            'M45x', 'M461', 'M468',
            'M469'))

# Coagulopathy
elixhausers_codes_2_check[['COAG']] = list(
  icd9_basic = c('2860', '2864', '2871', '2873', '2874', '2875'),
  
  icd9 = c('286x', '2871', '2873', '2874', '2875'),
  
  icd10 = c('D65', 'D66', 'D67', 'D682', 'D691',
             'D693', 'D694', 'D695', 'D696'))

# Obesity
elixhausers_codes_2_check[['OBESITY']] = list(
  icd9_basic = c('2780'),
  
  icd9 = c('2780'),
  
  icd10 = c('E662'))

# Weight loss
# WGHT_LOSS => WL
elixhausers_codes_2_check[['WT.LOSS']] = list(
  icd9_basic = c('2602', '2603', '2612', '2623', '2637'),
  
  icd9 = c('2602', '2603', '2612', '2623', '2637', 
           '7832', '7994'),
  
  icd10 = c('E402', 'E403', 'E412', 'E424', 'E434', 'E449', 
            'E451', 'E461', 'R634', 'R64'))

# Fluid and electrolyte disorders
elixhausers_codes_2_check[['LYTES']] = list(
  icd9_basic = c('2760', '2762', '2769'),
  
  icd9 = c('2536', '2762'),
  
  icd10 = c('E222', 'E863', 'E879'))

# Blood loss anemia
elixhausers_codes_2_check[['ANEMIA.LOSS']] = list(
  icd9_basic = c('2800'),
  
  icd9 = c('2800'),
  
  icd10 = c('D500'))

# Deficiency anemia
# ANEMIA => DA
elixhausers_codes_2_check[['ANEMIA.DEF']] = list(
  icd9_basic = c('2801', '2802', '2803', '2804', '2805', 
                 '2806', '2807', '2808', '2809', '2813'),
  
  icd9 = c('2801', '2802', '2803', '2804', '2805', 
           '2806', '2807', '2808', '2809', '2813'),
  
  icd10 = c('D508', 'D509', 'D512', 'D523', 'D539'))

# Alcohol abuse
elixhausers_codes_2_check[['ETOH']] = list(
  icd9_basic = c('2911', '2912', 
                 '2915', '2918', '2919', 
                 '3039', '3050', 
                 'V113'),
  
  icd9 = c('2652', '2911', '2912', '2913',
           '2915', '2916', '2917', '2918', '2919', '3030',
           '3039', '3050', '3575',
           '4255', '5353',
           '5710', '5711', '5712', '5713', '9802', 'V113'),
  
  icd10 = c('F10', 'E52', 'G621', 'I426',
             'K292', 'K700', 'K703',
             'K709', 'T511', 'Z502',
             'Z714', 'Z721'))

# Drug abuse
# DR_ABUSE => Drug
elixhausers_codes_2_check[['DRUGS']] = list(
  icd9_basic = c('2920', '29282', '3049', '3052', '3053', '3054', 
                 '3055', '3056', '3057', '3058', '3059'),
  
  icd9 = c('2922', '3049', '3052', '3053', '3054', '3055', '3056',
           '3057', '3058', '3059', 'V6542'),
  
  icd10 = c('F112', 'F129', 'F134', 'F151', 'F160', 'F189', 'F196',
             'Z715', 'Z722'))

# Psychoses
elixhausers_codes_2_check[['PSYCHOSES']] = list(
  icd9_basic = c('2952', '29604',
                 '2960', '2968',
                 '2978', '2983'),
  
  icd9_SE = c('2938', '2952', '29604',
           '2960', '2968',
           '2978', '2983'),
  
  icd9 = c('2938', '2952', '29604',
           '29614', '29644',
           '2978', '2983'),

  icd10 = c('F202', 'F222', 'F232', 'F249', 'F257', 'F281',
            'F292', 'F302', 'F312', 'F315'))

# Depression
elixhausers_codes_2_check[['DEPRESSION']] = list(
  icd9_basic = c('3004', '3091', '311'),
  
  icd9_SE = c('2961', '2963', '2965',
           '3004', '3092', '311'),

  icd9 = c('2962', '2963', '2965',
           '3004', '3092', '311'),
  
  icd10 = c('F204', 'F313', 'F314', 'F315', 'F322',
            'F339', 'F341', 'F412', 'F432'))


context("Numeric functions")

test_that("Check Elixhausers matches or the basic elixhauser() function",{
  for (n in names(elixhausers_codes_2_check)){
    if ("icd9_basic" %in% names(elixhausers_codes_2_check[[n]])){
      codes <- elixhausers_codes_2_check[[n]]$icd9_basic
      test_df <- data.frame(Codes=codes)
      out <- elixhauser(test_df)
      found_codes <- out$COMORBIDITIES[,n] == 1
      expect_true(all(found_codes), 
                  info=sprintf("The script fails to properly identify from '%s' the codes '%s'",
                               n,
                               paste(codes[!found_codes],
                                     collapse="', '")))
    }
  }
})


test_that("Check elixhauser count score",{
  t1 <- data.frame(icd1=elixhausers_codes_2_check$CHF$icd9_basic[1],
                   icd2=elixhausers_codes_2_check$VALVE$icd9_basic[1],
                   icd3=elixhausers_codes_2_check$WT.LOSS$icd9_basic[1])
  out <- elixhauser(t1)
  expect_equal(out$COMORBIDITY.CT, 1+1+1,
               label="3 code groups counted")
  
  t2 <- data.frame(icd1=elixhausers_codes_2_check$HTN.COMP$icd9_basic[1],
                   icd2=elixhausers_codes_2_check$DRUGS$icd9_basic[1],
                   icd3=elixhausers_codes_2_check$DM.COMP$icd9_basic[1],
                   icd4=elixhausers_codes_2_check$ANEMIA.LOSS$icd9_basic[1])
  out <- elixhauser(t2)
  expect_equal(out$COMORBIDITY.CT, 1+1+1+1, 
               label="4 code groups counted")
  
  t3 <- data.frame(icd1=elixhausers_codes_2_check$HTN.COMP$icd9_basic[1],
                   icd2=elixhausers_codes_2_check$DRUGS$icd9_basic[1],
                   icd3=NA,
                   icd4=elixhausers_codes_2_check$ANEMIA.LOSS$icd9_basic[1])

  out <- elixhauser(t3)
  expect_equal(out$COMORBIDITY.CT, 1+1+0+1, 
               label="3 code groups and one missing counted")
  
})


neg.icd9_codes <- 
  as.character(
    c(128.9, #   HELMINTH INFECTION UNSPECIFIED
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

test_that("Check elixhauser handling of negative scores",{
  test_df <- data.frame(Codes=neg.icd9_codes)
  out <- elixhauser(test_df)
  expect_true(all(out$COMORBIDITY.CT == 0), 
              label="Identified comorbidities that should be negative",
              info=sprintf("Comorbity code(s) identified: %s",
                           paste(neg.icd9_codes[rowSums(out$COMORBIDITIES) > 0],
                                 collapse=",")))
})


context("Regular expression functions")

test_that("Check Elixhausers matches to the regular expression Quan version",{
  for (n in names(elixhausers_codes_2_check)){
    for (icd_ver in names(elixhausers_codes_2_check[[n]])){
      if (icd_ver == "icd9_basic") {next;} # Only for the numerical version
      # There are a few Swedish adaptations that have a different match
      if (grepl("_SE", icd_ver)) {
        country_code = "SE"
      }else{
        country_code = "US"
      }
      codes <- elixhausers_codes_2_check[[n]][[icd_ver]]
      out <- t(sapply(codes,
                      function(code)
                        cmrbdt.finder.regex.elixhauser_Quan2005(icd_codes=code, 
                                                     country_code=country_code,
                                                     icd_ver=gsub("[^0-9]", "", icd_ver))))
      found_codes <- out[,n]
      expect_true(all(found_codes), 
                  info=sprintf("The script fails to properly identify ICD-%s from '%s' the codes '%s'",
                               gsub("[^0-9]", "", icd_ver),
                               n,
                               paste(codes[!found_codes],
                                     collapse="', '")))

#       # HTN.COMP overlaps and others - skip this check
#       falsely_found_codes <- apply(out[,-grep(n, colnames(out))], 1, any)
#       expect_true(all(!falsely_found_codes), 
#                   info=sprintf("The script fails to properly not identify ICD-%s from '%s' the codes '%s'",
#                                substring(icd_ver, nchar(icd_ver)),
#                                n,
#                                paste(codes[falsely_found_codes],
#                                      collapse="', '")))
      
    }
  }
})
