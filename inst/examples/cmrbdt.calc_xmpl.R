# Completely made up datasets
prim_data <- 
  data.frame(Patient_ID = c("A",
                            "B",
                            "C",
                            "D",
                            "MISSING"), # Has on purpose no match - should theoretically no occur
             # Transition from ICD-9 to ICD 10 was during 1997 in Sweden
             Surgery_date = as.Date(c("1999-01-25",
                                      "2004-02-25",
                                      "1996-07-04",
                                      "1997-12-04",
                                      "2014-05-06")),
             Surgery_type = c("Hip", # Add some non-relevant data
                              "Foot",
                              "Hand",
                              "Hip",
                              "Hand"))

admission_data <-
  data.frame(Patient_ID = 
               c("A", "A", # 2 A admission
                 "B", # 1 B admissions
                 "C", "C", "C", # 3 C admissions
                 "D", "D", "D", "D" # 4 D admissions
               ),
             admission_date = 
               as.Date(c("1999-01-24", "1998-05-29", # A
                         "2004-02-24", # B
                         "1996-07-01", "1995-02-01", "1992-10-04", # C
                         "1997-12-03", # D
                         "1998-03-01", # Admission should not be used as it is after surgery
                         "1995-10-24", "1995-08-20")),
             discharge_date = 
               as.Date(c("1999-02-01", "1998-05-25",# A
                         "2004-02-27", # B
                         "1996-07-08", "1995-02-04", "1992-10-14",# C
                         "1997-12-06", "1998-03-04", "1995-11-01", "1995-08-24" # D
               )),
             ICD1 = 
               c("M161", "S7200", # A's codes
                 "S8240", # B's codes
                 "3540", "486", "431", # C's codes - carpal tunnel, pneumonia, intracereb. hem.
                 "M169", # D's codes - Hip code
                 "B238", # This admission should be ignored! - HIV
                 "5400", "4220"), # D's codes - Peritonitis + Acute MI
             ICD2 = 
               c("I212", "I701", # A's codes - current MI, PVD
                 "N390", # B's codes
                 NA, "4011", "4011", # C's codes - benign hypertension
                 "N052", # D's codes - ICD-10 glom.nephritis 
                 "C619", # prostate tum.
                 "7812", "5569"), # ICD-9 Gait, Ulcerative Colitis
             ICD3 = 
               c("E890X", "E039", # A's codes - hypothyr.
                 NA, # B's codes
                 NA, "30301", "30009", # C's codes - Alcohol, anxiety
                 "E001", # D's codes - ICD-10 - iodine def. - thyroid. 
                 NA, 
                 "5810", "01280"), # ICD-9 Nephrotic syndr. + infection
             ICD4 = 
               c("J189", NA, # A's codes - pneumonia
                 NA, # B's codes
                 NA, "55090", NA, # C's codes
                 "N309",  # D's codes
                 NA, 
                 "42611", "6802")
  )

# Merge the data sets and include the one with no admissions
complete <- merge(prim_data, admission_data, 
                  by="Patient_ID", all=TRUE)

# Choose those with valid observations
# just to stress the code we will keep the MISSSING patient
data2analyze <- subset(complete, 
                       Surgery_date >= admission_date |
                         is.na(admission_date))

# Deduce the ICD-version from the date variable
data2analyze$icd_version <- 
  ifelse(data2analyze$discharge_date < "1997-01-01",
         9,
         ifelse(data2analyze$discharge_date >= "1998-01-01",
                10,
                FALSE))

# Figure out if the admission is the one registered for the surgery
data2analyze$include_acute <- 
  with(data2analyze,
       ifelse(discharge_date >= Surgery_date &
                admission_date <= Surgery_date,
              FALSE, # Current admission is the admission of the surgery, 
              # hence we should not include any acute episodes
              # as we are interested in pre-existing conditions
              TRUE))


out_incl_acute <- 
  cmrbdt.calc(data2analyze, 
              id_column="Patient_ID",
              icd_columns=grep("^ICD", colnames(data2analyze)),
              icd_ver_column=data2analyze$icd_version,
              cmrbdt.finder_fn=cmrbdt.finder.regex.charlson_Quan2005,
              cmrbdt.finder_hierarchy_fn=hierarchy.charlson_Quan2005,
              cmrbdt.weight_fn=weight.Charlsons.org)

out_without_acute <- 
  cmrbdt.calc(data2analyze, 
              include_acute_column="include_acute",
              id_column="Patient_ID",
              icd_columns=grep("^ICD", colnames(data2analyze)),
              icd_ver_column=data2analyze$icd_version,
              cmrbdt.finder_fn=cmrbdt.finder.regex.charlson_Quan2005,
              cmrbdt.finder_hierarchy_fn=hierarchy.charlson_Quan2005,
              cmrbdt.weight_fn=weight.Charlsons.org)

# The MI was not included for A when acute was taken into account
data.frame(ID = out_incl_acute$cmrbdt$Patient_ID,
           With=out_incl_acute$score, 
           Without=out_without_acute$score)

###################################
# Test an alterantive way to      #
# store diagnosis data by having  #
# one string separated characters #
# such as " " or ","              #
###################################
admission_data_alt <- admission_data
admission_data_alt$Main_ICD <- admission_data_alt$ICD1
admission_data_alt$Additional_ICD <- 
  apply(admission_data_alt[,c("ICD2", "ICD3", "ICD4")],
               1,
               function(x) paste(x[!is.na(x)], collapse=" "))

admission_data_alt$ICD1 <- NULL
admission_data_alt$ICD2 <- NULL
admission_data_alt$ICD3 <- NULL
admission_data_alt$ICD4 <- NULL

# Merge the data sets and include the one with no admissions
complete <- merge(prim_data, admission_data_alt, 
                  by="Patient_ID", all=TRUE)

# Choose those with valid observations
# just to stress the code we will keep the MISSSING patient
data2analyze <- subset(complete, 
                       Surgery_date >= admission_date |
                         is.na(admission_date))

# Deduce the ICD-version from the date variable
data2analyze$icd_version <- 
  ifelse(data2analyze$discharge_date < "1997-01-01",
         9,
         ifelse(data2analyze$discharge_date >= "1998-01-01",
                10,
                FALSE))

# Figure out if the admission is the one registered for the surgery
data2analyze$include_acute <- 
  with(data2analyze,
       ifelse(discharge_date >= Surgery_date &
                admission_date <= Surgery_date,
              FALSE, # Current admission is the admission of the surgery, 
              # hence we should not include any acute episodes
              # as we are interested in pre-existing conditions
              TRUE))

out_without_acute <- 
  cmrbdt.calc(data2analyze, 
              include_acute_column="include_acute",
              id_column="Patient_ID",
              icd_columns=grep("ICD$", colnames(data2analyze)),
              icd_ver_column=data2analyze$icd_version,
              cmrbdt.finder_fn=cmrbdt.finder.regex.charlson_Quan2005,
              cmrbdt.finder_hierarchy_fn=hierarchy.charlson_Quan2005,
              cmrbdt.weight_fn=weight.Charlsons.org,
              # Below is the magic function that splits the merged codes
              icd_code_preprocess_fn=function(code) unlist(strsplit(code, " ")))

# The MI was not included for A when acute was taken into account
data.frame(ID = out_incl_acute$cmrbdt$Patient_ID,
           With=out_incl_acute$score, 
           Without=out_without_acute$score)


