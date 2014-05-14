context("Pre-processor functions")

test_that("code_splitter", {
  expect_equivalent(length(preproc.code.splitter(icd=c("M161", "M161 ", "M161 J445"),
                                                 icd_ver=c(10, 10, 10))),
                    4)
  
  expect_equivalent(length(attr(preproc.code.splitter(icd=c("M161", "M161 ", "M161 J445"),
                                                      icd_ver=c(10, 10, 10)),
                                "icd_ver")),
                    4)
  
  expect_equivalent(length(preproc.code.splitter(icd=c("M161", "M161 ", "M161"),
                                                 icd_ver=c(10, 10, 10))),
                    3)
  
  expect_equivalent(length(attr(preproc.code.splitter(icd=c("M161", "M161 ", "M161"),
                                                      icd_ver=c(10, 10, 10)),
                                "icd_ver")),
                    3)
  
  expect_equivalent(length(preproc.code.splitter(icd=c("M161", "  M161 ", "  M161  "),
                                                 icd_ver=c(10, 10, 10))),
                    3)
  
  expect_equivalent(preproc.code.splitter(icd=c("M161", "J445, M161 ", "  M161  "), 
                                          split_str=", ",
                                          icd_ver=c(10, 10, 10)),
                    c("M161", "J445", "M161", "M161"))
  
  expect_equivalent(length(preproc.code.splitter(icd=c("M161", "J445, M161 ", "  M161  "), 
                                                 split_str=", ",
                                                 trim=FALSE,
                                                 icd_ver=c(10, 10, 10))),
                    4)
})


test_that("Check Swedish ICD-9 translator",{
  expect_equivalent(preproc.Swedich.ICD9("456A"), "4560")
  expect_equivalent(preproc.Swedich.ICD9("456X"), "4569")
  expect_error(preproc.Swedich.ICD9("3133"),
               info="If no letter at the fourth position there is an error")
  expect_error(preproc.Swedich.ICD9("313M"),
               info="M is not a valid letter at the fourth position and should give an error")
  
  expect_equivalent(preproc.Swedich.ICD9(c("456A", "873B")),
                    c("4560", "8731"))
  
  
  expect_equivalent(preproc.Swedich.ICD9(c("456a", "873b")),
                    c("4560", "8731"))
  
  expect_equivalent(preproc.Swedich.ICD9(icd=c("456a", "873b", "M161"),
                                         icd_ver=c(9,9,10)),
                    c("4560", "8731", "M161"))
  
  expect_error(preproc.Swedich.ICD9(icd=c("456a", "873b", "M161"),
                                    icd_ver=c(9,9,9)),
               info="An error should be thrown if an ICD-10 code is tried to be translated according to the Swedish fourth letter system.")
  
  expect_error(preproc.Swedich.ICD9(icd=c("456a", "873b", "M161"),
                                    icd_ver=c(9,9)), 
               info="The funciton should complain if there is a mismatch between ICD-codes and the true value")
  
  expect_equivalent(preproc.Swedich.ICD9(icd=c("456a", "873b", "M161"),
                                         icd_ver=c(9,9,FALSE)),
                    c("4560", "8731", "M161"))
  
  expect_equivalent(preproc.Swedich.ICD9(icd=c("456a", "873b", "M161"),
                                         icd_ver=FALSE),
                    c("4560", "8731", "M161"))
})

test_that("Advanced Swedish pre-processor test",{
  # Fake data frame
  swe_test_df <-
    data.frame(ID=c(1:4, 1),
               MainICD = c('M161  ', '715B   ', '410B   ', 'J441   ', 'C679   '),
               CoICD = c('C679 N210', '780E', '296C', 'K802 I509', 'K859'))
  
  swe_test_df$Icd_ver <- 
    ifelse(substr(swe_test_df$MainICD, 4, 4) %in% LETTERS,
           9,
           10)
  
  
  swe_proc <- 
    function(icd, icd_ver){
      icd <- preproc.code.splitter(icd, icd_ver)
      preproc.Swedich.ICD9(icd, attr(icd, "icd_ver"))
    }
  
  # The most important part is that this passes
  expect_true(is.list(cmrbdt.calc(swe_test_df, 
                                  id_column="ID", icd_columns=c("MainICD", "CoICD"), 
                                  icd_ver_column="Icd_ver",
                                  cmrbdt.finder_fn=cmrbdt.finder.regex.charlson_Quan2005, 
                                  cmrbdt.finder_hierarchy_fn=hierarchy.charlson_Quan2005, 
                                  cmrbdt.weight_fn=weight.Charlsons.org, 
                                  icd_code_preprocess_fn=swe_proc)))
  
})

