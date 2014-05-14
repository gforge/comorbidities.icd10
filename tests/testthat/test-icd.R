context("Check icd functions")

test_that("Test the is.ICD function",{
  expect_true(all(is.ICD(c("M161", "M509", "B999", "M511K"))),
              info="Issue in correctly identifying ICD-10 codes")
  
  
  expect_true(all(is.ICD(c("78079", "53081"))),
              info="Issue in correctly identifying ICD-9 codes")
  

  expect_false(any(is.ICD(c("780.79", "M16.1", "wild crazy string", "£1@£$"))),
              info="Identifies incorrect strings")
})