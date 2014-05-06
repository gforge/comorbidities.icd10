context("Test Deyo helpers")

test_that("Check pre-processing",{
  tf <- data.frame(ICD1 = c(4210, "V4340"), ICD2=c("4288", NA))
  expect_equivalent(pr.deyo.preprocess.icd9(tf),
                    matrix(c(42100, 44390, 42880, NA), nrow=2, byrow=FALSE))
})

test_that("Check ICD 5 digit handling", {
  expect_equal(pr.deyo.ICD9.5digit(NA), NA)
  
  # Input is expected to output a 5 digit output
  expect_equal(pr.deyo.ICD9.5digit("5412"), 54120)
  
  # Don't adjust numeric codes
  expect_equal(pr.deyo.ICD9.5digit(5412), 5412)
})
