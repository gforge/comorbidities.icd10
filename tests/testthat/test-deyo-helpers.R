context("Test Deyo helpers")

test_that("Check pre-processing",{
  tf <- data.frame(ICD1 = c(4210, "V4340"), ICD2=c("4288", NA))
  expect_error(pr.deyo.ICD9.5digit(tf))
  
  expect_equivalent(pr.deyo.ICD9.5digit(tf$ICD1),
                    c(42100, 44390))
  expect_equivalent(pr.deyo.ICD9.5digit(tf$ICD2),
                    c(42880, NA))
})

test_that("Check ICD 5 digit handling", {
  expect_equivalent(pr.deyo.ICD9.5digit(NA), NA)
  
  # Input is expected to output a 5 digit output
  expect_equivalent(pr.deyo.ICD9.5digit("5412"), 54120)
  
  # Don't adjust numeric codes
  expect_equivalent(pr.deyo.ICD9.5digit(5412), 5412)
})
