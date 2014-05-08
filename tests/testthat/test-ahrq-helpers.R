context("AHRQ helpers")

test_that("Check v.codes translator", {
  expect_equivalent(pr.ahrq.preprocess.v.codes(c("V420", "V427")),
                    c(58530, 7022))
  
  expect_equivalent(pr.ahrq.preprocess.v.codes("V4200"),
                    58530)
})


test_that("Check ICD 5 digit handling", {
  expect_equivalent(pr.ahrq.ICD9.5digit(NA), NA)
  
  # Input is expected to output a 5 digit output
  expect_equivalent(pr.ahrq.ICD9.5digit("5412"), 54120)
  
  # Don't adjust numeric codes
  expect_equivalent(pr.ahrq.ICD9.5digit(5412), 5412)
})
