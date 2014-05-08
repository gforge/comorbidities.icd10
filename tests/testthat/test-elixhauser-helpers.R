context("Elixhausers helpers")

test_that("Check v.codes translator", {
  expect_equivalent(pr.elixhauser.process.v.codes(c("V420", "V427")),
                    c(40311, 7032))

  expect_equivalent(pr.elixhauser.process.v.codes("V4200"),
                    40311)
})

test_that("Check ICD 5 digit handling", {
  expect_equivalent(pr.elixhauser.ICD9.5digit(NA), NA)
  
  # Input is expected to output a 5 digit output
  expect_equivalent(pr.elixhauser.ICD9.5digit("5412"), 54120)
  
  # Don't adjust numeric codes
  expect_equivalent(pr.ahrq.ICD9.5digit(5412), 5412)
})
