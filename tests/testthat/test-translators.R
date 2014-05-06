context("Translators")

test_that("Check Swedish ICD-9 translator",{
  expect_equal(translate.ICD9.from.Swedish("456A"), "4560")
  expect_equal(translate.ICD9.from.Swedish("456X"), "4569")
  expect_error(translate.ICD9.from.Swedish("3133"))
  expect_error(translate.ICD9.from.Swedish("313M"))
  
  expect_equal(translate.ICD9.from.Swedish(c("456A", "873B")),
              c("4560", "8731"))
  
})