context("Test weights")

test_that("Check that no missing are introduced",{
  out <- cmrbdt.finder.regex.charlson_Quan2005("I252",
                                               country_code="US")
  expect_false(any(is.na(weight.Charlsons.org(out))))
  expect_false(any(is.na(weight.Charlsons.org(rbind(out, out)))))
  expect_equivalent(length(weight.Charlsons.org(out)),
                    length(out),
                    "The software should always return a vector of the same length")
  expect_equivalent(dim(weight.Charlsons.org(rbind(out, out))),
                    dim(rbind(out, out)),
                    "The software should always return a matrix of the same length")
})

test_that("Check calculations",{
  out <- cmrbdt.finder.regex.charlson_Quan2005("I252",
                                               country_code = "US")
  expect_equivalent(sum(weight.Charlsons.org(out)), 1,
                    "Simple test for MI")

  # Clear all values
  empty <- sapply(out, function(x) FALSE)

  hiv_test <- empty
  hiv_test["HIV"] <- 1
  expect_equivalent(sum(weight.Charlsons.org(hiv_test)), 6)

  mets_test <- empty
  mets_test["METASTASIS"] <- TRUE
  expect_equivalent(sum(weight.Charlsons.org(mets_test)), 6)

  # Not really possible due to hierarchy
  all <- sapply(out, function(x) TRUE)
  expect_equivalent(sum(weight.Charlsons.org(all)), 1*(length(all)-4-1-2)+4*2+1*3+2*6)

  all_possible <- hierarchy.charlson_Quan2005(all)
  expect_equivalent(sum(weight.Charlsons.org(all_possible)),
                    1*(length(all)-4-1-2)+4*2+1*3+2*6 -
                      2*1-2) # Remove points from diabetes, mild.liver and malignancy
})