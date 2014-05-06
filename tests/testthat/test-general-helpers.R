context("Test some private functions")


test_that("Check that the icd code identifier works as expected",{
  expect_equal(pr.get.icd.ver(c("M161"), FALSE), '10')
  expect_equal(pr.get.icd.ver(c("V161"), 10), '10')
  expect_equal(pr.get.icd.ver(c("V161"), FALSE), '9')
  expect_equal(pr.get.icd.ver(c("161"), FALSE), '9')
  expect_equal(pr.get.icd.ver(c("161"), 10), '10')
  
  expect_equal(pr.get.icd.ver(c("M161"), FALSE), '10')
  expect_equal(pr.get.icd.ver(c("V161"), 10), '10')

  expect_equal(pr.get.icd.ver(c("M161", "191", "888"), c(10, FALSE, 9)), 
               c('10','9','9'))
  
  expect_equal(pr.get.icd.ver(c("M161", "191", "V234"), c(10, FALSE, 10)), 
               c('10','9','10'))
  
  expect_equal(pr.get.icd.ver(c("M161", "191", "V234")), 
               c('10','9','9'))
})
