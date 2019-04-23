# Tests for numeric column tests

vec <- c(1, 2, 3)
df <- data.frame(col = vec)

testthat::test_that("tests test_less_than_value_test evaluates correctly", {
    
    testthat::expect_true(test_less_than_value_test(vec, 4))
    testthat::expect_false(test_less_than_value_test(vec, 3))
    
})

testthat::test_that("tests test_less_than_value evaluates correctly", {
    
    testthat::expect_equal(length(test_less_than_value(vec, 4)), 3)
    testthat::expect_equal(test_less_than_value(vec, 4)[2], test_pass_ti)
    
    testthat::expect_equal(length(test_less_than_value(vec, 3)), 3)
    testthat::expect_equal(test_less_than_value(vec, 3)[2], test_fail_ti)
    
})

testthat::test_that("tests test_less_than_or_equal_value_test evaluates correctly", {
  
  testthat::expect_true(test_less_than_or_equal_value_test(vec, 4))
  testthat::expect_true(test_less_than_or_equal_value_test(vec, 3))
  testthat::expect_false(test_less_than_or_equal_value_test(vec, 2))
  
})

testthat::test_that("tests test_less_than_or_equal_value evaluates correctly", {
  
  testthat::expect_equal(length(test_less_than_or_equal_value(vec, 4)), 3)
  testthat::expect_equal(test_less_than_or_equal_value(vec, 4)[2], test_pass_ti)
  
  testthat::expect_equal(length(test_less_than_or_equal_value(vec, 3)), 3)
  testthat::expect_equal(test_less_than_or_equal_value(vec, 3)[2], test_pass_ti)
  
  testthat::expect_equal(length(test_less_than_or_equal_value(vec, 2)), 3)
  testthat::expect_equal(test_less_than_or_equal_value(vec, 2)[2], test_fail_ti)
  
})
