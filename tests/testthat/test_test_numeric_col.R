# Tests for numeric column tests

vec <- c(1:3)
vec2 <- c(1:5)
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


testthat::test_that("tests test_greater_than_value_test evaluates correctly", {
    
    testthat::expect_true(test_greater_than_value_test(vec, 0))
    testthat::expect_false(test_greater_than_value_test(vec, 1))
    
})

testthat::test_that("tests test_greater_than_value evaluates correctly", {
    
    testthat::expect_equal(length(test_greater_than_value(vec, 0)), 3)
    testthat::expect_equal(test_greater_than_value(vec, 0)[2], test_pass_ti)
    
    testthat::expect_equal(length(test_greater_than_value(vec, 1)), 3)
    testthat::expect_equal(test_greater_than_value(vec, 1)[2], test_fail_ti)
    
})

testthat::test_that("tests test_greater_than_or_equal_value_test evaluates correctly", {
    
    testthat::expect_true(test_greater_than_or_equal_value_test(vec, 0))
    testthat::expect_true(test_greater_than_or_equal_value_test(vec, 1))
    testthat::expect_false(test_greater_than_or_equal_value_test(vec, 2))
    
})

testthat::test_that("tests test_greater_than_or_equal_value evaluates correctly", {
    
    testthat::expect_equal(length(test_greater_than_or_equal_value(vec, 0)), 3)
    testthat::expect_equal(test_greater_than_or_equal_value(vec, 0)[2], test_pass_ti)
    
    testthat::expect_equal(length(test_greater_than_or_equal_value(vec, 1)), 3)
    testthat::expect_equal(test_greater_than_or_equal_value(vec, 1)[2], test_pass_ti)
    
    testthat::expect_equal(length(test_greater_than_or_equal_value(vec, 2)), 3)
    testthat::expect_equal(test_greater_than_or_equal_value(vec, 2)[2], test_fail_ti)
    
})

testthat::test_that("tests test_exclu_value_range_test evaluates correctly", {
    
    testthat::expect_true(test_exclu_value_range_test(vec, 0, 4))
    testthat::expect_false(test_exclu_value_range_test(vec, 0, 3))
    testthat::expect_false(test_exclu_value_range_test(vec, 1, 3))
    
})

testthat::test_that("tests test_greater_than_or_equal_value evaluates correctly", {
    
    testthat::expect_equal(length(test_exclu_value_range(vec, 0, 4)), 3)
    testthat::expect_equal(test_exclu_value_range(vec, 0, 4)[2], test_pass_ti)
    
    testthat::expect_equal(length(test_exclu_value_range(vec, 0, 3)), 3)
    testthat::expect_equal(test_exclu_value_range(vec, 0, 3)[2], test_fail_ti)
    
    testthat::expect_equal(length(test_exclu_value_range(vec, 1, 3)), 3)
    testthat::expect_equal(test_exclu_value_range(vec, 1, 3)[2], test_fail_ti)
    
})

testthat::test_that("tests test_inclu_value_range evaluates correctly", {
  
  testthat::expect_true(test_inclu_value_range_test(vec, 1, 3))
  testthat::expect_true(test_inclu_value_range_test(vec, 1, 4))
  testthat::expect_false(test_inclu_value_range_test(vec, 2, 5))
  
})

testthat::test_that("tests test_inclu_value_range evaluates correctly", {
  
  testthat::expect_equal(length(test_inclu_value_range(vec, 1, 3)), 3)
  testthat::expect_equal(test_inclu_value_range(vec, 1, 3)[2], test_pass_ti)
  
  testthat::expect_equal(length(test_inclu_value_range(vec, 1, 4)), 3)
  testthat::expect_equal(test_inclu_value_range(vec, 1, 4)[2], test_pass_ti)
  
  testthat::expect_equal(length(test_inclu_value_range(vec, 2, 5)), 3)
  testthat::expect_equal(test_inclu_value_range(vec, 2, 5)[2], test_fail_ti)
  
})
