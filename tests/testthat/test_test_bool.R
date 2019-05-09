# Test SetUp

vec1 <- c(TRUE, TRUE, TRUE)
vec2 <- c(FALSE, FALSE, FALSE)
vec3 <- c(TRUE, FALSE, TRUE)

df <- data.frame(col1 = vec1, col2 = vec2, col3 = vec3)

testthat::test_that("tests that all TRUE evaluates correctly", {
    
    testthat::expect_true(test_all_true_test(vec1))
    testthat::expect_false(test_all_true_test(vec2))
    testthat::expect_false(test_all_true_test(vec3))
    
    testthat::expect_true(test_all_true_test(df$col1))
    testthat::expect_false(test_all_true_test(df$col2))
    testthat::expect_false(test_all_true_test(df$col3))
    
})

testthat::test_that("tests that all FALSE evaluates correctly", {
    
    testthat::expect_false(test_all_false_test(vec1))
    testthat::expect_true(test_all_false_test(vec2))
    testthat::expect_false(test_all_false_test(vec3))
    
    testthat::expect_false(test_all_false_test(df$col1))
    testthat::expect_true(test_all_false_test(df$col2))
    testthat::expect_false(test_all_false_test(df$col3))
    
})

testthat::test_that("tests that any TRUE evaluates correctly", {
    
    testthat::expect_true(test_any_true_test(vec1))
    testthat::expect_false(test_any_true_test(vec2))
    testthat::expect_true(test_any_true_test(vec3))
    
    testthat::expect_true(test_any_true_test(df$col1))
    testthat::expect_false(test_any_true_test(df$col2))
    testthat::expect_true(test_any_true_test(df$col3))
    
})

testthat::test_that("tests that any FALSE evaluates correctly", {
    
    testthat::expect_false(test_any_false_test(vec1))
    testthat::expect_true(test_any_false_test(vec2))
    testthat::expect_true(test_any_false_test(vec3))
    
    testthat::expect_false(test_any_false_test(df$col1))
    testthat::expect_true(test_any_false_test(df$col2))
    testthat::expect_true(test_any_false_test(df$col3))
    
})

testthat::test_that("tests that all TRUE return a vector of length 3", {
    
    testthat::expect_equal(length(test_all_true(vec1)), 3)
    testthat::expect_equal(test_all_true(vec1)[2], test_pass$ti)
    
    testthat::expect_equal(length(test_all_true(vec2)), 3)
    testthat::expect_equal(test_all_true(vec2)[2], test_fail$ti)
    
})

testthat::test_that("tests that any TRUE return a vector of length 3", {
    
    testthat::expect_equal(length(test_any_true(vec1)), 3)
    testthat::expect_equal(test_any_true(vec1)[2], test_pass$ti)
    
    testthat::expect_equal(length(test_any_true(vec2)), 3)
    testthat::expect_equal(test_any_true(vec2)[2], test_fail$ti)
    
})


testthat::test_that("tests that all TRUE return a vector of length 3", {
  
  testthat::expect_equal(length(test_all_false(vec1)), 3)
  testthat::expect_equal(test_all_false(vec1)[2], test_fail$ti)
  
  testthat::expect_equal(length(test_all_false(vec2)), 3)
  testthat::expect_equal(test_all_false(vec2)[2], test_pass$ti)
  
})

testthat::test_that("tests that all TRUE return a vector of length 3", {
  
  testthat::expect_equal(length(test_any_false(vec1)), 3)
  testthat::expect_equal(test_any_false(vec1)[2], test_fail$ti)
  
  testthat::expect_equal(length(test_any_false(vec2)), 3)
  testthat::expect_equal(test_any_false(vec2)[2], test_pass$ti)
  
  testthat::expect_equal(length(test_any_false(vec3)), 3)
  testthat::expect_equal(test_any_false(vec3)[2], test_pass$ti)
  
})
