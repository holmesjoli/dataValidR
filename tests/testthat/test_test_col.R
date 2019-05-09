vec1 <- c(1, 2, 3)
vec2 <- c(1, 1, 2)
vec3 <- c(1, NA, 2)

testthat::test_that("tests test_unique_test evaluates correctly", {
    
    testthat::expect_true(test_unique_test(vec1))
    testthat::expect_false(test_unique_test(vec2))
    
})

testthat::test_that("tests test_unique evaluates correctly", {
    
    testthat::expect_equal(length(test_unique(vec1)), 3)
    testthat::expect_equal(test_unique(vec1)[2], test_pass$ti)
    
    testthat::expect_equal(length(test_unique(vec2)), 3)
    testthat::expect_equal(test_unique(vec2)[2], test_fail$ti)
    
})


testthat::test_that("tests test_values_test evaluates correctly", {
    
    expc_values <- c(1, 2)
    
    testthat::expect_equal(length(test_values(vec2, expc_values)), 3)
    testthat::expect_equal(test_values(vec2, expc_values)[2], test_pass$ti)
    
    testthat::expect_equal(length(test_values(vec1, expc_values)), 3)
    testthat::expect_equal(test_values(vec1, expc_values)[2], test_fail$ti)
    
})

testthat::test_that("tests test_null_values_test evaluates correctly", {
    
    testthat::expect_true(test_null_values_test(vec1))
    testthat::expect_false(test_null_values_test(vec3))
    
})

testthat::test_that("tests test_null_values evaluates correctly", {
    
    testthat::expect_equal(length(test_null_values(vec1)), 3)
    testthat::expect_equal(test_null_values(vec1)[2], test_pass$ti)
    
    testthat::expect_equal(length(test_null_values(vec3)), 3)
    testthat::expect_equal(test_null_values(vec3)[2], test_fail$ti)
    
})
