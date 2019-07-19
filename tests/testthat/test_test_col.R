testthat::test_that("test_values returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = 5:8)
  col_name <- "x"; values <- 1:4; na <- FALSE
   
  test <- test_values(df, col_name, values, na)
  testthat::expect_true(test$test_result)
  testthat::expect_that(test$test_result, testthat::equals(TRUE))
  
  col_name <- "y"
  
  test <- test_values(df, col_name, values, na)
  testthat::expect_false(test$test_result)
  testthat::expect_that(test$test_result, testthat::equals(FALSE))
  
})