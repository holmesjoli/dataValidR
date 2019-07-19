testthat::test_that("test_values returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = 5:8)
  values <- 1:4; na <- FALSE
   
  test <- test_values(df, "x", values, na)
  testthat::expect_true(test$test_result)
  
  test <- test_values(df, "y", values, na)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_na returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = c(NA, 6:8))
  
  test <- test_na(df, "x") 
  testthat::expect_true(test$test_result)
  
  test <- test_na(df, "y") 
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_unique returns TRUE when PASSING, FALSE when FAILING", {
  
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3))

  test <- test_unique_test(df, "x")
  testthat::expect_true(test$test_result)
  
  test <- test_unique_test(df, "y")
  testthat::expect_true(test$test_result)
  
})