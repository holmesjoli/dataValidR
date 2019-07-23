testthat::test_that("test_values returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = 5:8, z = c(1:3, NA))
  values <- 1:4
   
  cls <- class_test_values("df_name", "x", values, TRUE)
  test <- test_values(df, cls)
  testthat::expect_true(test$test_result)
  
  cls <- class_test_values("df_name", "y", values, TRUE)
  test <- test_values(df, cls)
  testthat::expect_false(test$test_result)
  
  cls <- class_test_values("df_name", "z", values, TRUE)
  test <- test_values(df, cls)
  testthat::expect_true(test$test_result)
  
  cls <- class_test_values("df_name", "z", values, FALSE)
  test <- test_values(df, cls)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_na returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = c(NA, 6:8))
  
  cls <- class_test_na("df_name", "x") 
  test <- test_na(df, cls)
  testthat::expect_true(test$test_result)
  
  cls <- class_test_na("df_name", "y") 
  test <- test_na(df, cls)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_unique returns TRUE when PASSING, FALSE when FAILING", {
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3))
  cls <- class_test_unique("df_name", "x") 
  
  test <- test_unique(df, cls)
  testthat::expect_true(test$test_result)
  
  cls <- class_test_unique("df_name", "y") 
  test <- test_unique(df, cls)
  testthat::expect_false(test$test_result)
  
})