testthat::test_that("test_unique", {
  
  df_name <- "x"
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3), 
                   z = c(NA, NA, 1:2), w = c(NA, 1:3))
  
  setup <- setup_test_unique(df_name, col_name = "x")
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "y")
  test <- test(setup, df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "z")
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "w")
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  df <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), 
                   z = c(1,2,1,1), w = c(1,2,1,NA))
  
  setup <- setup_test_unique(df_name, col_name = c("x", "y"))
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = c("x", "z"))
  test <- test(setup, df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = c("x", "w"))
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

})
