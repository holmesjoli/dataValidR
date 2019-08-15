testthat::test_that("test_values", {
  
  df <- data.frame(x = 1:4, y = 5:8, z = c(1:3, NA))
  setup <- setup_test_values(df_name = "X", col_name = "x", 
                             values = 1:4)
  
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "y", 
                             values = 1:4)
  test <- test(setup, df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                             values = 1:4)
  test <- test(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                             values = 3:5)
  test <- test(setup, df)
  testthat::expect_false(test$test_result)

})