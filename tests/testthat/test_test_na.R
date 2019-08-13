testthat::test_that("test_na", {
  
  df <- data.frame(x = 1:4, y = c(NA, 6:8))
  
  setup <- setup_test_na(df_name = "x", col_name = "x")
  test <- test_na(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_na(df_name = "x", col_name = "y")
  test <- test_na(df, setup)
  testthat::expect_false(test$test_result)
  
})
