testthat::test_that("setup_test_values tests", {
  
  df_name <- "x"; col_name <- "X"
  values <- c(1,2,3); na <- NA
  
  expect_error(setup_test_values(df_name, col_name, values, na))
  
  setup <- setup_test_values(df_name, col_name, values, na  = TRUE)
  testthat::expect_true(NA %in% setup$values)
  
  setup <- setup_test_values(df_name, col_name, values, na = FALSE)
  testthat::expect_false(NA %in% setup$values)
  
})

testthat::test_that("test_values", {
  
  df <- data.frame(x = 1:4, y = 5:8, z = c(1:3, NA))
  setup <- setup_test_values(df_name = "X", col_name = "x", 
                             values = 1:4, na = TRUE)
  
  test <- test.values(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "y", 
                             values = 1:4, na = TRUE)
  test <- test.values(setup, df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                             values = 1:4, na = TRUE)
  test <- test.values(setup, df)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                             values = 1:4, na = FALSE)
  test <- test.values(setup, df)
  testthat::expect_false(test$test_result)
  
})