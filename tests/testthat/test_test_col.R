testthat::test_that("test_values", {
  
  df <- data.frame(x = 1:4, y = 5:8, z = c(1:3, NA))
  setup <- setup_test_values(df_name = "X", col_name = "x", 
                values = 1:4, na = TRUE)
   
  test <- test_values(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "y", 
                values = 1:4, na = TRUE)
  test <- test_values(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                values = 1:4, na = TRUE)
  test <- test_values(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_values(df_name = "X", col_name = "z", 
                values = 1:4, na = FALSE)
  test <- test_values(df, setup)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_na", {
  
  df <- data.frame(x = 1:4, y = c(NA, 6:8))
  
  setup <- setup_test_na(df_name = "x", col_name = "x")
  test <- test_na(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_na(df_name = "x", col_name = "y")
  test <- test_na(df, setup)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_unique", {
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3), 
                   z = c(NA, NA, 1:2), w = c(NA, 1:3))
  
  setup <- setup_test_unique(df_name = "x", col_name = "x", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name = "x", col_name = "y", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name = "x", col_name = "z", na = TRUE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name = "x", col_name = "z", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name = "x", col_name = "w", na = TRUE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "x", col_name = "w", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
})