testthat::test_that("test_unique", {
  
  df_name <- "x"
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3), 
                   z = c(NA, NA, 1:2), w = c(NA, 1:3))
  
  setup <- setup_test_unique(df_name, col_name = "x", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "y", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "z", na = TRUE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "z", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "w", na = TRUE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = "w", na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  df <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), 
                   z = c(1,2,1,1), w = c(1,2,1,NA))
  
  setup <- setup_test_unique(df_name, col_name = c("x", "y"), na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = c("x", "z"), na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_unique(df_name, col_name = c("x", "w"), na = FALSE)
  test <- test_unique(df, setup)
  testthat::expect_true(test$test_result)

})
