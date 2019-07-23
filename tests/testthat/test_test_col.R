testthat::test_that("test_values", {
  
  df <- data.frame(x = 1:4, y = 5:8, z = c(1:3, NA))
  setup <- list(df_name = "X", col_name = "x", 
                values = 1:4, na = TRUE)
   
  cls <- class_test_values(setup)
  test <- test_values(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "X", col_name = "y", 
                values = 1:4, na = TRUE)
  cls <- class_test_values(setup)
  test <- test_values(df, cls)
  testthat::expect_false(test$test_result)
  
  setup <- list(df_name = "X", col_name = "z", 
                values = 1:4, na = TRUE)
  cls <- class_test_values(setup)
  test <- test_values(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "X", col_name = "z", 
                values = 1:4, na = FALSE)
  cls <- class_test_values(setup)
  test <- test_values(df, cls)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_na", {
  
  df <- data.frame(x = 1:4, y = c(NA, 6:8))
  
  setup <- list(df_name = "x", col_name = "x")
  cls <- class_test_na(setup) 
  test <- test_na(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "x", col_name = "y")
  cls <- class_test_na(setup) 
  test <- test_na(df, cls)
  testthat::expect_false(test$test_result)
  
})

testthat::test_that("test_unique", {
  
  df <- data.frame(x = 1:4, y = c(1,1,2:3), 
                   z = c(NA, NA, 1:2), w = c(NA, 1:3))
  
  setup <- list(df_name = "x", col_name = "x", na = FALSE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "x", col_name = "y", na = FALSE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_false(test$test_result)
  
  setup <- list(df_name = "x", col_name = "z", na = TRUE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "x", col_name = "z", na = FALSE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_false(test$test_result)
  
  setup <- list(df_name = "x", col_name = "w", na = TRUE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_true(test$test_result)
  
  setup <- list(df_name = "x", col_name = "w", na = FALSE)
  cls <- class_test_unique(setup) 
  test <- test_unique(df, cls)
  testthat::expect_true(test$test_result)
  
})