testthat::test_that("is_int returns TRUE for integers", {
  
  testthat::expect_true(is_int(1))
  testthat::expect_false(is_int(1.0))
  testthat::expect_false(is_int(1.1))
  testthat::expect_false(is_int(TRUE))
  testthat::expect_false(is_int(FALSE))
  testthat::expect_true(all(is_int(c(1,2,3))))
  testthat::expect_false(all(is_int(c(1,2,3.5))))
  
})

testthat::test_that("expc_params_check", {
  
  expc_params <- c("df_name", "col_name", "na")
  cls <- list(df_name = "x", col_name = "x")
  testthat::expect_error(test_expc_params(cls, expc_params))
  
})

testthat::test_that("test_param_logical", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_logical(cls, "t"))
  testthat::expect_error(test_param_logical(cls, "u"))
  testthat::expect_silent(test_param_logical(cls, "v"))
  testthat::expect_silent(test_param_logical(cls, "w"))
  testthat::expect_error(test_param_logical(cls, "x"))
  testthat::expect_error(test_param_logical(cls, "y"))
  testthat::expect_error(test_param_logical(cls, "z"))
  
})

testthat::test_that("test_param_logical_or_null", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_silent(test_param_logical_or_null(cls, "t"))
  testthat::expect_error(test_param_logical_or_null(cls, "u"))
  testthat::expect_silent(test_param_logical_or_null(cls, "v"))
  testthat::expect_silent(test_param_logical_or_null(cls, "w"))
  testthat::expect_error(test_param_logical_or_null(cls, "z"))
  testthat::expect_error(test_param_logical_or_null(cls, "y"))

})

testthat::test_that("test_param_numeric", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_numeric(cls, "t"))
  testthat::expect_error(test_param_numeric(cls, "u"))
  testthat::expect_error(test_param_numeric(cls, "v"))
  testthat::expect_error(test_param_numeric(cls, "w"))
  testthat::expect_silent(test_param_numeric(cls, "x"))
  testthat::expect_silent(test_param_numeric(cls, "y"))
  testthat::expect_error(test_param_numeric(cls, "z"))
  
})

testthat::test_that("test_param_numeric_or_null", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_numeric_or_null(cls, "u"))
  testthat::expect_error(test_param_numeric_or_null(cls, "v"))
  testthat::expect_error(test_param_numeric_or_null(cls, "w"))
  testthat::expect_silent(test_param_numeric_or_null(cls, "x"))
  testthat::expect_silent(test_param_numeric_or_null(cls, "y"))
  testthat::expect_error(test_param_numeric_or_null(cls, "z"))
  
})

testthat::test_that("test_param_integer_or_null", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_integer_or_null(cls, "u"))
  testthat::expect_error(test_param_integer_or_null(cls, "v"))
  testthat::expect_error(test_param_integer_or_null(cls, "w"))
  testthat::expect_error(test_param_integer_or_null(cls, "x"))
  testthat::expect_silent(test_param_integer_or_null(cls, "y"))
  testthat::expect_error(test_param_integer_or_null(cls, "z"))
  
})

testthat::test_that("test_param_string", {
  
  cls <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_string(cls, "t"))
  testthat::expect_error(test_param_string(cls, "u"))
  testthat::expect_error(test_param_string(cls, "v"))
  testthat::expect_error(test_param_string(cls, "w"))
  testthat::expect_error(test_param_string(cls, "x"))
  testthat::expect_silent(test_param_string(cls, "z"))
  
})

testthat::test_that("test_params_both_null_or_not", {
  
  cls <- list(x = NULL, y = "z")
  testthat::expect_error(test_params_both_null_or_not(cls, "x", "y"))
  testthat::expect_silent(test_params_both_null_or_not(cls, "x", "x"))
  testthat::expect_silent(test_params_both_null_or_not(cls, "y", "y"))
  
})

testthat::test_that("test_params_both_not_null", {
  
  cls <- list(x = NULL, y = "z")
  testthat::expect_error(test_params_both_not_null(cls, "x", "x"))
  testthat::expect_silent(test_params_both_not_null(cls, "x", "y"))
  testthat::expect_silent(test_params_both_not_null(cls, "y", "x"))
  testthat::expect_silent(test_params_both_not_null(cls, "y", "y"))
  
})

testthat::test_that("setup_test_values tests", {
  
  df_name <- "x"; col_name <- "X"
  values <- c(1,2,3); na <- NA
  
  expect_error(setup_test_values(df_name, col_name, values, na))
  
  cls <- setup_test_values(df_name, col_name, values, na  = TRUE)
  testthat::expect_true(NA %in% cls$values)
  
  cls <- setup_test_values(df_name, col_name, values, na = FALSE)
  testthat::expect_false(NA %in% cls$values)
  
})
