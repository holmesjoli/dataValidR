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
  setup <- list(df_name = "x", col_name = "x")
  testthat::expect_error(test_expc_params(setup, expc_params))
  
})

testthat::test_that("test_param_logical", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_logical(setup, "t"))
  testthat::expect_error(test_param_logical(setup, "u"))
  testthat::expect_silent(test_param_logical(setup, "v"))
  testthat::expect_silent(test_param_logical(setup, "w"))
  testthat::expect_error(test_param_logical(setup, "x"))
  testthat::expect_error(test_param_logical(setup, "y"))
  testthat::expect_error(test_param_logical(setup, "z"))
  
})

testthat::test_that("test_param_logical_or_null", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_silent(test_param_logical_or_null(setup, "t"))
  testthat::expect_error(test_param_logical_or_null(setup, "u"))
  testthat::expect_silent(test_param_logical_or_null(setup, "v"))
  testthat::expect_silent(test_param_logical_or_null(setup, "w"))
  testthat::expect_error(test_param_logical_or_null(setup, "z"))
  testthat::expect_error(test_param_logical_or_null(setup, "y"))
  
})

testthat::test_that("test_param_numeric", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_numeric(setup, "t"))
  testthat::expect_error(test_param_numeric(setup, "u"))
  testthat::expect_error(test_param_numeric(setup, "v"))
  testthat::expect_error(test_param_numeric(setup, "w"))
  testthat::expect_silent(test_param_numeric(setup, "x"))
  testthat::expect_silent(test_param_numeric(setup, "y"))
  testthat::expect_error(test_param_numeric(setup, "z"))
  
})

testthat::test_that("test_param_numeric_or_null", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_numeric_or_null(setup, "u"))
  testthat::expect_error(test_param_numeric_or_null(setup, "v"))
  testthat::expect_error(test_param_numeric_or_null(setup, "w"))
  testthat::expect_silent(test_param_numeric_or_null(setup, "x"))
  testthat::expect_silent(test_param_numeric_or_null(setup, "y"))
  testthat::expect_error(test_param_numeric_or_null(setup, "z"))
  
})

testthat::test_that("test_param_integer_or_null", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_integer_or_null(setup, "u"))
  testthat::expect_error(test_param_integer_or_null(setup, "v"))
  testthat::expect_error(test_param_integer_or_null(setup, "w"))
  testthat::expect_error(test_param_integer_or_null(setup, "x"))
  testthat::expect_silent(test_param_integer_or_null(setup, "y"))
  testthat::expect_error(test_param_integer_or_null(setup, "z"))
  
})

testthat::test_that("test_param_string", {
  
  setup <- list(t = NULL, u = NA, v = FALSE, w = TRUE, x = 1.1, y = 1, z = "X")
  testthat::expect_error(test_param_string(setup, "t"))
  testthat::expect_error(test_param_string(setup, "u"))
  testthat::expect_error(test_param_string(setup, "v"))
  testthat::expect_error(test_param_string(setup, "w"))
  testthat::expect_error(test_param_string(setup, "x"))
  testthat::expect_silent(test_param_string(setup, "z"))
  
})

testthat::test_that("test_params_both_null_or_not", {
  
  setup <- list(x = NULL, y = "z")
  testthat::expect_error(test_params_both_null_or_not(setup, "x", "y"))
  testthat::expect_silent(test_params_both_null_or_not(setup, "x", "x"))
  testthat::expect_silent(test_params_both_null_or_not(setup, "y", "y"))
  
})

testthat::test_that("test_params_both_not_null", {
  
  setup <- list(x = NULL, y = "z")
  testthat::expect_error(test_params_both_not_null(setup, "x", "x"))
  testthat::expect_silent(test_params_both_not_null(setup, "x", "y"))
  testthat::expect_silent(test_params_both_not_null(setup, "y", "x"))
  testthat::expect_silent(test_params_both_not_null(setup, "y", "y"))
  
})
