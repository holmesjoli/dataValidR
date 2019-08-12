testthat::test_that("Min NA", { 

  col <- c(1:3)
  
  testthat::expect_equal(min_na(col, TRUE), 1)
  testthat::expect_equal(min_na(col, FALSE), 1)

  col <- c(1,3, NA)  
  
  testthat::expect_equal(min_na(col, TRUE), 1)
  testthat::expect_true(is.na(min_na(col, FALSE)))
  
})

testthat::test_that("Max NA", { 
  
  col <- c(1:3)
  
  testthat::expect_equal(max_na(col, TRUE), 3)
  testthat::expect_equal(max_na(col, FALSE), 3)
  
  col <- c(1,3, NA)  
  
  testthat::expect_equal(max_na(col, TRUE), 3)
  testthat::expect_true(is.na(max_na(col, FALSE)))
  
})

testthat::test_that("Test Exclu lower", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  
  df_name = "x"; col_name = "x"; lower = 0; lower_inclu = FALSE;
  upper = NULL; upper_inclu = NULL; na = TRUE; int = TRUE
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper, na)

  test <- test_exclu_lower(df, setup)
  testthat::expect_true(test$test_result)
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper, na = FALSE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = TRUE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper, na = FALSE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper, na = TRUE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper, na = FALSE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = TRUE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = FALSE)

  test <- test_exclu_lower(df, setup)
  testthat::expect_false(test$test_result)

})

testthat::test_that("Test Inclu lower", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  
  df_name = "x"; col_name = "x"; lower = 0; lower_inclu = TRUE;
  upper = NULL; upper_inclu = NULL; na = TRUE; int = TRUE
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper, na)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper, na = FALSE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = TRUE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = FALSE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 0, upper, na = TRUE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper, na = FALSE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = TRUE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper, na = FALSE)

  test <- test_inclu_lower(df, setup)
  testthat::expect_false(test$test_result)

})

testthat::test_that("Test Exclu upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                na = TRUE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                na = FALSE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                na = TRUE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                na = FALSE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 5,
                na = TRUE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 5,
                na = FALSE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                na = TRUE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                na = FALSE)

  test <- test_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})

testthat::test_that("Test Inclu upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                na = TRUE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                na = FALSE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                na = TRUE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                na = FALSE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 5,
                na = TRUE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 5,
                na = FALSE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                na = TRUE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                na = FALSE)

  test <- test_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})

testthat::test_that("test_exclu_lower_exclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 1,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = FALSE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 0,
                na = FALSE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 1,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                lower = 1,
                na = TRUE)

  test <- test_exclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})


testthat::test_that("test_inclu_lower_exclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)


  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = TRUE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 1,
                na = FALSE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 0,
                na = FALSE)

  test <- test_inclu_lower_exclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})

testthat::test_that("test_inclu_lower_inclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 4,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = TRUE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = FALSE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 2,
                na = FALSE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                lower = 1,
                na = TRUE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 3,
                lower = 1,
                na = FALSE)

  test <- test_inclu_lower_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})

testthat::test_that("test_exclu_lower_inclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 4,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 5,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "x",
                upper = 4,
                lower = 1,
                na = TRUE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 0,
                na = TRUE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_true(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 0,
                na = FALSE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

  cls <- list(col_name = "y",
                upper = 4,
                lower = 1,
                na = FALSE)

  test <- test_exclu_lower_inclu_upper(df, cls)
  testthat::expect_false(test$test_result)

})

testthat::test_that("test_range", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                lower = 1,
                upper = 3,
                upper_inclu = TRUE,
                lower_inclu = TRUE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("inclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                lower = 1,
                upper = 3,
                upper_inclu = TRUE,
                lower_inclu = TRUE,
                na = FALSE)

  test <- test_range(df, cls)
  testthat::expect_true("inclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                lower = 1,
                upper = 3,
                upper_inclu = FALSE,
                lower_inclu = TRUE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("inclu_lower_exclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                lower = 1,
                upper = 3,
                upper_inclu = TRUE,
                lower_inclu = FALSE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("exclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                lower = 1,
                upper = 3,
                upper_inclu = FALSE,
                lower_inclu = FALSE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("exclu_lower_exclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = NULL,
                lower = 5,
                upper_inclu = NULL,
                lower_inclu = TRUE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("inclu_lower" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = NULL,
                lower = 5,
                upper_inclu = NULL,
                lower_inclu = FALSE,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("exclu_lower" %in% class(test))


  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                lower = NULL,
                upper_inclu = TRUE,
                lower_inclu = NULL,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  cls <- list(col_name = "x",
                upper = 5,
                lower = NULL,
                upper_inclu = FALSE,
                lower_inclu = NULL,
                na = TRUE)

  test <- test_range(df, cls)
  testthat::expect_true("exclu_upper" %in% class(test))

})

