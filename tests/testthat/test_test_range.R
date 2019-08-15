testthat::test_that("Test Exclu lower", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  
  df_name = "x"; col_name = "x"; lower = 0; lower_inclu = FALSE;
  upper = NULL; upper_inclu = NULL; int = TRUE
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)


  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

})

testthat::test_that("Test Inclu lower", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  
  df_name = "x"; col_name = "x"; lower = 0; lower_inclu = TRUE;
  upper = NULL; upper_inclu = NULL; int = TRUE
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper)


  test <- test(setup, df)
  testthat::expect_true(test$test_result)

})

testthat::test_that("Test Exclu upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = NULL
  upper_inclu = FALSE;lower = NULL; upper = 5
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper = 4)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower, upper = 5)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower, upper = 3)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

})

testthat::test_that("Test Inclu upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = NULL
  upper_inclu = TRUE; lower = NULL; upper = 5

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)
  
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper = 4)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower, upper = 5)
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower, upper = 3)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

})

testthat::test_that("exclu_lower_exclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = FALSE
  upper_inclu = FALSE; lower = 0; upper = 5
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper = 4)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 5)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 5)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 4)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 4)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 3)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 3)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

})


testthat::test_that("inclu_lower_exclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = TRUE
  upper_inclu = FALSE; lower = 1; upper = 5
 
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)
  
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 0, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 4)
  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 4)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 3)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 4)
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

})

testthat::test_that("inclu_lower_inclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))

  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = TRUE
  upper_inclu = TRUE; lower = 1; upper = 4
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 5)
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 2, upper = 5)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 3)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

})

testthat::test_that("exclu_lower_inclu_upper", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))

  df_name = "x"; col_name = "x"; int = TRUE; lower_inclu = FALSE
  upper_inclu = TRUE; lower = 0; upper = 4
  
  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper)
  
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower, upper = 5)

  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name, int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 5)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

  setup <- setup_test_range(df_name, col_name = "y", int, lower_inclu, 
                            upper_inclu, lower = 0, upper = 4)
  test <- test(setup, df)
  testthat::expect_true(test$test_result)

  setup <- setup_test_range(df_name, col_name = "x", int, lower_inclu, 
                            upper_inclu, lower = 1, upper = 4)

  test <- test(setup, df)
  testthat::expect_false(test$test_result)

})

testthat::test_that("setup_test_range", {

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",
                col_name = "x",
                int = TRUE,
                lower = 1,
                upper = 3,
                upper_inclu = TRUE,
                lower_inclu = TRUE)

  test <- test(setup, df)
  testthat::expect_true("inclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",            
                col_name = "x",
                int = TRUE,
                lower = 1,
                upper = 3,
                upper_inclu = TRUE,
                lower_inclu = TRUE)

  test <- test(setup, df)
  testthat::expect_true("inclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",            
                col_name = "x",
                int = TRUE,
                lower = 1,
                upper = 3,
                upper_inclu = FALSE,
                lower_inclu = TRUE)

  test <- test(setup, df)
  testthat::expect_true("inclu_lower_exclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",            
                col_name = "x",
                lower = 1,
                upper = 3,
                int = TRUE,
                upper_inclu = TRUE,
                lower_inclu = FALSE)

  test <- test(setup, df)
  testthat::expect_true("exclu_lower_inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",
                col_name = "x",
                int = TRUE,
                lower = 1,
                upper = 3,
                upper_inclu = FALSE,
                lower_inclu = FALSE)

  test <- test(setup, df)
  testthat::expect_true("exclu_lower_exclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",
                col_name = "x",
                int = TRUE,
                upper = NULL,
                lower = 5,
                upper_inclu = NULL,
                lower_inclu = TRUE)

  test <- test(setup, df)
  testthat::expect_true("inclu_lower" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",
                col_name = "x",
                int = TRUE,
                upper = NULL,
                lower = 5,
                upper_inclu = NULL,
                lower_inclu = FALSE)

  test <- test(setup, df)
  testthat::expect_true("exclu_lower" %in% class(test))


  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",    
                col_name = "x",
                int = TRUE,
                upper = 5,
                lower = NULL,
                upper_inclu = TRUE,
                lower_inclu = NULL)

  test <- test(setup, df)
  testthat::expect_true("inclu_upper" %in% class(test))

  df <- data.frame(x = 1:4, y = c(1:3, NA))
  setup <- setup_test_range(
                df_name = "x",
                col_name = "x",
                int = TRUE,
                upper = 5,
                lower = NULL,
                upper_inclu = FALSE,
                lower_inclu = NULL)

  test <- test(setup, df)
  testthat::expect_true("exclu_upper" %in% class(test))

})

