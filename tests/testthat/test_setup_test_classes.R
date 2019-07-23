testthat::test_that("is_int returns TRUE for integers", {
  
  testthat::expect_true(is_int(1))
  testthat::expect_false(is_int(1.0))
  testthat::expect_false(is_int(1.1))
  testthat::expect_true(all(is_int(c(1,2,3))))
  testthat::expect_false(all(is_int(c(1,2,3.5))))
  
})

testthat::test_that("class_test_numeric_range : upper_inclu and lower_inclu have to be boolean", {
  
  df_name <- "x"; col_name <- "x"; 
  upper <- 5; lower <- 3
  upper_inclu <- TRUE; lower_inclu <- TRUE; na = TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper_inclu <- TRUE; lower_inclu <- FALSE
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper_inclu <- FALSE; lower_inclu <- TRUE
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper_inclu <- FALSE; lower_inclu <- FALSE
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper_inclu <- FALSE; lower_inclu <- "X"
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                            upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper_inclu <- TRUE; lower_inclu <- "X"
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper_inclu <- "X"; lower_inclu <- TRUE
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper_inclu <- "X"; lower_inclu <- FALSE
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())

})

testthat::test_that("class_test_numeric_range : either upper_inclu or lower_inclu must not be NULL", {
  
  df_name <- "x"; col_name <- "x"; 
  upper <- NULL; lower <- 3
  upper_inclu <- NULL; lower_inclu <- TRUE; na = TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper <- NULL; lower <- 3
  upper_inclu <- NULL; lower_inclu <- FALSE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper <- 3; lower <- NULL
  upper_inclu <- TRUE; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper <- 3; lower <- NULL
  upper_inclu <- FALSE; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper <- NULL; lower <- NULL
  upper_inclu <- NULL; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- 3; lower <- NULL
  upper_inclu <- NULL; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- NULL; lower <- NULL
  upper_inclu <- TRUE; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- NULL; lower <- NULL
  upper_inclu <- NULL; lower_inclu <- FALSE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- NULL; lower <- 1
  upper_inclu <- NULL; lower_inclu <- NULL
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
})

testthat::test_that("class_test_numeric  upper and lower are numeric or NULL", {
  
  df_name <- "x"; col_name <- "x"; 
  upper <- NULL; lower <- 3
  upper_inclu <- NULL; lower_inclu <- TRUE; na = TRUE;
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  upper <- 5; lower <- 3
  upper_inclu <- TRUE; lower_inclu <- TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("numeric"))
  
  
  upper <- 5; lower <- "X"
  upper_inclu <- TRUE; lower_inclu <- TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  
  upper <- "X"; lower <- 3
  upper_inclu <- TRUE; lower_inclu <- TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- c(1,2); lower <- 3
  upper_inclu <- TRUE; lower_inclu <- TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- 3; lower <- c(1,2)
  upper_inclu <- TRUE; lower_inclu <- TRUE
  
  testthat::expect_that(class_test_numeric_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
})

testthat::test_that("class_test_integer: test only takes integers", {
  
  col_name <- "x"; upper <- NULL; lower <- 3
  upper_inclu <- NULL; lower_inclu <- TRUE; na = TRUE;
  
  testthat::expect_that(class_test_integer_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("integer"))
  
  upper <- 3; lower <- NULL
  upper_inclu <- TRUE; lower_inclu <- NULL
  
  testthat::expect_that(class_test_integer_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::is_a("integer"))
  
  upper <- 3.5; lower <- NULL
  upper_inclu <- TRUE; lower_inclu <- NULL
  
  testthat::expect_that(class_test_integer_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- NULL; lower <- 3.5
  upper_inclu <- NULL; lower_inclu <- FALSE
  
  testthat::expect_that(class_test_integer_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
  upper <- 7.5; lower <- 3.5
  upper_inclu <- TRUE; lower_inclu <- FALSE
  
  testthat::expect_that(class_test_integer_range(df_name, col_name, 
                                           upper_inclu, lower_inclu, upper, lower, na), 
                        testthat::throws_error())
  
})

testthat::test_that("class_test_values tests", {
  
  col_name <- "X"; values <- c(1,2,3); na <- NA
  
  testthat::expect_that(class_test_values(df_name, col_name, values, na), 
                        testthat::throws_error())
  
  col_name <- "X"; values <- c(1,2,3); na <- TRUE
  
  values <- class_test_values(df_name, col_name, values, na)$values
  
  testthat::expect_true(NA %in% values)
  
  col_name <- "X"; values <- c(1,2,3); na <- FALSE
  
  values <- class_test_values(df_name, col_name, values, na)$values
  
  testthat::expect_false(NA %in% values)
  
})
