#' @title tests to see if X is an integer
#' @param x a scalar or a vector
#' @return TRUE or FALSE
#' @export
is_int <- function(x) {
    
    if (!is.null(x) & !is.logical(x)) {
        x%%1 == 0
    } else {
        return(FALSE)
    }
}

#' @title Test the expected parameters exist in the class
#' @param setup the class to check for the parameters in
#' @param expc_params the expected parameters
test_expc_params <- function(setup, expc_params) {
  msg <- paste0("Test configuration missing parameter(s): ", paste(setdiff(expc_params, names(setup)), collapse = ", "))
  assertthat::assert_that(all(expc_params %in% names(setup)), msg = msg)

}

#' @title Test param is logical
#' @param setup the class to check the parameters
#' @param param string, the parameter to check
test_param_logical <- function(setup, param) {
    value <- setup[[param]]
    msg <- paste0("Parameter '", param, "' only takes the values TRUE or FALSE")
    assertthat::assert_that(!is.null(value), msg = msg)
    assertthat::assert_that(!is.na(value), msg = msg)
    assertthat::assert_that(is.logical(value), msg = msg)
}

#' @title Test param is logical or NULL
#' @inheritParams test_param_logical
test_param_logical_or_null <- function(setup, param) {
  value <- setup[[param]]
  msg <- paste0("Parameter '", param, "' only takes the values TRUE or FALSE or NULL")
  assertthat::assert_that((is.logical(value) | is.null(value)), msg = msg)
}

#' @title Test param is numeric
#' @inheritParams test_param_logical
test_param_numeric <- function(setup, param) {
   value <- setup[[param]]
   msg <- paste0("Parameter '", param, "' must be numeric")
   assertthat::assert_that(is.numeric(value), msg = msg)
}

#' @title Test param is numeric or NULL
#' @inheritParams test_param_logical
test_param_numeric_or_null <- function(setup, param) {
  value <- setup[[param]]
  msg <- paste0("Parameter '", param, "' must be numeric or NULL")
  assertthat::assert_that(is.numeric(value) | is.null(value), msg = msg)
}

#' @title Test param is integer or NULL
#' @inheritParams test_param_logical
test_param_integer_or_null <- function(setup, param) {
  value <- setup[[param]]
  msg <- paste0("Parameter '", param, "' must be integer or NULL")
  assertthat::assert_that((is_int(value) | is.null(value)), msg = msg)
}

#' @title Test param is string
#' @inheritParams test_param_logical
test_param_string <- function(setup, param) {
  value <- setup[[param]]
  msg <- paste0("Parameter '", param, "' must be a string")
  assertthat::assert_that(is.character(value), msg = msg)
}

#' @title Test params are both null or both not null
#' @description Tests that param1 and param2 both equal null or both are not equal to null
#' @param param1 first param to test
#' @param param2 second param to test
test_params_both_null_or_not <- function(setup, param1, param2) {
  msg <- paste0(param1, " and ", param2, " must both be null or both be not null")
  log <- (is.null(setup[[param1]]) & is.null(setup[[param2]])) | 
         (!is.null(setup[[param1]]) & !is.null(setup[[param2]]))
  assertthat::assert_that(log, msg = msg)
}

#' @title Test two params are both not null
#' @inheritParams test_params_both_null_or_not
test_params_both_not_null <- function(setup, param1, param2) {
  msg <- paste0(param1, " and ", param2, " must both be not null")
  log <-  !is.null(setup[[param1]]) | !is.null(setup[[param2]])
  assertthat::assert_that(log, msg = msg)
}

#' @title Setup Test Range
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @param int is a boolean value, if the values should be integers or not
#' @param upper_inclu takes on the values TRUE, FALSE, or NULL. If there's an upper bound then it should be set to TRUE or FALSE.
#' If the upper bound is inclusive, x <= upper, then upper_inclu = TRUE, else upper_inclu = FALSE.
#' @param lower_inclu takes on the values TRUE, FALSE, or NULL. If there's an lower bound then it should be set to TRUE or FALSE.
#' If the lower bound is inclusive, x >= lower, then lower_inclu = TRUE, else lower_inclu = FALSE.
#' @param upper can take a numeric value or NULL. If there's no upper bound, then upper should be null.
#' @param lower can take a numeric value or NULL. If there's no lower bound, then lower should be null.
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @export
setup_test_range <- function(df_name, col_name, int, lower_inclu, 
                             upper_inclu, lower, upper, na) {
  
  setup <- structure(list(test_category = "Accuracy",
                          test_name = "test_range", 
                          test_desc = "Test Range",
                          df_name = df_name,
                          col_name = col_name,
                          int = int,
                          lower_inclu = lower_inclu,
                          upper_inclu = upper_inclu,
                          lower = lower,
                          upper = upper,
                          na = na), class = "test_range")
  
  expc_params <- c("df_name", "col_name", "int", "upper_inclu", 
                   "lower_inclu", "upper", "lower", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "int")
  test_param_logical_or_null(setup, "upper_inclu")
  test_param_logical_or_null(setup, "lower_inclu")
  test_param_logical(setup, "na")
  test_param_numeric_or_null(setup, "upper")
  test_param_numeric_or_null(setup, "lower")
  test_params_both_null_or_not(setup, "upper", "upper_inclu")
  test_params_both_null_or_not(setup, "lower", "lower_inclu")
  
  if (setup$int) {
    test_param_integer_or_null(setup, "upper")
    test_param_integer_or_null(setup, "lower")
    class(setup) <- append(class(setup), "integer")
  } else {
    class(setup) <- append(class(setup), "double")
  }
  
  return(setup)
}

#' @title Setup Test Values
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @param values is a list of values that expected in the column to be tested. 
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @export
setup_test_values <- function(df_name, col_name, values, na) {
  
  setup <- structure(list(test_category = "Consistency",
                          test_name = "test_values",
                          test_desc = "Expected Values",
                          df_name = df_name,
                          col_name = col_name,
                          values = values,
                          na = na), class = "test_values")
  
  expc_params <- c("df_name", "col_name", "values", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "na")
  
  if (setup$na) {
    setup$values <- c(setup$values, NA)
  }
  
  return(setup)
}

#' @title Setup Test Unique
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @export
setup_test_unique <- function(df_name, col_name, na) {
  
  setup <- structure(list(test_category = "Uniqueness",
                          test_name = "test_unique",
                          test_desc = "Test Unique",
                          df_name = df_name,
                          col_name = col_name,
                          na = na), class = "test_unique")
  
  expc_params <- c("df_name", "col_name", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "na")
  
  return(setup)
}

#' @title Setup Test Na
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @export
setup_test_na <- function(df_name, col_name) {
  
  setup <- structure(list(test_category = "Completeness",
                          test_name = "test_na",
                          test_desc = "Test NA",
                          df_name = df_name,
                          col_name = col_name), class = "test_na")
  
  expc_params <- c("df_name", "col_name")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  
  return(setup)
  
  
}
