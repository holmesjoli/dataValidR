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
  assertthat::assert_that((is_int(value) | is.null(value)) & !is.na(value), msg = msg)
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

#' @title Class for range
#' @param setup the general test setup setup
class_test_range <- function(setup) {
    
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
    
    setup$test_desc <- "Test Range"
    class(setup) <- append(class(setup), "test_range")
    
    return(setup)
}

#' @title Class to test unique
#' @param setup the general test setup
#' @details if NA is set to TRUE, columns with multiple NAs will not be marked as non-unique, NA rows will be ignored
#' @export
class_test_unique <- function(setup) {
    
    expc_params <- c("df_name", "col_name")
    test_expc_params(setup, expc_params)
    test_param_string(setup, "df_name")
    test_param_string(setup, "col_name")
    test_param_logical(setup, "na")
    
    setup$test_desc <- "Test Unique"
    class(setup) <- append(class(setup), "test_unique")
    
    return(setup)
}

#' @title Class to test NA
#' @param setup the general test setup
#' @export
class_test_na <- function(setup) {
    
    expc_params <- c("df_name", "col_name")
    test_expc_params(setup, expc_params)
    test_param_string(setup, "df_name")
    test_param_string(setup, "col_name")
    
    setup$test_desc <- "Test NA"
    class(setup) <- append(class(setup), "test_na")
    
    return(setup)
}

#' @title Class to test values
#' @param setup the general test setup
#' @details if setup$na is TRUE then it gets added to the list of acceptable values
#' @export
class_test_values <- function(setup) {
    
    expc_params <- c("df_name", "col_name", "values", "na")
    test_expc_params(setup, expc_params)
    test_param_string(setup, "df_name")
    test_param_string(setup, "col_name")
    test_param_logical(setup, "na")
    
    if (setup$na) {
        setup$values <- c(setup$values, NA)
    }
    
    setup$test_desc <- "Expected Values"
    class(setup) <- append(class(setup), "test_values")
    
    return(setup)
}

#' @title Class to test boolean
#' @param setup the general test setup setup
#' @export
class_test_bool <- function(setup) {
    
    expc_params <- c("df_name", "col_name", "na")
    test_expc_params(setup, expc_params)
    test_param_string(setup, "df_name")
    test_param_string(setup, "col_name")
    test_param_logical(setup, "na")
    
    
    setup$test_desc <- "Boolean Values"
    class(setup) <- append(class(setup), "test_boolean")
    
    return(setup)
}
