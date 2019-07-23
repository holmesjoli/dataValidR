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
#' @param cls the class to check for the parameters in
#' @param expc_params the expected parameters
test_expc_params <- function(cls, expc_params) {
  msg <- paste0("Test configuration missing parameter(s): ", paste(setdiff(expc_params, names(cls)), collapse = ", "))
  assertthat::assert_that(all(expc_params %in% names(cls)), msg = msg)

}

#' @title Test param is logical
#' @param cls the class to check the parameters
#' @param param string, the parameter to check
test_param_logical <- function(cls, param) {
    value <- cls[[param]]
    msg <- paste0("Parameter '", param, "' only takes the values TRUE or FALSE")
    assertthat::assert_that(!is.null(value), msg = msg)
    assertthat::assert_that(!is.na(value), msg = msg)
    assertthat::assert_that(is.logical(value), msg = msg)
}

#' @title Test param is logical or NULL
#' @inheritParams test_param_logical
test_param_logical_or_null <- function(cls, param) {
  value <- cls[[param]]
  msg <- paste0("Parameter '", param, "' only takes the values TRUE or FALSE or NULL")
  assertthat::assert_that((is.logical(value) | is.null(value)), msg = msg)
}

#' @title Test param is numeric
#' @inheritParams test_param_logical
test_param_numeric <- function(cls, param) {
   value <- cls[[param]]
   msg <- paste0("Parameter '", param, "' must be numeric")
   assertthat::assert_that(is.numeric(value), msg = msg)
}

#' @title Test param is numeric or NULL
#' @inheritParams test_param_logical
test_param_numeric_or_null <- function(cls, param) {
  value <- cls[[param]]
  msg <- paste0("Parameter '", param, "' must be numeric or NULL")
  assertthat::assert_that(is.numeric(value) | is.null(value), msg = msg)
}

#' @title Test param is integer or NULL
#' @inheritParams test_param_logical
test_param_integer_or_null <- function(cls, param) {
  value <- cls[[param]]
  msg <- paste0("Parameter '", param, "' must be integer or NULL")
  assertthat::assert_that((is_int(value) | is.null(value)) & !is.na(value), msg = msg)
}

#' @title Test param is string
#' @inheritParams test_param_logical
test_param_string <- function(cls, param) {
  value <- cls[[param]]
  msg <- paste0("Parameter '", param, "' must be a string")
  assertthat::assert_that(is.character(value), msg = msg)
}

#' @title Test params are both null or both not null
#' @description Tests that param1 and param2 both equal null or both are not equal to null
#' @param param1 first param to test
#' @param param2 second param to test
test_params_both_null_or_not <- function(cls, param1, param2) {
  msg <- paste0(param1, " and ", param2, " must both be null or both be not null")
  log <- (is.null(cls[[param1]]) & is.null(cls[[param2]])) | 
         (!is.null(cls[[param1]]) & !is.null(cls[[param2]]))
  assertthat::assert_that(log, msg = msg)
}

#' @title Test two params are both not null
#' @inheritParams test_params_both_null_or_not
test_params_both_not_null <- function(cls, param1, param2) {
  msg <- paste0(param1, " and ", param2, " must both be not null")
  log <-  !is.null(cls[[param1]]) | !is.null(cls[[param2]])
  assertthat::assert_that(log, msg = msg)
}

#' @title Class for range
#' @param cls the general test setup cls
class_test_range <- function(cls) {
    
    expc_params <- c("df_name", "col_name", "int", "upper_inclu", 
                     "lower_inclu", "upper", "lower", "na")
    test_expc_params(cls, expc_params)
    test_param_string(cls, "df_name")
    test_param_string(cls, "col_name")
    test_param_logical(cls, "int")
    test_param_logical_or_null(cls, "upper_inclu")
    test_param_logical_or_null(cls, "lower_inclu")
    test_param_logical(cls, "na")
    test_param_numeric_or_null(cls, "upper")
    test_param_numeric_or_null(cls, "lower")
    test_params_both_null_or_not(cls, "upper", "upper_inclu")
    test_params_both_null_or_not(cls, "lower", "lower_inclu")
    
    if (cls$int) {
        test_param_integer_or_null(cls, "upper")
        test_param_integer_or_null(cls, "lower")
        class(cls) <- append(class(cls), "integer")
    } else {
        class(cls) <- append(class(cls), "double")
    }
    
    cls$test_desc <- "Test Range"
    class(cls) <- append(class(cls), "test_range")
    
    return(cls)
}

#' @title Class to test unique
#' @param cls the general test setup cls
#' @details if NA is set to TRUE, columns with multiple NAs will not be marked as non-unique, NA rows will be ignored
#' @export
class_test_unique <- function(cls) {
    
    expc_params <- c("df_name", "col_name")
    test_expc_params(cls, expc_params)
    test_param_string(cls, "df_name")
    test_param_string(cls, "col_name")
    test_param_logical(cls, "na")
    
    cls$test_desc <- "Test Unique"
    class(cls) <- append(class(cls), "test_unique")
    
    return(cls)
}

#' @title Class to test NA
#' @param cls the general test setup cls
#' @export
class_test_na <- function(cls) {
    
    expc_params <- c("df_name", "col_name")
    test_expc_params(cls, expc_params)
    test_param_string(cls, "df_name")
    test_param_string(cls, "col_name")
    
    cls$test_desc <- "Test NA"
    class(cls) <- append(class(cls), "test_na")
    
    return(cls)
}

#' @title Class to test values
#' @param cls the general test setup cls
#' @details if cls$na is TRUE then it gets added to the list of acceptable values
#' @export
class_test_values <- function(cls) {
    
    expc_params <- c("df_name", "col_name", "values", "na")
    test_expc_params(cls, expc_params)
    test_param_string(cls, "df_name")
    test_param_string(cls, "col_name")
    test_param_logical(cls, "na")
    
    if (cls$na) {
        cls$values <- c(cls$values, NA)
    }
    
    cls$test_desc <- "Expected Values"
    class(cls) <- append(class(cls), "test_values")
    
    return(cls)
}

#' @title Class to test boolean
#' @param cls the general test setup cls
#' @export
class_test_bool <- function(cls) {
    
    expc_params <- c("df_name", "col_name", "na")
    test_expc_params(cls, expc_params)
    test_param_string(cls, "df_name")
    test_param_string(cls, "col_name")
    test_param_logical(cls, "na")
    
    
    cls$test_desc <- "Boolean Values"
    class(cls) <- append(class(cls), "test_boolean")
    
    return(cls)
}
