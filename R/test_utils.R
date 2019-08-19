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
  assertthat::assert_that(all(is.character(value)), msg = msg)
}

#' @title Test params are both null or both not null
#' @description Tests that param1 and param2 both equal null or both are not equal to null
#' @param setup the class to check the parameters
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