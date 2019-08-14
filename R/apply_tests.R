#' @title Apply test
#' @param setup the setup class
apply_test <- function(setup, ...) {
  UseMethod("apply_test", object = setup)

}

apply_test.default <- function(setup, ...) {

  warning(paste("apply_test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes test_na, test_unique, test_values, test_range, and test_orphan_rec"))

}

#' @title Tests for NAs
#' @esetupport
apply_test.test_na <- function(setup, df, ...) {

  test <- test_na(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests for uniqueness
#' @esetupport
apply_test.test_unique <- function(setup, df, ...) {

  test <- test_unique(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests for Values
#' @esetupport
apply_test.test_values <- function(setup, df, ...) {

  test <- test_values(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests Range of Values
#' @esetupport
apply_test.test_range <- function(setup, df, ...) {

  test <- test_range(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests One to Many
#' @param df_left left dataset
#' @param df_right right dataset
#' @param setup from the merge setup class
#' @esetupport
apply_test.test_orphan_rec <- function(setup, primary_df, related_df, ...) {

  test <- test_orphan_rec(primary_df, related_df, setup)
  test_result <- test_merge(test)

  return(test_result)
}

