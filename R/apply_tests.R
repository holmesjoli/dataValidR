#' @title Apply test
#' @export
apply_test <- function(df, setup) {
  UseMethod("apply_test", object = setup)

}

apply_test.default <- function(df, setup, ...) {

  warning(paste("apply_test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes test_na, test_unique, test_values, and test_range"))

}

#' @title Tests for NAs
#' @export
apply_test.test_na <- function(df, setup) {

  test <- test_na(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests for uniqueness
#' @export
apply_test.test_unique <- function(df, setup) {

  test <- test_unique(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests for Values
#' @export
apply_test.test_values <- function(df, setup) {

  test <- test_values(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests Range of Values
#' @export
apply_test.test_range <- function(df, setup) {

  test <- test_range(df, setup)
  test_result <- test_col(test)

  return(test_result)
}

#' @title Tests One to Many
#' @param df_left left dataset
#' @param df_right right dataset
#' @param setup from the merge setup class
#' @export
apply_test.test_orphan_rec <- function(primary_df, related_df, setup) {

  test <- test_orphan_rec(primary_df, related_df, setup)
  test_result <- test_merge(test)

  return(test_result)
}

