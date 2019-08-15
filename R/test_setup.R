#' @title Setup for tests for orphaned records
#' @param primary_df the name of the primary dataframe
#' @param related_df the name of the related dataframe
#' @param primary_key the column or vector of columns to merge on
#' @param foreign_key the column or vector of columns to merge on
setup_test_orphan_rec <- function(primary_df, related_df, primary_key, foreign_key) {
  
  setup <- structure(list(test_category = "Completeness",
                          test_name = "test_merge",
                          test_desc = "Test Merge",
                          primary_df = primary_df,
                          related_df = related_df,
                          primary_key = primary_key,
                          foreign_key = foreign_key), class = c("orphan_rec", "merge"))
  
  test_param_string(setup, "primary_df")
  test_param_string(setup, "related_df")
  test_param_string(setup, "primary_key")
  test_param_string(setup, "foreign_key")
  
  return(setup)
  
}

#' @title Setup Test Na
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @examples
#' df <- data.frame(x = 1:10, y = c(rep("X", 4), rep("Y", 4), rep("Z", 2)))
#' setup <- setup_test_na(df_name = "df", col_name = "y")
#' @export
setup_test_na <- function(df_name, col_name) {
  
  setup <- structure(list(test_category = "Completeness",
                          test_name = "test_na",
                          test_desc = "Test NA",
                          df_name = df_name,
                          col_name = col_name), class = c("na", "column"))
  
  
  expc_params <- c("df_name", "col_name")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  
  return(setup)
  
}

#' @title Setup Test Unique
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string or vector of column names. If more than one column is included they will be concatenated and tested.
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @examples
#' df <- data.frame(x = 1:10, y = c(rep("X", 4), rep("Y", 4), rep("Z", 2)))
#' setup <- setup_test_unique(df_name = "df", col_name = "x", na = FALSE)
#' @export
setup_test_unique <- function(df_name, col_name, na) {
  
  setup <- structure(list(test_category = "Uniqueness",
                          test_name = "test_unique",
                          test_desc = "Test Unique",
                          df_name = df_name,
                          col_name = col_name,
                          na = na), class = c("unique", "column"))
  
  expc_params <- c("df_name", "col_name", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "na")
  
  return(setup)
}

#' @title Setup Test Values
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @param values is a list of values that expected in the column to be tested. 
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @examples
#' df <- data.frame(x = 1:10, y = c(rep("X", 4), rep("Y", 4), rep("Z", 2)))
#' setup <- setup_test_values(df_name = "df", col_name = "y", values = c("X", "Y"), na = FALSE)
#' @export
setup_test_values <- function(df_name, col_name, values, na) {
  
  setup <- structure(list(test_category = "Consistency",
                          test_name = "test_values",
                          test_desc = "Expected Values",
                          df_name = df_name,
                          col_name = col_name,
                          values = values,
                          na = na), class = c("values", "column"))
  
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
                          na = na), class = c("range", "column"))
  
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
  
  if (!is.null(setup$upper) & !is.null(setup$lower)) {
    
    if (setup$upper_inclu & setup$lower_inclu) {
      class(setup) <- append(class(setup), "inclu_lower_inclu_upper")
    } else if (setup$upper_inclu & !setup$lower_inclu) {
      class(setup) <- append(class(setup), "exclu_lower_inclu_upper")
    } else if (!setup$upper_inclu & setup$lower_inclu) {
      class(setup) <- append(class(setup), "inclu_lower_exclu_upper")
    } else {
      class(setup) <- append(class(setup), "exclu_lower_exclu_upper")
    }
    
  } else if (!is.null(setup$upper) & is.null(setup$lower)) {
    
    if (setup$upper_inclu) {
      class(setup) <- append(class(setup), "inclu_upper")
    } else {
      class(setup) <- append(class(setup), "exclu_upper")
    }
    
  } else if (is.null(setup$upper) & !is.null(setup$lower)) {
    
    if (setup$lower_inclu) {
      class(setup) <- append(class(setup), "inclu_lower")
    } else {
      class(setup) <- append(class(setup), "exclu_lower")
    }
    
  }

  return(setup)
}
