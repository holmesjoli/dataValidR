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


#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @return setup with test_result, test_message, test_description
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' values <- 1:4
#' na <- FALSE
#' setup <- setup_test_values("df_name", "x", values, na)
#' 
#' test <- test_values(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_values("df_name", "y", values, na)
#' test <- test_values(df, setup)
#' ## test$test_result returns FALSE
#' }
#' @export
test_values <- function(df, setup) {
  
  actual_values <- unique(df[[setup$col_name]])
  add_values <- setdiff(actual_values, setup$values)
  
  if (length(add_values) > 0) {
    
    setup$test_result <- FALSE
    setup$test_message <- paste0("FAILED with additional values in col: ", 
                                 paste(add_values, collapse = ","))
    
  } else {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  }
  
  return(setup)
}