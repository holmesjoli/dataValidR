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
                          col_name = col_name), class = "test_na")
  
  expc_params <- c("df_name", "col_name")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  
  return(setup)

}


#' @title Test NA values
#' @description Tests if the vector/column contains any NA values
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(NA, 6:8))
#' setup <- setup_test_na("df_name", "x") 
#' test <- test_na(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_na("df_name", "y") 
#' test <- test_na(df, setup)
#' ## test$test_result returns FALSE
#' }
#' @export
test_na <- function(df, setup) {
  
  if (all(!is.na(df[[setup$col_name]]))) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    
    setup$test_result <- FALSE  
    setup$test_message <- "FAILED: Contains na values"
  }
  
  return(setup)
}
