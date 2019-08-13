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
                          na = na), class = "test_unique")
  
  expc_params <- c("df_name", "col_name", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "na")
  
  return(setup)
}


#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' setup <- setup_test_unique("df_name", "x", FALSE) 
#' 
#' test <- test_unique(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_unique("df_name", "y", FALSE) 
#' test <- test_unique(df, setup)
#' ## test$test_result returns FALSE
#' }
#' @export
test_unique <- function(df, setup) {

  if (length(setup$col_name) > 1) {
    col <- do.call(paste, c(df[setup$col_name], sep="-"))
    setup$col_name <- paste(col_name, collapse = ", ")
    
  } else {
    col <- df[[setup$col_name]]
  }
  
  if (setup$na) {
    col <- col[!is.na(col)]
  }
  
  if (length(unique(col)) == length(col)) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "FAILED: Not Unique"
  }
  
  return(setup)
}
