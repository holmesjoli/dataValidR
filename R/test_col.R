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
  
  if (setup$na) {
    
    col <- df[[setup$col_name]][!is.na(df[[setup$col_name]])]
  } else {
    col <- df[[setup$col_name]]
  }
  
  if (length(unique(col)) == length(col)) {
      
    setup$test_result <- TRUE
    setup$test_message <- "PASSED"
    
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "FAILED: Not Unique"
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
    setup$test_message <- "PASSED"
    
  }
  
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
      setup$test_message <- "PASSED"
      
    } else {
      
      setup$test_result <- FALSE  
      setup$test_message <- "FAILED: Contains na values"
    }
    
  return(setup)
}
