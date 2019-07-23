#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @param df the dataframe
#' @param cls the setup class for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' cls <- class_test_unique("df_name", "x", FALSE) 
#' 
#' test <- test_unique(df, cls)
#' ## test$test_result returns TRUE
#' 
#' cls <- class_test_unique("df_name", "y", FALSE) 
#' test <- test_unique(df, cls)
#' ## test$test_result returns FALSE
#' }
#' @export
test_unique <- function(df, cls) {
  
  if (cls$na) {
    
    col <- df[[cls$col_name]][!is.na(df[[cls$col_name]])]
  } else {
    col <- df[[cls$col_name]]
  }
  
  if (length(unique(col)) == length(col)) {
      
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
    
  } else {
    cls$test_result <- FALSE
    cls$test_message <- "FAILED: Not Unique"
  }
  
  return(cls)
}

#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @param df the dataframe
#' @param cls the setup class for testing
#' @return class with test_result, test_message, test_description
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' values <- 1:4
#' na <- FALSE
#' cls <- class_test_values("df_name", "x", values, na)
#' 
#' test <- test_values(df, cls)
#' ## test$test_result returns TRUE
#' 
#' cls <- class_test_values("df_name", "y", values, na)
#' test <- test_values(df, cls)
#' ## test$test_result returns FALSE
#' }
#' @export
test_values <- function(df, cls) {
  
  actual_values <- unique(df[[cls$col_name]])
  add_values <- setdiff(actual_values, cls$values)
  
  if (length(add_values) > 0) {
    
    cls$test_result <- FALSE
    cls$test_message <- paste0("FAILED with additional values in col: ", 
                                 paste(add_values, collapse = ","))
    
  } else {
   
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
    
  }
  
  return(cls)
}


#' @title Test NA values
#' @description Tests if the vector/column contains any NA values
#' @param df the dataframe
#' @param cls the setup class for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(NA, 6:8))
#' cls <- class_test_na("df_name", "x") 
#' test <- test_na(df, cls)
#' ## test$test_result returns TRUE
#' 
#' cls <- class_test_na("df_name", "y") 
#' test <- test_na(df, cls)
#' ## test$test_result returns FALSE
#' }
#' @export
test_na <- function(df, cls) {
  
    if (all(!is.na(df[[cls$col_name]]))) {
        
      cls$test_result <- TRUE
      cls$test_message <- "PASSED"
      
    } else {
      
      cls$test_result <- FALSE  
      cls$test_message <- "FAILED: Contains na values"
    }
    
  return(cls)
}
