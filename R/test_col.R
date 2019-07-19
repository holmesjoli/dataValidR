#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @param df the dataframe
#' @inheritParams class_test_unique
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' 
#' test <- test_unique_test(df, "x")
#' ## test$test_result returns TRUE
#' 
#' test <- test_unique_test(df, "y)
#' ## test$test_result returns FALSE
#' }
#' @export
test_unique <- function(df, col_name) {
    
  class <- class_test_unique(col_name)
  
    if (length(unique(df[[class$col_name]])) == length(df[[class$col_name]])) {
        
      class$test_result <- TRUE
      class$test_message <- "PASSED"
      
    } else {
      class$test_result <- FALSE
      class$test_message <- "FAILED: Not Unique"
    }
  
  return(class)
}

#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @param df the dataframe
#' @inheritParams class_test_values
#' @return class with test_result, test_message, test_description
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' values <- 1:4
#' na <- FALSE
#' 
#' test <- test_values(df, "x", values, na)
#' ## test$test_result returns TRUE
#' 
#' test <- test_values(df, "y", values, na)
#' ## test$test_result returns FALSE
#' }
#' @export
test_values <- function(df, col_name, values, na) {
  
  class <- class_test_values(col_name, values, na) 
  
  actual_values <- unique(df[[class$col_name]])
  add_values <- setdiff(actual_values, class$values)
  
  if (length(add_values) > 0) {
    
    class$test_result <- FALSE
    class$test_message <- paste0("FAILED with additional values in col", 
                                 paste(add_values, collapse = ","))
    
  } else {
   
    class$test_result <- TRUE
    class$test_message <- "PASSED"
    
  }
  
  return(class)
}


#' @title Test NA values
#' @description Tests if the vector/column contains any NA values
#' @param df the dataframe
#' @inheritParams class_test_na
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(NA, 6:8))
#' 
#' test_na(df, "x")
#' ## test$test_result returns TRUE
#' 
#' test_na(df, "y")
#' ## test$test_result returns FALSE
#' }
#' @export
test_na <- function(df, col_name) {
    
  class <- class_test_na(col_name)
  
    if (all(!is.na(df[[class$col_name]]))) {
        
      class$test_result <- TRUE
      class$test_message <- "PASSED"
      
    } else {
      
      class$test_result <- FALSE  
      class$test_message <- "FAILED: Contains null values"
    }
    
  return(class)
}
