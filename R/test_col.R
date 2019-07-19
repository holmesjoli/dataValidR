## General column tests

#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @param vec the vector or column to test
#' @return boolean
#' @examples
#' \dontrun{
#' vec1 <- c(1,2,3)
#' vec2 <- c(1,1,2)
#' 
#' test_unique_test(vec1)
#' # Returns TRUE
#' 
#' test_unique_test(vec2)
#' ## Returns FALSE
#' }
test_unique_test <- function(vec) {
    
    if (length(unique(vec)) == length(vec)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @inherit test_unique_test return title
#' @description Tests if the vector/column is unique
#' @inheritParams test_unique_test
#' @family general column tests
#' @return vector
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(1,1,2)
#' 
#' test_unique(vec1)[2]
#' ## Returns PASS
#' 
#' test_unique(vec2)[2]
#' ## Returns ERROR 
#' @export
test_unique <- function(vec) {
    
    td <- "Unique Values"
    
    if (test_unique_test(vec)) {
        return(c(td, test_pass$ti, test_pass$tm))
    } else {
        return(c(td, test_fail$ti, "Not Unique"))
    }
    
}

#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @param df the dataframe
#' @inheritParams class_test_values
#' @return boolean
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' col_name <- "x"
#' values <- 1:4
#' na <- FALSE
#' 
#' test <- test_values(df, col_name, values, na)
#' ## Returns TRUE
#' 
#' col_name <- "y"
#' test <- test_values(df, col_name, values, na)
#' ## Returns FALSE
#' }
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
#' @param vec the vector or column to test
#' @return boolean
#' @examples
#' \dontrun{
#' vec1 <- c(1,2,3)
#' vec2 <- c(1,NA,2)
#' 
#' test_null_values_test(vec1)
#' # Returns TRUE
#' 
#' test_null_values_test(vec2)
#' ## Returns FALSE
#' }
test_null_values_test <- function(vec) {
    
    if (all(!is.na(vec))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_null_values_test return title
#' @description Tests if the vector/column contains any NA values
#' @inheritParams test_null_values_test
#' @family general column tests
#' @return vector
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(1,NA,2)
#' 
#' test_null_values(vec1)[2]
#' ## Returns PASS
#' 
#' test_null_values(vec2)[2]
#' ## Returns ERROR
#' @export
test_null_values <- function(vec) {
    
    td <- "Null Values"
    
    if (test_null_values_test(vec)) {
        return(c(td, test_pass$ti, test_pass$tm))
    } else {
        return(c(td, test_fail$ti, "Contains null values"))
    }
}
