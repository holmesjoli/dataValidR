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
#' @param add_values a list additional values
#' @return boolean
#' @examples
#' \dontrun{
#' test_values_test(c())
#' # Returns TRUE
#' 
#' test_values_test(c(4,5))
#' ## Returns FALSE
#' }
test_values_test <- function(add_values) {
    
    if (length(add_values) == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_values_test return title
#' @description Tests if the vector/column contains values other than expected
#' @param vec the vector or column to test
#' @param expc_values a vector of expected values 
#' @family general column tests
#' @return vector
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(1,1,2)
#' expc_values <- c(1, 2)
#' 
#' test_values(vec2, expc_values)[2]
#' ## Returns PASS
#' 
#' test_values(vec1, expc_values)[2]
#' ## Returns ERROR
#' @export
test_values <- function(vec, expc_values) {
    
    td <- "Expected Values"
    add_values <- setdiff(unique(vec), expc_values)
    
    if (test_values_test(add_values)) {
        return(c(td, test_pass$ti, test_pass$tm))
    } else {
        return(c(td, test_fail$ti, paste0("Additional values in col", paste(add_values, collapse = ","))))
    }
    
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
