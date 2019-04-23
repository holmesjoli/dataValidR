# Tests for numeric values vectors/columns

#' @title Test less than upper bound 
#' @description Tests if all the values in a column are less than an upper bound
#' @param vec the vector or column to test
#' @param upper the upper bound
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_value_test(vec, 4)
#' # Returns TRUE
#' 
#' test_less_than_value_test(vec, 3)
#' ## Returns FALSE
test_less_than_value_test <- function(vec, upper) {
    
    if (all(vec < upper)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @title Test less than upper bound 
#' @description Tests if all the values in a column are less than an upper bound
#' @inheritParams test_less_than_value_test 
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_value_test(vec, 4)[2]
#' # PASSES
#' 
#' test_less_than_value_test(vec, 3)[2]
#' # FAILS
test_less_than_value <- function(vec, upper) {
    
    td <- "Less than value (<)"
    
    if (test_less_than_value_test(vec, upper)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of upper bound"))
    }
    
}

#' @title Test less than or equal to upper bound 
#' @description Tests if all the values in a column are less than or equal to an upper bound
#' @inheritParams test_less_than_value_test 
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_or_equal_value_test(vec, 4)
#' # Returns TRUE
#' 
#' test_less_than_or_equal_value_test(vec, 3)
#' # Returns TRUE
#' 
#' test_less_than_or_equal_value_test(vec, 2)
#' # Returns FALSE
test_less_than_or_equal_value_test <- function(vec, upper) {
    
    if (all(vec <= upper)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @title Test less than or equal to upper bound 
#' @description Tests if all the values in a column are less than or equal to an upper bound
#' @inheritParams test_less_than_value_test 
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_or_equal_value_test(vec, 4)[2]
#' # PASSES
#' 
#' test_less_than_or_equal_value_test(vec, 3)[2]
#' # PASSES
#' 
#' test_less_than_or_equal_value_test(vec, 2)[2]
#' # FAILS
test_less_than_or_equal_value <- function(vec, upper) {
    
    td <- "Less than value (<=)"
    
    if (test_less_than_or_equal_value_test(vec, upper)) {
        
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of upper bound"))
    }
}
