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

#' @inherit test_greater_than_value_test return title
#' @description Tests if all the values in a column are less than an upper bound
#' @inheritParams test_less_than_value_test 
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_value_test(vec, 4)[2]
#' ## Returns PASS
#' 
#' test_less_than_value_test(vec, 3)[2]
#' ## Returns ERROR 
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

#' @inherit test_less_than_or_equal_value_test return title
#' @description Tests if all the values in a column are less than or equal to an upper bound
#' @inheritParams test_less_than_value_test 
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_less_than_or_equal_value_test(vec, 4)[2]
#' ## Returns PASS
#' 
#' test_less_than_or_equal_value_test(vec, 3)[2]
#' ## Returns PASS
#' 
#' test_less_than_or_equal_value_test(vec, 2)[2]
#' ## Returns ERROR
test_less_than_or_equal_value <- function(vec, upper) {
    
    td <- "Less than value (<=)"
    
    if (test_less_than_or_equal_value_test(vec, upper)) {
        
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of upper bound"))
    }
}

#' @title Test greater than lower bound 
#' @description Tests if all the values in a column are greater than a lower bound
#' @param vec the vector or column to test
#' @param lower the lower bound
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_greater_than_value_test(vec, 0)
#' # Returns TRUE
#' 
#' test_greater_than_value_test(vec, 1)
#' ## Returns FALSE
test_greater_than_value_test <- function(vec, lower) {
    
    if (all(vec > lower)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_greater_than_value_test return title
#' @description Tests if all the values in a column are greater than a lower bound
#' @inheritParams test_greater_than_value_test
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_greater_than_value(vec, 0)[2]
#' # Returns PASS
#' 
#' test_greater_than_value(vec, 1)[2]
#' ## Returns ERROR
test_greater_than_value <- function(vec, lower) {
    
    td <- "Less than value (>)"
    
    if (test_greater_than_value_test(vec, lower)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of lower bound"))
    }
    
}

#' @title Test greater than or equal to lower bound 
#' @description Tests if all the values in a column are greater than or equal to a lower bound
#' @inheritParams test_greater_than_value_test 
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_greater_than_value_test(vec, 0)
#' # Returns TRUE
#' 
#' test_greater_than_value_test(vec, 1)
#' ## Returns TRUE
#' 
#' test_greater_than_value_test(vec, 2)
#' ## Returns FALSE
test_greater_than_or_equal_value_test <- function(vec, lower) {
    
    if (all(vec >= lower)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_greater_than_value_test return title
#' @description Tests if all the values in a column are greater than an lower bound
#' @inheritParams test_greater_than_value_test
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_greater_than_value(vec, 0)[2]
#' # Returns PASS
#' 
#' test_greater_than_value(vec, 1)[2]
#' ## Returns PASS
#' 
#' test_greater_than_value(vec, 2)[2]
#' ## Returns ERROR
test_greater_than_or_equal_value <- function(vec, lower) {
    
    td <- "Less than value (>=)"
    
    if (test_greater_than_or_equal_value_test(vec, lower)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of lower bound"))
    }
    
}

#' @title Test exclusive bounds range
#' @description Tests if all the values in a column between an upper and lower bound 
#' @param lower the lower bound
#' @param upper the upper bound
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_exclu_exclu_range_test(vec, 0, 4)
#' ## Returns TRUE
#' 
#' test_exclu_exclu_range_test(vec, 0, 3)
#' ## Returns FALSE
#' 
#' test_exclu_exclu_range_test(vec, 1, 3)
#' ## Returns FALSE
test_exclu_value_range_test <- function(vec, lower, upper) {
    
    mn <- min(vec)
    mx <- max(vec)
    
    if (mn > lower & mx < upper) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_exclu_value_range_test return title
#' @description Tests if all the values in a column between an upper and lower bound 
#' @inheritParams test_exclu_value_range_test
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3,4,5)
#' 
#' test_exclu_value_range(vec, 2, 4)
#' # Returns PASS
#' 
#' test_exclu_value_range(vec, 1, 4)
#' ## Returns ERROR
#' 
#' test_exclu_value_range(vec, 2, 5)
#' ## Returns ERROR
test_exclu_value_range <- function(vec, lower, upper) {
    
    td <- "Exclusive Range (lower < X < upper)"
    
    if (test_exclu_value_range_test(vec, lower, upper)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of range"))
    }
    
}

#' @title Test inclusive bounds range
#' @description Tests if all the values in a column between an upper and lower bound or equal to upper/lower bound
#' @inheritParams test_exclu_value_range_test
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_inclu_value_range_test(vec, 0, 4)
#' # Returns TRUE
#' 
#' test_inclu_value_range_test(vec, 0, 3)
#' ## Returns FALSE
#' 
#' test_inclu_value_range_test(vec, 1, 4)
#' ## Returns FALSE
test_inclu_value_range_test <- function(vec, lower, upper) {
    
    mn <- min(vec)
    mx <- max(vec)
    
    if (mn >= lower & mx <= upper) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_inclu_value_range_test return title
#' @description Tests if all the values are less than the upper and greater than the lower bound or equal to upper/lower bound
#' @inheritParams test_exclu_value_range_test
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_inclu_value_range(vec, 1, 3)
#' # Returns PASS
#' 
#' test_inclu_value_range(vec, 1, 4)
#' ## Returns PASS
#' 
#' test_inclu_value_range(vec, 2, 5)
#' ## Returns ERROR
test_inclu_value_range <- function(vec, lower, upper) {
    
    td <- "Inclusive Range (lower <= X <= upper)"
    
    if (test_inclu_value_range_test(vec, lower, upper)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Values outside of range"))
    }
    
}

#' @title Test exclusive lower bound, inclusive upper bound
#' @description Tests if all the values in a column are less than the lower bound and greater than or equal to the upper bound
#' @inheritParams test_exclu_value_range_test
#' @return boolean
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_exclu_lower_inclu_range_test(vec, 0, 3)
#' ## Returns TRUE
#' 
#' test_exclu_lower_inclu_range_test(vec, 1, 3)
#' ## Returns FALSE
#' 
#' test_exclu_lower_inclu_range_test(vec, 0, 4)
#' ## Returns FALSE
test_exclu_lower_inclu_upper_range_test <- function(vec, lower, upper) {
    
    mn <- min(vec)
    mx <- max(vec)
    
    if (mn > lower & mx <= upper) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @inherit test_exclu_lower_inclu_upper_range_test return title
#' @description Tests if all the values are less than the upper and greater than the lower bound or equal to upper/lower bound
#' @inheritParams test_exclu_value_range_test
#' @family numeric column tests
#' @return vector
#' @examples
#' vec <- c(1,2,3)
#' 
#' test_exclu_lower_inclu_upper_range(vec, 0, 3)
#' ## Returns PASS
#' 
#' test_exclu_lower_inclu_upper_range(vec, 1, 3)
#' ## Returns ERROR
#' 
#' test_exclu_lower_inclu_upper_range(vec, 0, 2)
#' ## Returns ERROR
test_exclu_lower_inclu_upper_range <- function(vec, lower, upper) {
    
    td <- "Exclusive lower bound, inclusive upper bound (lower < X <= upper)"
    
    if (test_exclu_lower_inclu_upper_range_test(vec, lower, upper)) {
        return(c(td, test_pass_ti, test_pass_tm))
    } else {
        return(c(td, test_fail_ti, "Exclusive lower bound, inclusive upper bound (lower < X <= upper)"))
    }
}


