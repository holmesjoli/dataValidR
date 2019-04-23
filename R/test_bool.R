# Test boolean properties

#' @title Tests all values equal true
#' @param vec the vector or column to test
#' @return boolean
#' @examples
#' vec <- c(T, F, T, F)
#' test_all_true(vec)
#' # FAILS
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_all_true(df$col1)
#' # PASSES
test_all_true_test <- function(vec) {
    
    if (all(vec)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

#' @title  Test all values equal true
#' @inheritParams test_all_true_test
#' @family boolean tests
#' @return vector
#' @examples
#' vec <- c(T, F, T, F)
#' test_all_true(vec)[2]
#' # FAILS
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_all_true(df$col1)[2]
#' # PASSES

test_all_true <- function(vec) {

    td <- "Test all equal TRUE"
    
    if (test_all_true_test(vec)) {
        return(c(td, test_pass_ti, ""))
    } else {
        return(c(td, test_fail_ti, "Not all equal to TRUE"))
    }
}

#' @title Test all values equal false
#' @inheritParams test_all_true_test
#' @return boolean
#' @examples
#' vec <- c(T, F, T, F)
#' test_all_false(vec)
#' # FAILS
#' df <- data.frame(col1 = c(F, F, F, F))
#' test_all_false(df$col1)
#' # PASSES
test_all_false_test <- function(vec) {

    
    if (any(vec)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
#' @title Test all values equal false
#' @inheritParams test_all_true_test
#' @family boolean tests
#' @return vector
#' @examples
#' vec <- c(T, F, T, F)
#' test_all_false(vec)[2]
#' # FAILS
#' df <- data.frame(col1 = c(F, F, F, F))
#' test_all_false(df$col1)[2]
#' # PASSES
test_all_false <- function(vec) {
    
    td <- "Test all equal FALSE"
    
    if (test_all_false_test(vec)) {
        return(c(td, tess_fail_ti, "Not all equal to FALSE"))
    } else {
        return(c(td, test_pass_ti, ""))
    }
}

#' @title Tests any values equal TRUE
#' @inheritParams test_all_true_test
#' @return boolean
#' @examples
#' vec <- c(T, F, T, F)
#' test_any_true(vec)
#' # PASSES
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_any_true(df$col1)
#' # PASSES
test_any_true_test <- function(vec) {
    
    if (any(vec)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @title Tests any values equal TRUE
#' @inheritParams test_all_true_test
#' @family boolean tests
#' @return vector
#' @examples
#' vec <- c(T, F, T, F)
#' test_any_true(vec)[2]
#' # PASSES
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_any_true(df$col1)[2]
#' # PASSES
test_any_true <- function(vec) {
    
    td <- "Test any equal TRUE"
    
    if (test_any_true_test(vec)) {
        return(c(td, test_pass_ti, ""))
    } else {
        return(c(td, test_fail_ti, "All are FALSE"))
    }
}

#' @title Tests any values equal FALSE
#' @inheritParams test_all_true_test
#' @return boolean
#' @examples
#' vec <- c(T, F, T, F)
#' test_any_false(vec)
#' # PASSES
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_any_false(df$col1)
#' # FAILS
test_any_false_test <- function(vec) {
    
    if (all(vec)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' @title Tests any values equal FALSE
#' @inheritParams test_all_true_test
#' @family boolean tests
#' @return vector
#' @examples
#' vec <- c(T, F, T, F)
#' test_any_false(vec)[2]
#' # PASSES
#' df <- data.frame(col1 = c(T, T, T, T))
#' test_any_false(df$col1)[2]
#' # FAILS
test_any_false <- function(vec) {
    
    td <- "Test any equal FALSE"
    
    if (test_any_false_test(vec)) {
        return(c(td, tess_fail_ti, "All are TRUE"))
    } else {
        return(c(td, test_pass_ti, ""))
    }
}
