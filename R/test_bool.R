# Test boolean properties

test_all_true_test <- function(vec) {
  #' Tests all rows equal TRUE
  #' @param vec the vector or column to test
  #' @return boolean
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_all_true(vec)
  #' # FAILS
  #' df <- data.frame(col1 = c(T, T, T, T))
  #' test_all_true(df$col1)
  #' # PASSES
  
  if(all(vec)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

test_all_true <- function(vec) {
    #' Tests all rows equal TRUE
    #' @param vec the vector or column to test
    #' @return vector
    #' @examples
    #' vec <- c(T, F, T, F)
    #' test_all_true(vec)
    #' # FAILS
    #' df <- data.frame(col1 = c(T, T, T, T))
    #' test_all_true(df$col1)
    #' # PASSES
    
    td <- "Test all equal TRUE"

    if(test_all_true_test(vec)) {
        return(c(td, test_pass_ti, ""))
    } else {
        return(c(td, test_fail_ti, "Not all equal to TRUE"))
    }
}

test_all_false_test <- function(vec) {
  #' Tests all rows equal FALSE
  #' @param vec the vector or column to test
  #' @return boolean
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_all_false(vec)
  #' # FAILS
  #' df <- data.frame(col1 = c(F, F, F, F))
  #' test_all_false(df$col1)
  #' # PASSES
  
  if(any(vec)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

test_all_false <- function(vec) {
  #' Tests all rows equal FALSE
  #' @param vec the vector or column to test
  #' @return vector
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_all_false(vec)
  #' # FAILS
  #' df <- data.frame(col1 = c(F, F, F, F))
  #' test_all_false(df$col1)
  #' # PASSES
  
  td <- "Test all equal FALSE"
  
  if(any(vec)) {
    return(c(td, tess_fail_ti, "Not all equal to FALSE"))
  } else {
    return(c(td, test_pass_ti, ""))
  }
}

test_any_true_test <- function(vec) {
  #' Tests any rows equal TRUE
  #' @param vec the vector or column to test
  #' @return boolean
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_any_true(vec)
  #' # PASSES
  #' df <- data.frame(col1 = c(T, T, T, T))
  #' test_any_true(df$col1)
  #' # PASSES
  
  if(any(vec)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


test_any_true <- function(vec) {
  #' Tests any rows equal TRUE
  #' @param vec the vector or column to test
  #' @return vector
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_any_true(vec)
  #' # PASSES
  #' df <- data.frame(col1 = c(T, T, T, T))
  #' test_any_true(df$col1)
  #' # PASSES
  
  td <- "Test any equal TRUE"
  
  if(test_any_true_test(vec)) {
    return(c(td, test_pass_ti, ""))
  } else {
    return(c(td, test_fail_ti, "All are FALSE"))
  }
}

test_any_false_test <- function(vec) {
  #' Tests any rows equal FALSE
  #' @param vec the vector or column to test
  #' @return boolean
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_any_false(vec)
  #' # PASSES
  #' df <- data.frame(col1 = c(T, T, T, T))
  #' test_any_false(df$col1)
  #' # FAILS
  
  td <- "Test any equal FALSE"
  
  if(all(vec)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

test_any_false <- function(vec) {
  #' Tests any rows equal FALSE
  #' @param vec the vector or column to test
  #' @return vector
  #' @examples
  #' vec <- c(T, F, T, F)
  #' test_any_false(vec)
  #' # PASSES
  #' df <- data.frame(col1 = c(T, T, T, T))
  #' test_any_false(df$col1)
  #' # FAILS
  
  td <- "Test any equal FALSE"
  
  if(test_any_false_test(vec)) {
    return(c(td, tess_fail_ti, "All are TRUE"))
  } else {
    return(c(td, test_pass_ti, ""))
  }
}
