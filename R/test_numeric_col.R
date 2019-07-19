#' @title Min with NA
#' @description Returns the minimum with NA's removed if na = TRUE
#' @param col the column to test
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @export
min_na <- function(col, na) {
  
  if (na) {
    mn <- min(col, na.rm = T)
  } else {
    mn <- min(col)
  }
  return(mn)
}

#' @title Max with NA
#' @description Returns the maximum with NA's removed if na = TRUE
#' @param col the column to test
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @export
max_na <- function(col, na) {
  
  if (na) {
    mx <- max(col, na.rm = T)
  } else {
    mx <- max(col)
  }
  return(mx)
}

#' @title Test Exclu lower
#' @description Tests that column is greater than the lower bound
#' @param lower the lower bound
#' @inheritParams min_na
#' @export
test_exclu_lower <- function(df, class) {
  
  class$test_desc <- "Test x > lower"
  class(class) <- append(class(class), "exclu_lower")

  mn <- min_na(df[[class$col_name]], class$na)
  
  if (!is.na(mn)) {
  
    if (mn > class$lower) {
      class$test_result <- TRUE
      class$test_message <- "PASSED"
    } else {
      class$test_result <- FALSE
      class$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than ", class$lower)
    }
  } else {
    class$test_result <- FALSE
    class$test_message <- "NAs present but NA was set to FALSE"
  }
  return(class)
}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower <- function(df, class) {
  
  class$test_desc <- "Test x >= lower"
  class(class) <- append(class(class), "inclu_lower")
  
  mn <- min_na(df[[class$col_name]], class$na)
  
  if (!is.na(mn)) {
    
    if (mn >= class$lower) {
      class$test_result <- TRUE
      class$test_message <- "PASSED"
    } else {
      class$test_result <- FALSE
      class$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than or equal to", class$lower)
    }
  } else {
    class$test_result <- FALSE
    class$test_message <- "NAs present but NA was set to FALSE"
  }
  return(class)
}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @param col the column to test
#' @param upper the upper bound
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @export
test_exclu_upper <- function(df, class) {
  
  class$test_desc <- "Test x < upper"
  class(class) <- append(class(class), "exclu_upper")
  
  mx <- max_na(df[[class$col_name]], class$na)
  
  if (!is.na(mx)) {
    
    if (mx < class$upper) {
      class$test_result <- TRUE
      class$test_message <- "PASSED"
    } else {
      class$test_result <- FALSE
      class$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than ", class$upper)
    }
  } else {
    class$test_result <- FALSE
    class$test_message <- "NAs present but NA was set to FALSE"
  }
  return(class)
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test_exclu_upper
#' @export
test_inclu_upper <- function(df, class) {
  
  class$test_desc <- "Test x <= upper"
  class(class) <- append(class(class), "inclu_upper")
  
  mx <- max_na(df[[class$col_name]], class$na)
  
  if (!is.na(mx)) {
    
    if (mx <= class$upper) {
      class$test_result <- TRUE
      class$test_message <- "PASSED"
    } else {
      class$test_result <- FALSE
      class$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than or equal to ", class$upper)
    }
  } else {
    class$test_result <- FALSE
    class$test_message <- "NAs present but NA was set to FALSE"
  }
  return(class)
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test_exclu_lower
#' @inheritParams test_exclu_upper
#' @export
test_exclu_lower_exclu_upper <- function(df, class) {

  class$test_desc <- "Test lower < x < upper"
  class(class) <- append(class(class), "exclu_lower_exclu_upper")
  
  el <- test_exclu_lower(df, class)
  eu <- test_exclu_upper(df, class)
  
  if (el$test_result & eu$test_result) {
    
    class$test_result <- TRUE
    class$test_message <- "PASSED"
  
  } else {
    
    class$test_result <- FALSE
    
    if (el$test_result) {
      class$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      class$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", el$test_message)
    } else {
      class$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(class)
}

#' @title Test inclu lower and exclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less than the upper bound
#' @inheritParams test_inclu_lower
#' @inheritParams test_exclu_upper
#' @export
test_inclu_lower_exclu_upper <- function(df, class) {
  
  class$test_desc <- "Test lower <= x < upper"
  class(class) <- append(class(class), "inclu_lower_exclu_upper")
  
  il <- test_inclu_lower(df, class)
  eu <- test_exclu_upper(df, class)
  
  if (il$test_result & eu$test_result) {
    
    class$test_result <- TRUE
    class$test_message <- "PASSED"
    
  } else {
    
    class$test_result <- FALSE
    
    if (il$test_result) {
      class$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      class$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", il$test_message)
    } else {
      class$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(class)
}

#' @title Test inclu lower and inclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less or equal to than the upper bound
#' @inheritParams test_inclu_lower
#' @inheritParams test_inclu_upper
#' @export
test_inclu_lower_inclu_upper <- function(df, class) {
  
  class$test_desc <- "Test lower <= x <= upper"
  class(class) <- append(class(class), "inclu_lower_inclu_upper")
  
  il <- test_inclu_lower(df, class)
  iu <- test_inclu_upper(df, class)
  
  if (il$test_result & iu$test_result) {
    
    class$test_result <- TRUE
    class$test_message <- "PASSED"
    
  } else {
    
    class$test_result <- FALSE
    
    if (il$test_result) {
      class$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      class$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", il$test_message)
    } else {
      class$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(class)
}


#' @title Test exclu lower and inclu upper
#' @description Tests that the column is greater thanto the lower bound AND less or equal to than the upper bound
#' @inheritParams test_exclu_lower
#' @inheritParams test_inclu_upper
#' @export
test_exclu_lower_inclu_upper <- function(df, class) {
  
  class$test_desc <- "Test lower < x <= upper"
  class(class) <- append(class(class), "exclu_lower_inclu_upper")
  
  el <- test_exclu_lower(df, class)
  iu <- test_inclu_upper(df, class)
  
  if (el$test_result & iu$test_result) {
    
    class$test_result <- TRUE
    class$test_message <- "PASSED"
    
  } else {
    
    class$test_result <- FALSE
    
    if (el$test_result) {
      class$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      class$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", el$test_message)
    } else {
      class$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(class)
}



assign_numeric_class <- function(df, class) {
  
  if (!is.null(class$upper) & !is.null(class$lower)) {
    
    if (class$upper_inclu & class$lower_inclu) {
      class <- test_inclu_lower_inclu_upper(df, class) 
    } else if (class$upper_inclu & !class$lower_inclu) {
      class <- test_exclu_lower_inclu_upper(df, class) 
    } else if (!class$upper_inclu & class$lower_inclu) {
      class <- test_inclu_lower_exclu_upper(df, class)
    } else {
      class <- test_exclu_lower_exclu_upper(df, class)
    }
    
  } else if (!is.null(class$upper) & is.null(class$lower)) {
    
     if (class$upper_inclu) {
       class <- test_inclu_upper(df, class)
     } else {
       class <- test_exclu_upper(df, class)
     }
    
  } else if (is.null(class$upper) & !is.null(class$lower)) {
    
    if (class$lower_inclu) {
      class <- test_inclu_lower(df, class)
    } else {
      class <- test_exclu_lower(df, class)
    }
    
  }
  return(class)
}


#' @title Test integer range
#' @description Tests the integer range based on the parameter configurations
#' @param df the dataframe
#' @inheritParams class_test_numeric_range
#' @export
test_integer_range <- function(df, col_name, upper_inclu, lower_inclu, 
                               upper, lower, na) {
  
  class <- class_test_integer_range(col_name, upper_inclu, lower_inclu, 
                                    upper, lower, na)
  class <- assign_numeric_class(df, class)
  
  return(class)
}

#' @title Test double range
#' @description Tests the double range based on the parameter configurations
#' @param df the dataframe
#' @inheritParams class_test_numeric_range
#' @export
test_double_range <- function(df, col_name, upper_inclu, lower_inclu, 
                              upper, lower, na) {
  
  class <- class_test_double_range(col_name, upper_inclu, lower_inclu, 
                                   upper, lower, na)
  class <- assign_numeric_class(df, class)
  
  return(class)

}