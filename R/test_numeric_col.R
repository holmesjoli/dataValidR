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
#' @param df the dataframe
#' @param cls the setup class for testing
#' @export
test_exclu_lower <- function(df, cls) {
  
  cls$test_desc <- "Test x > lower"
  class(cls) <- append(class(cls), "exclu_lower")

  mn <- min_na(df[[cls$col_name]], cls$na)
  
  if (!is.na(mn)) {
  
    if (mn > cls$lower) {
      cls$test_result <- TRUE
      cls$test_message <- "PASSED"
    } else {
      cls$test_result <- FALSE
      cls$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than ", cls$lower)
    }
  } else {
    cls$test_result <- FALSE
    cls$test_message <- "NAs present but NA was set to FALSE"
  }
  return(cls)
}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower <- function(df, cls) {
  
  cls$test_desc <- "Test x >= lower"
  class(cls) <- append(class(cls), "inclu_lower")
  
  mn <- min_na(df[[cls$col_name]], cls$na)
  
  if (!is.na(mn)) {
    
    if (mn >= cls$lower) {
      cls$test_result <- TRUE
      cls$test_message <- "PASSED"
    } else {
      cls$test_result <- FALSE
      cls$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than or equal to", cls$lower)
    }
  } else {
    cls$test_result <- FALSE
    cls$test_message <- "NAs present but NA was set to FALSE"
  }
  return(cls)
}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_upper <- function(df, cls) {
  
  cls$test_desc <- "Test x < upper"
  class(cls) <- append(class(cls), "exclu_upper")
  
  mx <- max_na(df[[cls$col_name]], cls$na)
  
  if (!is.na(mx)) {
    
    if (mx < cls$upper) {
      cls$test_result <- TRUE
      cls$test_message <- "PASSED"
    } else {
      cls$test_result <- FALSE
      cls$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than ", cls$upper)
    }
  } else {
    cls$test_result <- FALSE
    cls$test_message <- "NAs present but NA was set to FALSE"
  }
  return(cls)
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_upper <- function(df, cls) {
  
  cls$test_desc <- "Test x <= upper"
  class(cls) <- append(class(cls), "inclu_upper")
  
  mx <- max_na(df[[cls$col_name]], cls$na)
  
  if (!is.na(mx)) {
    
    if (mx <= cls$upper) {
      cls$test_result <- TRUE
      cls$test_message <- "PASSED"
    } else {
      cls$test_result <- FALSE
      cls$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than or equal to ", cls$upper)
    }
  } else {
    cls$test_result <- FALSE
    cls$test_message <- "NAs present but NA was set to FALSE"
  }
  return(cls)
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_lower_exclu_upper <- function(df, cls) {

  cls$test_desc <- "Test lower < x < upper"
  class(cls) <- append(class(cls), "exclu_lower_exclu_upper")
  
  el <- test_exclu_lower(df, cls)
  eu <- test_exclu_upper(df, cls)
  
  if (el$test_result & eu$test_result) {
    
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
  
  } else {
    
    cls$test_result <- FALSE
    
    if (el$test_result) {
      cls$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      cls$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", el$test_message)
    } else {
      cls$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(cls)
}

#' @title Test inclu lower and exclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower_exclu_upper <- function(df, cls) {
  
  cls$test_desc <- "Test lower <= x < upper"
  class(cls) <- append(class(cls), "inclu_lower_exclu_upper")
  
  il <- test_inclu_lower(df, cls)
  eu <- test_exclu_upper(df, cls)
  
  if (il$test_result & eu$test_result) {
    
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
    
  } else {
    
    cls$test_result <- FALSE
    
    if (il$test_result) {
      cls$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      cls$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", il$test_message)
    } else {
      cls$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(cls)
}

#' @title Test inclu lower and inclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less or equal to than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower_inclu_upper <- function(df, cls) {
  
  cls$test_desc <- "Test lower <= x <= upper"
  class(cls) <- append(class(cls), "inclu_lower_inclu_upper")
  
  il <- test_inclu_lower(df, cls)
  iu <- test_inclu_upper(df, cls)
  
  if (il$test_result & iu$test_result) {
    
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
    
  } else {
    
    cls$test_result <- FALSE
    
    if (il$test_result) {
      cls$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      cls$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", il$test_message)
    } else {
      cls$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(cls)
}


#' @title Test exclu lower and inclu upper
#' @description Tests that the column is greater thanto the lower bound AND less or equal to than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_lower_inclu_upper <- function(df, cls) {
  
  cls$test_desc <- "Test lower < x <= upper"
  class(cls) <- append(class(cls), "exclu_lower_inclu_upper")
  
  el <- test_exclu_lower(df, cls)
  iu <- test_inclu_upper(df, cls)
  
  if (el$test_result & iu$test_result) {
    
    cls$test_result <- TRUE
    cls$test_message <- "PASSED"
    
  } else {
    
    cls$test_result <- FALSE
    
    if (el$test_result) {
      cls$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      cls$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", el$test_message)
    } else {
      cls$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(cls)
}


#' @title Assign the correct numeric test
#' @description Assigns the correct numeric test depending on the parameters
#' @inheritParams test_exclu_lower
assign_numeric_class <- function(df, cls) {
  
  if (!is.null(cls$upper) & !is.null(cls$lower)) {
    
    if (cls$upper_inclu & cls$lower_inclu) {
      cls <- test_inclu_lower_inclu_upper(df, cls) 
    } else if (cls$upper_inclu & !cls$lower_inclu) {
      cls <- test_exclu_lower_inclu_upper(df, cls) 
    } else if (!cls$upper_inclu & cls$lower_inclu) {
      cls <- test_inclu_lower_exclu_upper(df, cls)
    } else {
      cls <- test_exclu_lower_exclu_upper(df, cls)
    }
    
  } else if (!is.null(cls$upper) & is.null(cls$lower)) {
    
     if (cls$upper_inclu) {
       cls <- test_inclu_upper(df, cls)
     } else {
       cls <- test_exclu_upper(df, cls)
     }
    
  } else if (is.null(cls$upper) & !is.null(cls$lower)) {
    
    if (cls$lower_inclu) {
      cls <- test_inclu_lower(df, cls)
    } else {
      cls <- test_exclu_lower(df, cls)
    }
    
  }
  return(cls)
}


#' @title Test integer range
#' @description Tests the integer range based on the parameter configurations
#' @param df the dataframe
#' @inheritParams class_test_numeric_range
#' @export
test_integer_range <- function(df, col_name, upper_inclu, lower_inclu, 
                               upper, lower, na) {
  
  cls <- class_test_integer_range(col_name, upper_inclu, lower_inclu, 
                                    upper, lower, na)
  cls <- assign_numeric_class(df, cls)
  
  return(cls)
}

#' @title Test double range
#' @description Tests the double range based on the parameter configurations
#' @param df the dataframe
#' @inheritParams class_test_numeric_range
#' @export
test_double_range <- function(df, col_name, upper_inclu, lower_inclu, 
                              upper, lower, na) {
  
  cls <- class_test_double_range(col_name, upper_inclu, lower_inclu, 
                                   upper, lower, na)
  cls <- assign_numeric_class(df, cls)
  
  return(cls)

}