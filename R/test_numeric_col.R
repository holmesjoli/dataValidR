#' @title Min with NA
#' @description Returns the minimum with NA's removed if na = TRUE
#' @param col the column to test
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
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
test_exclu_lower <- function(col, lower, na) {
  
  mn <- min_na(col, na)
  
  if (mn > lower) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test_exclu_lower
test_inclu_lower <- function(col, lower, na) {
  
  mn <- min_na(col, na)
  
  if (mn >= lower) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @param col the column to test
#' @param upper the upper bound
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
test_exclu_upper <- function(col, upper, na) {
  
  mx <- max_na(col, na)
  
  if (mx < upper) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test_exclu_upper
test_inclu_upper <- function(col, upper, na) {
  
  mx <- max_na(col, na)
  
  if (mx <= upper) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' #' @title Test exclu lower and exclu upper
#' #' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' #' @inheritParams test_exclu_lower
#' #' @inheritParams test_exclu_upper
#' test_exclu_lower_exlcu_lower <- function(col, upper, lower, na) {
#'   
#'   if (test_exclu_lower(col, lower) & test_exclu_upper(col, upper)) {
#'     return(TRUE)
#'   } else {
#'     return(FALSE)
#'   }
#'   
#' }



assign_numeric_class <- function(class) {
  
  if (!is.null(class$upper) & !is.null(class$lower)) {
    
    # assertthat::assert_that(!is.null(class$upper_inclu) & !is.null(class$lower_inclu))
    
    # class(class) <- append(class(class), "test_range")
    
    
  } else if (!is.null(class$upper) & is.null(class$lower)) {
    
    # class(class) <- append(class(class), "test_upper_bound")
    
  } else if (is.null(class$upper) & !is.null(class$lower)) {
    
    # class(class) <- append(class(class), "test_lower_bound")
    
  }
  
  if (!is.null(class$upper_inclu)) {
  
    if (class$upper_inclu) {
      
      # class(class) <- append(class(class), "test_upper_inclu")
      
    } else {
      
      # class(class) <- append(class(class), "test_upper_exclu")
      
    }
  }
  
  if (!is.null(class$lower_inclu)) {
  
    if (class$lower_inclu) {
      
      # class(class) <- append(class(class), "test_lower_inclu")
      
    } else {
      
      # class(class) <- append(class(class), "test_lower_exclu")
    }
  
  }
  
  return(class)
  
}

test_numeric_range <- function(class) {
  
  class <- assign_numeric_class(class)
  
  if (!is.null(class$upper) & !is.null(class$lower)) {
    
    
    
  } else {
    
  }
  
  return(class)
  
}

#' @param df the dataframe
#' @export
test_integer_range <- function(df, col_name, upper_inclu, lower_inclu, 
                               upper, lower, na) {
  
  class <- class_test_integer_range(col_name, upper_inclu, lower_inclu, 
                                    upper, lower, na)
  
  return(class)
}

#' @param df the dataframe
#' @export
test_double_range <- function(df, col_name, upper_inclu, lower_inclu, 
                              upper, lower, na) {
  
  class <- class_test_double_range(col_name, upper_inclu, lower_inclu, 
                                   upper, lower, na)
  
  return(class)

}