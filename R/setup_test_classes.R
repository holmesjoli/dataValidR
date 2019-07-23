#' @title tests to see if X is an integer
#' @param x a scalar or a vector
#' @return TRUE or FALSE
#' @export
is_int <- function(x) {
  
  if (!is.null(x)) {
    x%%1==0
  } else {
    return(FALSE)
  }
}

#' @title Class for column type numeric
#' @param df_name the dataframe name
#' @param col_name the column name
#' @param int takes the value TRUE if the column is an integer, FALSE if double, takes the values TRUE or FALSE
#' @param upper_inclu indicates if the upper bound should be inclusive or not, takes on the values NULL, TRUE or FALSE
#' @param lower_inclu indicates if the lower bound should be inclusive or not, takes on the values NULL, TRUE or FALSE
#' @param upper the upper value
#' @param lower the lower value
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @export
class_test_numeric_range <- function(df_name, col_name,
                                     upper_inclu = NULL, lower_inclu = NULL, 
                                     upper = NULL, lower = NULL, na) {
  
  if((is.logical(upper_inclu) | is.null(upper_inclu)) && 
     (is.logical(lower_inclu) | is.null(lower_inclu)) && 
     (is.numeric(upper) | is.null(upper)) && 
     (is.numeric(lower) | is.null(lower)) &&
     (length(upper) == 1 | is.null(upper)) && 
     (length(lower) == 1 | is.null(lower)) &&
     (!is.null(upper_inclu) == !is.null(upper)) &&
     (!is.null(lower_inclu) == !is.null(lower)) &&
     !(is.null(upper_inclu) & is.null(lower_inclu)) &&
     !(is.null(upper) & is.null(lower)) &&
     is.logical(na)) {
    
    structure(list(df_name,
                   col_name = col_name,
                   upper_inclu = upper_inclu, 
                   lower_inclu = lower_inclu, 
                   upper = upper,
                   lower = lower,
                   na = na),
              class = "numeric")  
    
  } else {
    
    if(!(is.logical(upper_inclu) | is.null(upper_inclu))) stop("upper_inclu is not boolean or NULL")
    if(!(is.logical(lower_inclu) | is.null(lower_inclu))) stop("lower_inclu is not boolean or NULL")
    if(!(is.numeric(upper) | is.null(upper))) stop("upper is not numeric or NULL")
    if(!(is.numeric(lower) | is.null(lower))) stop("lower is not numeric or NULL")
    if(!(length(upper) == 1 | is.null(upper))) stop("upper is not of length 1 or NULL")
    if(!(length(lower) == 1 | is.null(lower))) stop("lower is not of length 1 or NULL")
    if((is.null(upper_inclu) == is.null(upper))) stop("upper_inclu and upper must both be NULL or both be not null")
    if((is.null(lower_inclu) == is.null(lower))) stop("lower_inclu and lower must both be NULL or both be not null")
    if(!(is.null(upper_inclu) & is.null(lower_inclu))) stop("upper_inclu and lower_inclu cannot both by NULL")
    if(!(is.null(upper) & is.null(lower))) stop("upper and lower cannot both be NULL")
    if(!is.logical(na)) stop("na only takes the values TRUE or FALSE")
    
  }
}

#' @title Class for column type integer
#' @inheritParams class_test_numeric_range
#' @export
class_test_integer_range <- function(df_name, col_name,
                                     upper_inclu, lower_inclu, upper, lower, na) {
  
  if ((is_int(upper) | is.null(upper)) &&
      (is_int(lower) | is.null(lower))) {
    
    num <- class_test_numeric_range(df_name, col_name, 
                                    upper_inclu, lower_inclu, upper, lower, na)
    class(num) <- append(class(num), "integer")
    
    return(num)
    
  } else {
    
    if(!(is_int(upper) | is.null(upper))) stop("upper is not integer or NULL")
    if(!(is_int(lower) | is.null(lower))) stop("lower is not integer or NULL")

  }
}


#' @title Class for column type double
#' @inheritParams class_test_numeric_range
#' @export
class_test_double_range <- function(df_name, col_name, 
                                    upper_inclu, lower_inclu, upper, lower, na) {
  
  num <- class_test_numeric_range(df_name, col_name, 
                                  upper_inclu, lower_inclu, upper, lower, na)
  class(num) <- append(class(num), "double")
  
  return(num)
  
}


#' @title Class to test unique
#' @param df_name the dataframe name
#' @param col_name the column name
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @details if NA is set to TRUE, columns with multiple NAs will not be marked as non-unique, NA rows will be ignored
#' @export
class_test_unique <- function(df_name, col_name, na) {
  
  structure(list(test_desc = "Test Unique",
                 df_name = df_name,
                 col_name = col_name,
                 na = na),
            class = "test_unique") 
  
}

#' @title Class to test NA
#' @param df_name the dataframe name
#' @param col_name the column name
#' @export
class_test_na <- function(df_name, col_name) {
  
  structure(list(test_desc = "Test NA",
                 df_name = df_name,
                 col_name = col_name),
            class = "test_na") 
  
}

#' @title Class for range
#' @export
class_test_range <- function(df_name, col_name, int, upper_inclu, lower_inclu, 
                             upper, lower, na) {
  
  if (is.logical(int)) {
  
    if (int) {
      class_test_integer(df_name, col_name, 
                         upper_inclu, lower_inclu, upper, lower, na)
    } else {
      class_test_double(df_name, col_name, 
                        upper_inclu, lower_inclu, upper, lower, na)
    }
      
  } else {
    
    if(!is.logical(int)) stop("na only takes the values TRUE or FALSE")
    
  }
}

#' @title Class to test values
#' @param df_name the dataframe name
#' @param col_name the column name
#' @param values the expected values for the variable
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @details if NA is TRUE then it gets added to the list of acceptable values
#' @export
class_test_values <- function(df_name, col_name, values, na) {
  
  if (is.logical(na)) {
    
    if (na) {
      values <- c(values, NA)
    }
    
    structure(list(test_desc = "Expected Values",
                   df_name = df_name,
                   col_name = col_name,
                   values = values,
                   na = na),
              class = "test_values")
  } else {
    
    if(!is.logical(na)) stop("na only takes the values TRUE or FALSE")
    
  }
}

#' @title Class to test boolean
#' @param df_name the dataframe name
#' @param col_name the column name
#' @param na if the column should include NA values or not, takes the values TRUE or FALSE
#' @details if NA is TRUE then it gets added to the list of acceptable values
#' @export
class_test_bool <- function(df_name, col_name, na) {
  
  structure(list(test_desc = "Boolean Values",
                 df_name = df_name,
                 col_name = col_name,
                 na = na),
            class = "test_bool")
}

