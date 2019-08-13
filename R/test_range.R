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
#' @param setup the setup class for testing
#' @export
test_exclu_lower <- function(df, setup) {
  
  mn <- min_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," > ", mn)
  class(setup) <- append(class(setup), "exclu_lower")
  
  if (!is.na(mn)) {
  
    if (mn > setup$lower) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than ", setup$lower)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower <- function(df, setup) {
  
  mn <- min_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," >= ", mn)
  class(setup) <- append(class(setup), "inclu_lower")
  
  if (!is.na(mn)) {
    
    if (mn >= setup$lower) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: lower bound is ", mn, " but expected less than or equal to", setup$lower)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_upper <- function(df, setup) {
  
  mx <- max_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," < ", mx)
  
  class(setup) <- append(class(setup), "exclu_upper")
  
  if (!is.na(mx)) {
    
    if (mx < setup$upper) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than ", setup$upper)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_upper <- function(df, setup) {
  
  mx <- max_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," <= ", mx)
  class(setup) <- append(class(setup), "inclu_upper")
  
  if (!is.na(mx)) {
    
    if (mx <= setup$upper) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: upper bound is ", mx, " but expected greater than or equal to ", setup$upper)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_lower_exclu_upper <- function(df, setup) {

  el <- test_exclu_lower(df, setup)
  eu <- test_exclu_upper(df, setup)
  
  setup$test_desc <- paste0("Test ", el$lower," < ", setup$col_name," < ", eu$upper)
  class(setup) <- append(class(setup), "exclu_lower_exclu_upper")
  
  if (el$test_result & eu$test_result) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
  
  } else {
    
    setup$test_result <- FALSE
    
    if (el$test_result) {
      setup$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      setup$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", el$test_message)
    } else {
      setup$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(setup)
}

#' @title Test inclu lower and exclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower_exclu_upper <- function(df, setup) {
  
  il <- test_inclu_lower(df, setup)
  eu <- test_exclu_upper(df, setup)
  
  setup$test_desc <- paste0("Test ", il$lower," <= ", setup$col_name," < ", eu$upper)
  class(setup) <- append(class(setup), "inclu_lower_exclu_upper")
  
  if (il$test_result & eu$test_result) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    
    setup$test_result <- FALSE
    
    if (il$test_result) {
      setup$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", eu$test_message)
    } else if (eu$test_result) {
      setup$test_message <- paste0("upper bound: ", eu$test_message, ", but lower bound ", il$test_message)
    } else {
      setup$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", eu$test_message)
    }
  }
  return(setup)
}

#' @title Test inclu lower and inclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less or equal to than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_inclu_lower_inclu_upper <- function(df, setup) {
  
  il <- test_inclu_lower(df, setup)
  iu <- test_inclu_upper(df, setup)
  
  setup$test_desc <- paste0("Test ", il$lower," <= ", setup$col_name," <= ", iu$upper)
  class(setup) <- append(class(setup), "inclu_lower_inclu_upper")
  
  if (il$test_result & iu$test_result) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    
    setup$test_result <- FALSE
    
    if (il$test_result) {
      setup$test_message <- paste0("lower bound: ", il$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      setup$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", il$test_message)
    } else {
      setup$test_message <- paste0("lower bound: ", il$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(setup)
}


#' @title Test exclu lower and inclu upper
#' @description Tests that the column is greater thanto the lower bound AND less or equal to than the upper bound
#' @inheritParams test_exclu_lower
#' @export
test_exclu_lower_inclu_upper <- function(df, setup) {
  
  el <- test_exclu_lower(df, setup)
  iu <- test_inclu_upper(df, setup)
  
  setup$test_desc <- paste0("Test ", el$lower," <= ", setup$col_name," <= ", iu$upper)
  class(setup) <- append(class(setup), "exclu_lower_inclu_upper")
  
  if (el$test_result & iu$test_result) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    
    setup$test_result <- FALSE
    
    if (el$test_result) {
      setup$test_message <- paste0("lower bound: ", el$test_message, ", but upper bound ", iu$test_message)
    } else if (iu$test_result) {
      setup$test_message <- paste0("upper bound: ", iu$test_message, ", but lower bound ", el$test_message)
    } else {
      setup$test_message <- paste0("lower bound: ", el$test_message, " and upper bound: ", iu$test_message)
    }
  }
  return(setup)
}


#' @title Setup Test Range
#' @param df_name is a string and represents the name of the dataframe. 
#' @param col_name is a string and represents the name of the column to be tested.
#' @param int is a boolean value, if the values should be integers or not
#' @param upper_inclu takes on the values TRUE, FALSE, or NULL. If there's an upper bound then it should be set to TRUE or FALSE.
#' If the upper bound is inclusive, x <= upper, then upper_inclu = TRUE, else upper_inclu = FALSE.
#' @param lower_inclu takes on the values TRUE, FALSE, or NULL. If there's an lower bound then it should be set to TRUE or FALSE.
#' If the lower bound is inclusive, x >= lower, then lower_inclu = TRUE, else lower_inclu = FALSE.
#' @param upper can take a numeric value or NULL. If there's no upper bound, then upper should be null.
#' @param lower can take a numeric value or NULL. If there's no lower bound, then lower should be null.
#' @param na is a boolean value, if NA values are allowed in the column to be tested. If NA = FALSE and
#' there are NA values, an error will occur.
#' @export
setup_test_range <- function(df_name, col_name, int, lower_inclu, 
                             upper_inclu, lower, upper, na) {
  
  setup <- structure(list(test_category = "Accuracy",
                          test_name = "test_range", 
                          test_desc = "Test Range",
                          df_name = df_name,
                          col_name = col_name,
                          int = int,
                          lower_inclu = lower_inclu,
                          upper_inclu = upper_inclu,
                          lower = lower,
                          upper = upper,
                          na = na), class = "test_range")
  
  expc_params <- c("df_name", "col_name", "int", "upper_inclu", 
                   "lower_inclu", "upper", "lower", "na")
  test_expc_params(setup, expc_params)
  test_param_string(setup, "df_name")
  test_param_string(setup, "col_name")
  test_param_logical(setup, "int")
  test_param_logical_or_null(setup, "upper_inclu")
  test_param_logical_or_null(setup, "lower_inclu")
  test_param_logical(setup, "na")
  test_param_numeric_or_null(setup, "upper")
  test_param_numeric_or_null(setup, "lower")
  test_params_both_null_or_not(setup, "upper", "upper_inclu")
  test_params_both_null_or_not(setup, "lower", "lower_inclu")
  
  if (setup$int) {
    test_param_integer_or_null(setup, "upper")
    test_param_integer_or_null(setup, "lower")
    class(setup) <- append(class(setup), "integer")
  } else {
    class(setup) <- append(class(setup), "double")
  }
  
  return(setup)
}

#' @title Assign the correct numeric test
#' @description Assigns the correct numeric test depending on the parameters
#' @inheritParams test_exclu_lower
test_range <- function(df, setup) {
  
  if (!is.null(setup$upper) & !is.null(setup$lower)) {
    
    if (setup$upper_inclu & setup$lower_inclu) {
      setup <- test_inclu_lower_inclu_upper(df, setup) 
    } else if (setup$upper_inclu & !setup$lower_inclu) {
      setup <- test_exclu_lower_inclu_upper(df, setup) 
    } else if (!setup$upper_inclu & setup$lower_inclu) {
      setup <- test_inclu_lower_exclu_upper(df, setup)
    } else {
      setup <- test_exclu_lower_exclu_upper(df, setup)
    }
    
  } else if (!is.null(setup$upper) & is.null(setup$lower)) {
    
     if (setup$upper_inclu) {
       setup <- test_inclu_upper(df, setup)
     } else {
       setup <- test_exclu_upper(df, setup)
     }
    
  } else if (is.null(setup$upper) & !is.null(setup$lower)) {
    
    if (setup$lower_inclu) {
      setup <- test_inclu_lower(df, setup)
    } else {
      setup <- test_exclu_lower(df, setup)
    }
    
  }
  return(setup)
}
