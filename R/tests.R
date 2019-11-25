#' @title Check for column
#' @description Checks that column name is in the dataframe
#' @export
check_for_column <- function(setup, df) {
  
  assertthat::assert_that(setup$col_name %in% colnames(df),
                          msg = paste(setup$col_name, " not in data."))
}

#' @title Test Pass
#' @param setup the setup class
#' @param ... other arguments passed to the function
#' @export
test_pass <- function(setup, ...) {
  UseMethod("test_pass", object = setup)
  
}

#' @export
#' @inheritParams test_pass
test_pass.default <- function(setup, ...) {
 
  setup$test_result <- TRUE
  setup$test_message <- "PASSED"
  
  return(setup)
  
}

#' @title Test Fail
#' @param setup the setup class
#' @param ... other arguments passed to the function
#' @export
test_fail <- function(setup, ...) {
  UseMethod("test_fail", object = setup)
}

#' @inheritParams test_fail
#' @export
test_fail.default <- function(setup, ...) {
  
  warning(paste("test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes na, unique, values, range"))
  
}

#' @inheritParams test_fail
#' @export
test_fail.values <- function(setup, ...) {
 
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED with additional values in col: ", 
                        paste(setup$add_values, collapse = ","))
  
  return(setup)
   
}

#' @inheritParams test_fail
#' @export
test_fail.unique <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- "FAILED: Not Unique"
  
  return(setup)
  
}

#' @inheritParams test_fail
#' @export
test_fail.na <- function(setup, ...) {
  
  setup$test_result <- FALSE  
  setup$test_message <- "FAILED: Contains na values"
  
  return(setup)
  
}

#' @inheritParams test_fail
#' @export
test_fail.exclu_lower <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: lower bound is ", setup$mn, 
                               " but expected greater than ", setup$lower)
  
  return(setup)
}

#' @inheritParams test_fail
#' @export
test_fail.inclu_lower <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: lower bound is ", setup$mn, 
                               " but expected greater than or equal to", setup$lower)
  
  return(setup)
  
}

#' @inheritParams test_fail
#' @export
test_fail.exclu_upper <- function(setup, ...) {
 
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: upper bound is ", setup$mx, 
                               " but expected less than ", setup$upper)
  
  return(setup)
   
}

#' @inheritParams test_fail
#' @export
test_fail.inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: upper bound is ", setup$mx, 
                               " but expected lesser than or equal to ", setup$upper)

  return(setup)
}

#' @inheritParams test_fail
#' @export
test_fail.exclu_lower_exclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  
  if (setup$el$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 ", but upper bound ", setup$eu$test_message)
  } else if (setup$eu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$eu$test_message, 
                                 ", but lower bound ", setup$el$test_message)
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 " and upper bound: ", setup$eu$test_message)
  }
  
  return(setup)
}

#' @inheritParams test_fail
#' @export
test_fail.inclu_lower_exclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  
  if (setup$il$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 ", but upper bound ", setup$eu$test_message)
  } else if (setup$eu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$eu$test_message, 
                                 ", but lower bound ", setup$il$test_message)
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 " and upper bound: ", setup$eu$test_message)
  }
  
  return(setup)
  
}

#' @export
test_fail.inclu_lower_inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  
  if (setup$il$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 ", but upper bound ", setup$iu$test_message)
  } else if (setup$iu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$iu$test_message, 
                                 ", but lower bound ", setup$il$test_message)
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 " and upper bound: ", setup$iu$test_message)
  }
  
  return(setup)
}

#' @export
test_fail.exclu_lower_inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  
  if (setup$el$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 ", but upper bound ", setup$iu$test_message)
  } else if (setup$iu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$iu$test_message, 
                                 ", but lower bound ", setup$el$test_message)
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 " and upper bound: ", setup$iu$test_message)
  }
  return(setup)
}

#' @inheritParams test_fail
#' @export
test_fail.orphan_rec <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: orphaned values in ", setup$related_df)
  
  return(setup)
}

#' @title Test conditional
#' @description Applies the attribute of a passing or failing test
#' @param setup the setup class
#' @param ... other arguments passed to the function
test_conditional <- function(setup) {
  
  if (nrow(setup$wrong_rows) == 0) {
    
    return(test_pass(setup))
    
  } else {
    
    return(test_fail(setup))
    
  }
  
}

#' @title Test
#' @param setup the setup class
#' @param ... other arguments passed to the function
#' @export
test <- function(setup, ...) {
  UseMethod("test", object = setup)
}

#' @inheritParams test
#' @export
test.default <- function(setup, ...) {
  
  warning(paste("test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes na, unique, values, range, and orphan_rec"))
  
}

#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @inheritParams test
#' @param df the dataframe
#' @return class with test_result, test_message, test_description
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' setup <- setup_test_values("df_name", "x", values = 1:4)
#' 
#' test <- test(setup, df)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_values("df_name", "y", values = 1:4)
#' test <- test(setup, df)
#' ## test$test_result returns FALSE
#' }
#' @export
test.values <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
    
  setup$wrong_rows <- df[df[[setup$col_name]] %in% setup$values == FALSE, ]
  setup$add_values <- unique(setup$wrong_rows[[setup$col_name]])
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}

#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @inheritParams test
#' @param df the dataframe
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' setup <- setup_test_unique("df_name", "x") 
#' 
#' test <- test(setup, df)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_unique("df_name", "y") 
#' test <- test(setup, df)
#' ## test$test_result returns FALSE
#' }
#' @export
test.unique <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  if (length(setup$col_name) == 1) {
    setup$wrong_rows <- df[duplicated(df[[setup$col_name]]), ]
  } else {
    setup$wrong_rows <- df[duplicated(df[setup$col_name]), ]
    setup$col_name <- paste(setup$col_name, collapse = ", ")
  }
  
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)
}

#' @title Test NA values
#' @description Tests if the vector/column contains any NA values
#' @inheritParams test
#' @param df the dataframe
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(NA, 6:8))
#' setup <- setup_test_na("df_name", "x") 
#' test <- test(setup, df)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_na("df_name", "y") 
#' test <- test(setup, df)
#' ## test$test_result returns FALSE
#' }
#' @export
test.na <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  setup$wrong_rows <- df[is.na(df[setup$col_name]), ]
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
    
  test_conditional(setup)
  
}

#' @title Test Exclu lower
#' @description Tests that column is greater than the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup$mn <- min(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," > ", setup$mn)
  
  setup$wrong_rows <- df[df[[setup$col_name]] <= setup$lower, ]
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower <- function(setup, df, ...) {

  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup$mn <- min(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," >= ", setup$mn)
  
  setup$wrong_rows <- df[df[[setup$col_name]] < setup$lower, ]
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_upper <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup$mx <- max(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," < ", setup$mx)
  
  setup$wrong_rows <- df[df[[setup$col_name]] >= setup$upper, ]
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100

  test_conditional(setup)
  
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_upper <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup$mx <- max(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," <= ", setup$mx)
  
  setup$wrong_rows <- df[df[[setup$col_name]] > setup$upper, ]
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)
  
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_exclu_upper <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup_el <- setup
  class(setup_el) <- "exclu_lower"
  setup$el <- test(setup_el, df)
  
  setup_eu <- setup
  class(setup_eu) <- "exclu_upper"
  setup$eu <- test(setup_eu, df)
  
  setup$test_desc <- paste0("Test ", setup$lower," < ", 
                            setup$col_name," < ", setup$upper)
  
  setup$wrong_rows <- rbind(setup$el$wrong_rows, setup$eu$wrong_rows)
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100

  test_conditional(setup)

}

#' @title Test inclu lower and exclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_exclu_upper <- function(setup, df, ...) {

  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup_il <- setup
  class(setup_il) <- "inclu_lower"
  setup$il <- test(setup_il, df)
  
  setup_eu <- setup
  class(setup_eu) <- "exclu_upper"
  setup$eu <- test(setup_eu, df)
  
  setup$test_desc <- paste0("Test ", setup$lower," <= ", 
                            setup$col_name," < ", setup$upper)
  
  setup$wrong_rows <- rbind(setup$il$wrong_rows, setup$eu$wrong_rows)
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}

#' @title Test inclu lower and inclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less or equal to than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_inclu_upper <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup_il <- setup
  class(setup_il) <- "inclu_lower"
  setup$il <- test(setup_il, df)
  
  setup_iu <- setup
  class(setup_iu) <- "inclu_upper"
  setup$iu <- test(setup_iu, df)

  setup$test_desc <- paste0("Test ", setup$il$lower," <= ",
                            setup$col_name," <= ", setup$iu$upper)

  setup$wrong_rows <- rbind(setup$il$wrong_rows, setup$iu$wrong_rows)
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}


#' @title Test exclu lower and inclu upper
#' @description Tests that the column is greater than the lower bound AND less or equal to than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_inclu_upper <- function(setup, df, ...) {
  
  check_for_column(setup, df)
  
  df <- df[stats::complete.cases(df[setup$col_name]), ]
  
  setup_el <- setup
  class(setup_el) <- "exclu_lower"
  setup$el <- test(setup_el, df)
  
  setup_iu <- setup
  class(setup_iu) <- "inclu_upper"
  setup$iu <- test(setup_iu, df)
  
  
  setup$test_desc <- paste0("Test ", setup$lower, " <= ", 
                            setup$col_name," <= ", setup$upper)
  
  setup$wrong_rows <- rbind(setup$el$wrong_rows, setup$iu$wrong_rows)
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(df))*100
  
  test_conditional(setup)

}

#' @title Tests for orphaned records 
#' @description Tests that there is a one to many relationship between the left and right dataset
#' @inheritParams test
#' @param primary_df the primary dataframe
#' @param related_df the related dataframe
#' @examples 
#' primary_df <- data.frame(id1 = c(1,2,2,3), id2 = c(1,1,2,1), y = c(1,1,2,5))
#' related_df <- data.frame(id1 = c(1,2,3), id2 = c(1,1,1), x = c(1,2,3))
#'
#' setup <- setup_test_orphan_rec(primary_df = "df1", related_df = "df2", 
#'                                primary_key = c("id1", "id2"), foreign_key = c("id1", "id2"))
#'test <- test(setup, primary_df, related_df)
#'## test$test_result returns TRUE
#' @export
test.orphan_rec <- function(setup, primary_df, related_df, ...) {
  
  related_df <- related_df[stats::complete.cases(related_df[setup$foreign_key]), ]
  
  setup$wrong_rows <- primary_df %>% 
    dplyr::mutate(primary_df = 1) %>% 
    dplyr::right_join(related_df, by = structure(names = setup$primary_key, 
                                                 .Data = setup$foreign_key)) %>% 
    dplyr::filter(is.na(primary_df)) %>% 
    dplyr::select(-primary_df) %>% 
    dplyr::select(dplyr::one_of(setup$primary_key)) %>% 
    dplyr::distinct()
  
  setup$rows_failed <- nrow(setup$wrong_rows)
  setup$pct_failed <- (setup$rows_failed/nrow(primary_df))*100
  
  test_conditional(setup)

}
