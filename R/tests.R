#' @title Test Pass
#' @param setup the setup class
#' @export
test_pass <- function(setup, ...) {
  UseMethod("test_pass", object = setup)
  
}

test_pass.default <- function(setup, ...) {
 
  setup$test_result <- TRUE
  setup$test_message <- "PASSED"
  setup$rows_failed <- 0
  
  return(setup)
  
}

#' @title Test Fail
#' @param setup the setup class
#' @export
test_fail <- function(setup, ...) {
  UseMethod("test_fail", object = setup)
}

test_fail.default <- function(setup, ...) {
  
  warning(paste("test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes na, unique, values, range"))
  
}

test_fail.values <- function(setup, ...) {
 
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED with additional values in col: ", 
                        paste(setup$add_values, collapse = ","))
  setup$rows_failed <- 0
  
  # n <- nrow(df)
  # setup$problem_df <- data.frame(table(df[[setup$col_name]]), stringsAsFactors = F) %>% 
  #   dplyr::filter(Var1 %in% add_values) %>% 
  #   dplyr::mutate(percent = (Freq/n)*100) %>% 
  #   dplyr::rename(value = Var1,
  #                 freq = Freq)
  
  return(setup)
   
}

test_fail.unique <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- "FAILED: Not Unique"
  setup$rows_failed <- 0
  
  return(setup)
  
}

test_fail.na <- function(setup, ...) {
  
  setup$test_result <- FALSE  
  setup$test_message <- "FAILED: Contains na values"
  setup$rows_failed <- 0
  # setup$problem_df <- data.frame(n_na = sum(is.na(df[[setup$col_name]])),
  #                                stringsAsFactors = F)
  
  return(setup)
  
}

test_fail.exclu_lower <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: lower bound is ", setup$mn, 
                               " but expected greater than ", setup$lower)
  setup$rows_failed <- 0
  
  # n_out_bound <- sum(df[[setup$col_name]] <= mn, na.rm = TRUE)
  # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
  #                                pct_out_bound = n_out_bound/nrow(df),
  #                                stringsAsFactors = FALSE)
  
  return(setup)
}

test_fail.inclu_lower <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: lower bound is ", setup$mn, 
                               " but expected greater than or equal to", setup$lower)
  setup$rows_failed <- 0
  
  # n_out_bound <- sum(df[[setup$col_name]] <= mn, na.rm = TRUE)
  # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
  #                                pct_out_bound = n_out_bound/nrow(df),
  #                                stringsAsFactors = FALSE)
  
  return(setup)
  
}

test_fail.exclu_upper <- function(setup, ...) {
 
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: upper bound is ", setup$mx, 
                               " but expected less than ", setup$upper)
  setup$rows_failed <- 0
  
  # n_out_bound <- sum(df[[setup$col_name]] >= mx, na.rm = TRUE)
  # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
  #                                pct_out_bound = n_out_bound/nrow(df),
  #                                stringsAsFactors = FALSE)
  
  return(setup)
   
}

test_fail.inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$test_message <- paste0("FAILED: upper bound is ", setup$mx, 
                               " but expected lesser than or equal to ", setup$upper)
  setup$rows_failed <- 0
  
  # n_out_bound <- sum(df[[setup$col_name]] > mx, na.rm = TRUE)
  # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
  #                                pct_out_bound = n_out_bound/nrow(df),
  #                                stringsAsFactors = FALSE)
  return(setup)
}

test_fail.exclu_lower_exclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$rows_failed <- 0
  
  if (setup$el$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 ", but upper bound ", setup$eu$test_message)
    # setup$problem_df <- eu$problem_df
  } else if (setup$eu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$eu$test_message, 
                                 ", but lower bound ", setup$el$test_message)
    # setup$problem_df <- el$problem_df
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 " and upper bound: ", setup$eu$test_message)
    # n_out_bound <- el$problem_df$n_out_bound + eu$problem_df$n_out_bound
    # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
    #                                pct_out_bound = n_out_bound/nrow(df),
    #                                stringsAsFactors = FALSE)
  }
  
  return(setup)
}

test_fail.inclu_lower_exclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$rows_failed <- 0
  
  if (setup$il$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 ", but upper bound ", setup$eu$test_message)
    # setup$problem_df <- eu$problem_df
  } else if (setup$eu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$eu$test_message, 
                                 ", but lower bound ", setup$il$test_message)
    # setup$problem_df <- il$problem_df
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 " and upper bound: ", setup$eu$test_message)
    # n_out_bound <- il$problem_df$n_out_bound + eu$problem_df$n_out_bound
    # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
    #                                pct_out_bound = n_out_bound/nrow(df),
    #                                stringsAsFactors = FALSE)
  }
  
  return(setup)
  
}

test_fail.inclu_lower_inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$rows_failed <- 0
  
  if (setup$il$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 ", but upper bound ", setup$iu$test_message)
    # setup$problem_df <- setup$iu$problem_df
  } else if (setup$iu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$iu$test_message, 
                                 ", but lower bound ", setup$il$test_message)
    # setup$problem_df <- il$problem_df
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$il$test_message, 
                                 " and upper bound: ", setup$iu$test_message)
    # n_out_bound <- il$problem_df$n_out_bound + iu$problem_df$n_out_bound
    # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
    #                                pct_out_bound = n_out_bound/nrow(df),
    #                                stringsAsFactors = FALSE)
  }
  
  return(setup)
}

test_fail.exclu_lower_inclu_upper <- function(setup, ...) {
  
  setup$test_result <- FALSE
  setup$rows_failed <- 0
  
  if (setup$el$test_result) {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 ", but upper bound ", setup$iu$test_message)
    # setup$problem_df <- iu$problem_df
  } else if (setup$iu$test_result) {
    setup$test_message <- paste0("FAILED: upper bound: ", setup$iu$test_message, 
                                 ", but lower bound ", setup$el$test_message)
    # setup$problem_df <- el$problem_df
  } else {
    setup$test_message <- paste0("FAILED: lower bound: ", setup$el$test_message, 
                                 " and upper bound: ", setup$iu$test_message)
    # n_out_bound <- el$problem_df$n_out_bound + iu$problem_df$n_out_bound
    # setup$problem_df <- data.frame(n_out_bound = n_out_bound,
    #                                pct_out_bound = n_out_bound/nrow(df),
    #                                stringsAsFactors = FALSE)
  }
  return(setup)
}

#' @title Test conditional
#' @description Applies the attribute of a passing or failing test
#' @inheritParams test
#' @param t a boolean value of TRUE or FALSE, if t == TRUE then return the passing class, else return the failing class
test_conditional <- function(setup, t) {
  
  if (t) {
    
    return(test_pass(setup))
    
  } else {
    
    return(test_fail(setup))
    
  }
  
}

#' @title Test
#' @param setup the setup class
#' @export
test <- function(setup, ...) {
  UseMethod("test", object = setup)
  
}

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
  
  col <- df[[setup$col_name]][!is.na(df[[setup$col_name]])]
  
  actual_values <- unique(col)
  setup$add_values <- setdiff(actual_values, setup$values)
  
  t <- length(setup$add_values) == 0
  
  test_conditional(setup, t)

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
  
  df <- df[complete.cases(df[setup$col_name]), ]
  
  if (length(setup$col_name) > 1) {
    
    col <- do.call(paste, c(df[setup$col_name], sep="-"))
    setup$col_name <- paste(col_name, collapse = ", ")
    
  } else {
    col <- df[[setup$col_name]]
  }
  
  t <- length(unique(col)) == length(col)
  
  test_conditional(setup, t)
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
  
  t <- all(!is.na(df[[setup$col_name]]))
    
  test_conditional(setup, t)
  
}

#' @title Test Exclu lower
#' @description Tests that column is greater than the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower <- function(setup, df, ...) {
  
  setup$mn <- min(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," > ", setup$mn)
  
  t <- setup$mn > setup$lower
  test_conditional(setup, t)

}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower <- function(setup, df, ...) {

  setup$mn <- min(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," >= ", setup$mn)
  
  t <- setup$mn >= setup$lower
  test_conditional(setup, t)

}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_upper <- function(setup, df, ...) {
  
  setup$mx <- max(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," < ", setup$mx)
  
  t <- setup$mx < setup$upper
  test_conditional(setup, t)

}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_upper <- function(setup, df, ...) {
  
  setup$mx <- max(df[[setup$col_name]], na.rm = TRUE)
  setup$test_desc <- paste0("Test ", setup$col_name," <= ", setup$mx)
  
  t <- setup$mx <= setup$upper
  test_conditional(setup, t)
  
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_exclu_upper <- function(setup, df, ...) {
  
  setup_el <- setup
  class(setup_el) <- "exclu_lower"
  setup$el <- test(setup_el, df)
  
  setup_eu <- setup
  class(setup_eu) <- "exclu_upper"
  setup$eu <- test(setup_eu, df)
  
  setup$test_desc <- paste0("Test ", setup$lower," < ", 
                            setup$col_name," < ", setup$upper)
  
  t <- setup$el$test_result & setup$eu$test_result
  test_conditional(setup, t)

}

#' @title Test inclu lower and exclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_exclu_upper <- function(setup, df, ...) {

  setup_il <- setup
  class(setup_il) <- "inclu_lower"
  setup$il <- test(setup_il, df)
  
  setup_eu <- setup
  class(setup_eu) <- "exclu_upper"
  setup$eu <- test(setup_eu, df)
  
  setup$test_desc <- paste0("Test ", setup$lower," <= ", 
                            setup$col_name," < ", setup$upper)
  
  t <- setup$il$test_result & setup$eu$test_result
  test_conditional(setup, t)

}

#' @title Test inclu lower and inclu upper
#' @description Tests that the column is greater than or equal to the lower bound AND less or equal to than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_inclu_upper <- function(setup, df, ...) {
  
  setup_il <- setup
  class(setup_il) <- "inclu_lower"
  setup$il <- test(setup_il, df)
  
  setup_iu <- setup
  class(setup_iu) <- "inclu_upper"
  setup$iu <- test(setup_iu, df)

  setup$test_desc <- paste0("Test ", setup$il$lower," <= ",
                            setup$col_name," <= ", setup$iu$upper)

  t <- setup$il$test_result & setup$iu$test_result
  test_conditional(setup, t)

}


#' @title Test exclu lower and inclu upper
#' @description Tests that the column is greater than the lower bound AND less or equal to than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_inclu_upper <- function(setup, df, ...) {
  
  setup_el <- setup
  class(setup_el) <- "exclu_lower"
  setup$el <- test(setup_el, df)
  
  setup_iu <- setup
  class(setup_iu) <- "inclu_upper"
  setup$iu <- test(setup_iu, df)
  
  
  setup$test_desc <- paste0("Test ", setup$lower, " <= ", 
                            setup$col_name," <= ", setup$upper)
  
  t <- setup$el$test_result & setup$iu$test_result
  test_conditional(setup, t)

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
#'setup <- setup_test_orphan_rec(primary_df = "df1", related_df = "df2", 
#'                               primary_key = c("id1", "id2"), foreign_key = c("id1", "id2"))

#'test <- test(setup, primary_df, related_df)
#'## test$test_result returns TRUE
#' @export
test.orphan_rec <- function(setup, primary_df, related_df, ...) {
  
  related_df <- related_df[complete.cases(related_df[setup$foreign_key]), ]
  
  test <- primary_df %>% 
    dplyr::inner_join(related_df, by = structure(names = setup$primary_key, 
                                                 .Data = setup$foreign_key))
  
  if (nrow(test) != nrow(related_df)) {
    
    
    setup$test_result <- FALSE
    setup$test_message <- paste0("FAILED: orphaned values in ", setup$related_df)
    
    # setup$problem_df <- primary_df %>% 
    #   dplyr::mutate(primary_df = 1) %>% 
    #   dplyr::right_join(related_df, by = structure(names = setup$primary_key, 
    #                                                .Data = setup$foreign_key)) %>% 
    #   dplyr::filter(is.na(primary_df)) %>% 
    #   dplyr::select(dplyr::one_of(setup$primary_key))
    
  } else {
    
    setup$test_result <- TRUE
    setup$test_message <- "PASSED"
    
  }
  
  return(setup)
}