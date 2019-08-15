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
#' setup <- setup_test_values("df_name", "x", values = 1:4, na = FALSE)
#' 
#' test <- test(setup, df)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_values("df_name", "y", values = 1:4, na = FALSE)
#' test <- test(setup, df)
#' ## test$test_result returns FALSE
#' }
#' @export
test.values <- function(setup, df, ...) {
  
  actual_values <- unique(df[[setup$col_name]])
  add_values <- setdiff(actual_values, setup$values)
  
  if (length(add_values) > 0) {
    
    setup$test_result <- FALSE
    setup$test_message <- paste0("FAILED with additional values in col: ", 
                                 paste(add_values, collapse = ","))
    
    n <- nrow(df)
    setup$problem_df <- data.frame(table(df[[setup$col_name]]), stringsAsFactors = F) %>% 
                          dplyr::filter(Var1 %in% add_values) %>% 
                          dplyr::mutate(percent = (Freq/n)*100) %>% 
                          dplyr::rename(value = Var1,
                                        freq = Freq)
  } else {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  }
  
  return(setup)
}

#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @inheritParams test
#' @param df the dataframe
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' setup <- setup_test_unique("df_name", "x", FALSE) 
#' 
#' test <- test(setup, df)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_unique("df_name", "y", FALSE) 
#' test <- test(setup, df)
#' ## test$test_result returns FALSE
#' }
#' @export
test.unique <- function(setup, df, ...) {
  
  if (length(setup$col_name) > 1) {
    col <- do.call(paste, c(df[setup$col_name], sep="-"))
    setup$col_name <- paste(col_name, collapse = ", ")
    
  } else {
    col <- df[[setup$col_name]]
  }
  
  if (setup$na) {
    col <- col[!is.na(col)]
  }
  
  if (length(unique(col)) == length(col)) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "FAILED: Not Unique"
    
    setup$problem_df <- as.data.frame(table(col)) %>% 
                          dplyr::filter(Freq > 1) %>% 
                          dplyr::rename(value = col,
                                        freq = Freq)
  }
  
  return(setup)
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
  
  if (all(!is.na(df[[setup$col_name]]))) {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  } else {
    
    setup$test_result <- FALSE  
    setup$test_message <- "FAILED: Contains na values"
    setup$problem_df <- data.frame(n_na = sum(is.na(df[[setup$col_name]])),
                                   stringsAsFactors = F)
  }
  
  return(setup)
}

#' @title Test Exclu lower
#' @description Tests that column is greater than the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower <- function(setup, df, ...) {
  
  mn <- min_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," > ", mn)
  
  if (!is.na(mn)) {
    
    if (mn > setup$lower) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: lower bound is ", mn, " but expected greater than ", setup$lower)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  
  return(setup)
}

#' @title Test inclu lower
#' @description Tests that column is greater than or equal to the lower bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower <- function(setup, df, ...) {
  
  mn <- min_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," >= ", mn)
  
  if (!is.na(mn)) {
    
    if (mn >= setup$lower) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: lower bound is ", mn, " but expected greater than or equal to", setup$lower)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test Exclu upper
#' @description Tests that column is less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_upper <- function(setup, df, ...) {
  
  mx <- max_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," < ", mx)
  
  if (!is.na(mx)) {
    
    if (mx < setup$upper) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: upper bound is ", mx, " but expected less than ", setup$upper)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test Exclu upper
#' @description Tests that column is less than or equal to the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_upper <- function(setup, df, ...) {
  
  mx <- max_na(df[[setup$col_name]], setup$na)
  
  setup$test_desc <- paste0("Test ", setup$col_name," <= ", mx)
  
  if (!is.na(mx)) {
    
    if (mx <= setup$upper) {
      setup$test_result <- TRUE
      setup$test_message <- passed
    } else {
      setup$test_result <- FALSE
      setup$test_message <- paste0("FAILED: upper bound is ", mx, " but expected lesser than or equal to ", setup$upper)
    }
  } else {
    setup$test_result <- FALSE
    setup$test_message <- "NAs present but NA was set to FALSE"
  }
  return(setup)
}

#' @title Test exclu lower and exclu upper
#' @description Tests that the column is greater than the lower bound AND less than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_exclu_upper <- function(setup, df, ...) {
  
  el <- test.exclu_lower(setup,df)
  eu <- test.exclu_upper(setup,df)
  
  setup$test_desc <- paste0("Test ", el$lower," < ", setup$col_name," < ", eu$upper)
  
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
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_exclu_upper <- function(setup, df, ...) {
  
  il <- test.inclu_lower(setup,df)
  eu <- test.exclu_upper(setup,df)
  
  setup$test_desc <- paste0("Test ", il$lower," <= ", setup$col_name," < ", eu$upper)
  
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
#' @inheritParams test
#' @param df the dataframe
#' @export
test.inclu_lower_inclu_upper <- function(setup, df, ...) {
  
  il <- test.inclu_lower(setup,df)
  iu <- test.inclu_upper(setup,df)
  
  setup$test_desc <- paste0("Test ", il$lower," <= ", setup$col_name," <= ", iu$upper)
  
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
#' @description Tests that the column is greater than the lower bound AND less or equal to than the upper bound
#' @inheritParams test
#' @param df the dataframe
#' @export
test.exclu_lower_inclu_upper <- function(setup, df, ...) {
  
  el <- test.exclu_lower(setup,df)
  iu <- test.inclu_upper(setup,df)
  
  setup$test_desc <- paste0("Test ", el$lower," <= ", setup$col_name," <= ", iu$upper)
  
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
    setup$problem_df <- primary_df %>% 
      dplyr::mutate(primary_df = 1) %>% 
      dplyr::right_join(related_df, by = structure(names = setup$primary_key, 
                                                   .Data = setup$foreign_key)) %>% 
      dplyr::filter(is.na(primary_df)) %>% 
      dplyr::select(dplyr::one_of(setup$primary_key))
    
  } else {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  }
  
  return(setup)
}