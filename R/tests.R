#' @title Test
#' @param setup the setup class
test <- function(setup, ...) {
  UseMethod("test", object = setup)
  
}

test.default <- function(setup, ...) {
  
  warning(paste("apply_test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes na, unique, values, range, and orphan_rec"))
  
}


#' @title Test expected values
#' @description Tests if the vector/column contains values other than expected
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @return setup with test_result, test_message, test_description
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = 5:8)
#' values <- 1:4
#' na <- FALSE
#' setup <- setup_test_values("df_name", "x", values, na)
#' 
#' test <- test_values(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_values("df_name", "y", values, na)
#' test <- test_values(df, setup)
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
    
  } else {
    
    setup$test_result <- TRUE
    setup$test_message <- passed
    
  }
  
  return(setup)
}

#' @title Test uniqueness
#' @description Tests if the vector/column is unique
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(1,1,2:3))
#' setup <- setup_test_unique("df_name", "x", FALSE) 
#' 
#' test <- test(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_unique("df_name", "y", FALSE) 
#' test <- test(df, setup)
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
  }
  
  return(setup)
}

#' @title Assign the correct numeric test
#' @description Assigns the correct numeric test depending on the parameters
test.range <- function(setup, df, ...) {
  
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

#' @title Test NA values
#' @description Tests if the vector/column contains any NA values
#' @param df the dataframe
#' @param setup the setup setup for testing
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:4, y = c(NA, 6:8))
#' setup <- setup_test_na("df_name", "x") 
#' test <- test_na(df, setup)
#' ## test$test_result returns TRUE
#' 
#' setup <- setup_test_na("df_name", "y") 
#' test <- test_na(df, setup)
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
  }
  
  return(setup)
}

#' @title Tests for orphaned records 
#' @description Tests that there is a one to many relationship between the left and right dataset
#' @param primary_df the primary dataframe
#' @param related_df the related dataframe
#' @param setup from the merge setup class
test.orphan_rec <- function(setup, primary_df, related_df, ...) {
  
  related_df <- related_df[complete.cases(related_df[setup$foreign_key]), ]
  
  test <- primary_df %>% 
    dplyr::inner_join(related_df, by = structure(names = setup$primary_key, 
                                                 .Data = setup$foreign_key))
  
  if (nrow(test) != nrow(related_df)) {
    
    setup$problem_df <- primary_df %>% 
      dplyr::mutate(primary_df = 1) %>% 
      dplyr::right_join(related_df, by = structure(names = setup$primary_key, 
                                                   .Data = setup$foreign_key)) %>% 
      dplyr::filter(is.na(primary_df)) %>% 
      dplyr::select(dplyr::one_of(setup$primary_key))
    
    setup$test_result <- FALSE
    setup$test_message <- paste0("FAILED: orphaned values in ", setup$related_df)
    
  } else {
    setup$problem_df <- NA
    setup$test_result <- TRUE
    setup$test_message <- passed
  }
  
  return(setup)
}
