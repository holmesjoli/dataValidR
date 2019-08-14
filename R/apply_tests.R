#' @title Get summary
#' @description Summarizes the attributes from the test
#' @inheritParams test
#' @return dataframe
#' @export
test_summary <- function(setup, ...) {
  UseMethod("test_summary", object = setup)
  
}

test_summary.default <- function(setup, ...) {
  
  warning(paste("apply_test does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes column or merge"))
  
}

test_summary.column <- function(setup, ...) {
  
  t <- test(setup, df)
  
  d <- data.frame(test_category = t$test_category,
             df_name = t$df_name,
             col_name = t$col_name,
             test_name = t$test_name,
             test_desc = t$test_desc,
             test_result = t$test_result,
             test_message = t$test_message)
  
  return(d)
  
}

test_summary.merge <- function(setup, ...){
  
  t <- test(setup, primary_df, related_df)
  
  d <- data.frame(test_category = t$test_category,
             primary_df = t$primary_df,
             related_df = t$related_df,
             test_name = t$test_name,
             test_desc = t$test_desc,
             test_result = t$test_result,
             test_message = t$test_message)
  
  return(d)
  
}

#' @title Get Problems
#' @inheritParams test
#' @inheritParams test
#' @return dataframe
#' @export
get_problems <- function(setup, ...) {
  UseMethod("get_problems", object = setup)
}

get_problems.default <- function(setup, ...) {
  
  warning(paste("get_problems does not know how to handle object of class ",
                class(setup),
                "and can only be used on classes column or merge"))

}

get_problems.column <- function(setup, ...) {
  
  t <- test(setup, df)
  
  if(!is.null(t$problem_df)) {
    
    t$problem_df <- t$problem_df %>% 
        dplyr::mutate(df_name = t$df_name,
                      col_name = t$col_name,
                      date = lubridate::today())
    
    return(t$problem_df)
  }
  
}

get_problems.merge <- function(setup, ...) {
  
  t <- test(setup, primary_df, related_df)
  
  if(!is.null(t$problem_df)) {
    
    t$problem_df <- t$problem_df %>% 
      dplyr::mutate(df_name = t$df_name,
                    col_name = t$col_name,
                    date = lubridate::today())
    
    return(t$problem_df)
  }
  
}
