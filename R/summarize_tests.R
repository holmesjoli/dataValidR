#' @title Get summary
#' @description Summarizes the attributes from the test
#' @param test the test class
#' @param ... other arguments passed to the function
#' @export
test_summary <- function(test, ...) {
  UseMethod("test_summary", object = test)
  
}

#' @inheritParams test_summary
#' @export
test_summary.default <- function(test, ...) {
  
  warning(paste("test_summary does not know how to handle object of class ",
                class(test),
                "and can only be used on classes column or merge"))
  
}

#' @inheritParams test_summary
#' @export
test_summary.column <- function(test, ...) {
  
  data.frame(date = lubridate::today(),
             test_category = test$test_category,
             df_name = test$df_name,
             col_name = test$col_name,
             test_name = test$test_name,
             test_desc = test$test_desc,
             test_result = test$test_result,
             test_message = test$test_message,
             rows_failed = test$rows_failed,
             pct_failed = test$pct_failed)

}

#' @inheritParams test_summary
#' @export
test_summary.merge <- function(test, ...){

 data.frame(date = lubridate::today(),
            test_category = test$test_category,
            primary_df = test$primary_df,
            related_df = test$related_df,
            test_name = test$test_name,
            test_desc = test$test_desc,
            test_result = test$test_result,
            test_message = test$test_message,
            rows_failed = test$rows_failed,
            pct_failed = test$pct_failed)

}

#' @title Get Problems
#' @param test the test class
#' @param ... other arguments passed to the function
#' @return dataframe
#' @export
wrong_rows <- function(test, ...) {
  UseMethod("wrong_rows", object = test)
}

#' @inheritParams wrong_rows
#' @export
wrong_rows.default <- function(test, ...) {
  
  warning(paste("wrong_rows does not know how to handle object of class ",
                class(test),
                "and can only be used on classes column or merge"))

}

#' @inheritParams wrong_rows
#' @export
wrong_rows.column <- function(test, ...) {
  
  if(!is.null(test$wrong_rows)) {
    
    test$wrong_rows <- test$wrong_rows %>% 
        dplyr::mutate(date = lubridate::today(),
                      test_category = test$test_category,
                      df_name = test$df_name,
                      col_name = test$col_name,
                      test_name = test$test_name,
                      test_desc = test$test_desc)
    
    return(test$wrong_rows)
  }
  
}

#' @inheritParams wrong_rows
#' @export
wrong_rows.merge <- function(test, ...) {
  
  if(!is.null(test$wrong_rows)) {
    
    test$wrong_rows <- test$wrong_rows %>% 
      dplyr::mutate(date = lubridate::today(),
                    test_category = test$test_category,
                    test_name = test$test_name,
                    test_desc = test$test_desc,
                    primary_df = test$primary_df,
                    related_df = test$related_df,)
    
    return(test$wrong_rows)
  }
  
}
