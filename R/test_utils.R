passed <- "PASSED"
failed <- "FAILED"

test_row <- function(test) {
  
  data.frame(df_name = test$df_name,
             col_name = test$col_name,
             test_name = test$test_name,
             test_desc = test$test_desc,
             test_result = test$test_result,
             test_message = test$test_message)
  
}

apply_tests <- function(df, setup) {
  
  test_name <- setup$test_name
  
  if (test_name == "test_na") {
    
    cls <- class_test_na(setup)
    test <- test_na(df, cls)
    
  } else if (test_name == "test_unique") {
    
    cls <- class_test_unique(setup)
    test <- test_unique(df, cls)
    
  } else if (test_name == "test_values") {
    
    cls <- class_test_values(setup)
    test <- test_values(df, cls)
    
  } else if (test_name == "test_range") {
    
    cls <- class_test_range(setup)
    test <- test_range(df, cls)
    
  }
  
  row <- test_row(test)
  
  return(row)
}
