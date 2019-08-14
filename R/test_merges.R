#' @title Setup for tests for orphaned records
#' @param primary_df the name of the primary dataframe
#' @param related_df the name of the related dataframe
#' @param primary_key the column or vector of columns to merge on
#' @param foreign_key the column or vector of columns to merge on
setup_test_orphan_rec <- function(primary_df, related_df, primary_key, foreign_key) {

  setup <- structure(list(test_category = "Completeness",
                          test_name = "test_merge",
                          test_desc = "Test Merge",
                          primary_df = primary_df,
                          related_df = related_df,
                          primary_key = primary_key,
                          foreign_key = foreign_key), class = "test_orphan_rec")
  
  test_param_string(setup, "primary_df")
  test_param_string(setup, "related_df")
  test_param_string(setup, "primary_key")
  test_param_string(setup, "foreign_key")

  return(setup)

}

#' @title Tests for orphaned records 
#' @description Tests that there is a one to many relationship between the left and right dataset
#' @param primary_df the primary dataframe
#' @param related_df the related dataframe
#' @param setup from the merge setup class
test_orphan_rec <- function(primary_df, related_df, setup) {
  
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
