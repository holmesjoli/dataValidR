testthat::test_that("test_orphan_rec", {
  
  setup <- setup_test_orphan_rec(primary_df = "df1", related_df = "df2", 
                                 primary_key = "id", foreign_key = "id")
  
  primary_df <- data.frame(id = c(1,2,3,4), y = c(1,1,2,3))
  related_df <- data.frame(id = c(1,2,3), x = c(1,2,3))

  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)
  
  primary_df <- data.frame(id = c(1,2,3,4), y = c(1,1,2,3))
  related_df <- data.frame(id = c(1,2,3,NA), x = c(1,2,3,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)

  primary_df <- data.frame(id = c(1,2,3), y = c(1,1,2))
  related_df <- data.frame(id = c(1,2,2), x = c(1,2,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)
    
  primary_df <- data.frame(id = c(1,2,3), y = c(1,1,2))
  related_df <- data.frame(id = c(1,2,15, NA), x = c(1,2,3,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_orphan_rec(primary_df = "df1", related_df = "df2", 
                                 primary_key = c("id1", "id2"), foreign_key = c("id1", "id2"))
  
  primary_df <- data.frame(id1 = c(1,2,2,3), id2 = c(1,1,2,1), y = c(1,1,2,5))
  related_df <- data.frame(id1 = c(1,2,3), id2 = c(1,1,1), x = c(1,2,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)
  
  primary_df <- data.frame(id1 = c(1,2,2,3), id2 = c(1,1,2,1), y = c(1,1,2,5))
  related_df <- data.frame(id1 = c(1,2,2), id2 = c(1,1,1), x = c(1,2,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)
  
  primary_df <- data.frame(id1 = c(1,2,2,3), id2 = c(1,1,2,1), y = c(1,1,2,5))
  related_df <- data.frame(id1 = c(1,2,3,NA), id2 = c(1,1,1,1), x = c(1,2,3,1))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_true(test$test_result)
  
  primary_df <- data.frame(id1 = c(1,2,2), id2 = c(1,1,2), y = c(1,1,2))
  related_df <- data.frame(id1 = c(1,2,15), id2 = c(1,1,1), x = c(1,2,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_false(test$test_result)
  
  setup <- setup_test_orphan_rec(primary_df = "df1", related_df = "df2", 
                                 primary_key = c("id", "id2"), foreign_key = c("id1", "id2"))
  
  primary_df <- data.frame(id = c(1,2,2), id2 = c(1,1,2), y = c(1,1,2))
  related_df <- data.frame(id1 = c(1,2,15), id2 = c(1,1,1), x = c(1,2,3))
  
  test <- test.orphan_rec(setup, primary_df, related_df)
  testthat::expect_false(test$test_result)
  
})
