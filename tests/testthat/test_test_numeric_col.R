testthat::test_that("Min NA", { 

  col <- c(1:3)
  
  testthat::expect_equal(min_na(col, TRUE), 1)
  testthat::expect_equal(min_na(col, FALSE), 1)

  col <- c(1,3, NA)  
  
  testthat::expect_equal(min_na(col, TRUE), 1)
  testthat::expect_true(is.na(min_na(col, FALSE)))
  
})

testthat::test_that("Max NA", { 
  
  col <- c(1:3)
  
  testthat::expect_equal(max_na(col, TRUE), 3)
  testthat::expect_equal(max_na(col, FALSE), 3)
  
  col <- c(1,3, NA)  
  
  testthat::expect_equal(max_na(col, TRUE), 3)
  testthat::expect_true(is.na(max_na(col, FALSE)))
  
})