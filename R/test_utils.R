test_pass_ti <- "PASS"
test_fail_ti <- "ERROR"
test_warn_ti <- "WARNING"

test_pass_tm <- ""


named_df <- function(x) {
  
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  names(df) <- c("Description", "Indicator", "Message")
  names(x) <- c("Description", "Indicator", "Message")
  df <- rbind(df, x)
  
  return(df)
}
