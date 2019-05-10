
test_pass <- list(
  
  ti = "PASS",
  tm = ""
  
)

class(test_pass) <- append(class(test_pass), "test_pass")

test_fail <- list(
  
  ti = "ERROR"
)

class(test_fail) <- append(class(test_fail), "test_fail")

test_warn <- list(
  
  ti = "WARNING"
)

class(test_warn) <- append(class(test_warn), "test_warn")


df <- list(
  
  names = c("Description", "Indicator", "Message")
)

named_df <- function(x) {
  
  df <- t(data.frame(x))
  colnames(df) <- c("Description", "Indicator", "Message")
  row.names(df) <- NULL
  
  return(df)
}
