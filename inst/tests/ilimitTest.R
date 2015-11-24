# Simple test of ilimit
test01 <- function() {
  actual <- as.list(ilimit(icount(), 10))
  expected <- as.list(icount(10))
  checkEquals(actual, expected)
}

# Test that the wrapped iterator can be reused
test02 <- function() {
  it <- icount()

  actual <- unlist(as.list(ilimit(it, 3)))
  expected <- 1:3
  checkEquals(actual, expected)

  actual <- unlist(as.list(ilimit(it, 3)))
  expected <- 4:6
  checkEquals(actual, expected)
}
