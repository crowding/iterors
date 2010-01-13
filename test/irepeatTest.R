# Simple test
test01 <- function() {
  actual <- unlist(as.list(irepeat(42, 10)))
  expected <- rep(42, 10)
  checkEquals(actual, expected)
}

# Test irepeat with times set to zero
test02 <- function() {
  actual <- as.list(irepeat(42, 0))
  expected <- list()
  checkEquals(actual, expected)
}

# Test irepeat with one argument
test03 <- function() {
  actual <- unlist(as.list(irepeat(42), 10))
  expected <- rep(42, 10)
  checkEquals(actual, expected)
}
