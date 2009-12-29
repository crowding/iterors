# Simple test
test01 <- function() {
  actual <- unlist(as.list(irep(42, 10)))
  expected <- rep(42, 10)
  checkEquals(actual, expected)
}

# Test irep with times set to zero
test02 <- function() {
  actual <- as.list(irep(42, 0))
  expected <- list()
  checkEquals(actual, expected)
}

# Test irep with one argument
test03 <- function() {
  actual <- unlist(as.list(irep(42), 10))
  expected <- rep(42, 10)
  checkEquals(actual, expected)
}
