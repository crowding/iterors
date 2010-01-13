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

# Test irepeat with a vector argument
test04 <- function() {
  x <- 1:10
  n <- 10
  actual <- as.list(irepeat(x), n)
  expected <- rep(list(x), n)
  checkEquals(actual, expected)
}

# Test irepeat with a list argument
test05 <- function() {
  x <- as.list(1:10)
  n <- 10
  actual <- as.list(irepeat(x), n)
  expected <- rep(list(x), n)
  checkEquals(actual, expected)
}
