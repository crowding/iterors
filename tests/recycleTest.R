# Test using an iterator
test01 <- function() {
  for (n in c(0, 1, 2, 3, 7, 11, 13, 100, 101, 102, 103)) {
    actual <- as.list(recycle(icount(3)), n)
    expected <- as.list(rep(1:3, length=n))
    checkEquals(actual, expected)
  }
}

# Test using an iterable
test02 <- function() {
  for (n in c(0, 1, 2, 3, 7, 11, 13, 100, 101, 102, 103)) {
    actual <- as.list(recycle(1:3), n)
    expected <- as.list(rep(1:3, length=n))
    checkEquals(actual, expected)
  }
}

# Test with no arguments
test03 <- function() {
  checkException(recycle())
}

# Test with negative "times"
test04 <- function() {
  checkException(recycle(1:3, -1))
  checkException(recycle(icount(3), -1))
}

# Test various legal values of the "times" argument
test05 <- function() {
  x <- 1:3
  for (n in c(0, 1, 2, 3, 9)) {
    actual <- as.list(recycle(x, times=n))
    expected <- rep(as.list(x), times=n)
    checkEquals(actual, expected)
  }
}

# Same as test05, but with an iterator
test06 <- function() {
  m <- 3
  x <- seq(length=m)
  for (n in c(0, 1, 2, 3, 9)) {
    actual <- as.list(recycle(icount(m), times=n))
    expected <- rep(as.list(x), times=n)
    checkEquals(actual, expected)
  }
}
