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
