# Test that timeout can stop an infinite iterator
test01 <- function() {
  it <- ihasNext(timeout(icount(), 2))
  x <- 0
  while (hasNext(it))
    x <- nextElem(it)
  checkTrue(x > 0)
}

# Test that timeout doesn't get in the way too much
test02 <- function() {
  n <- 1000
  actual <- as.list(timeout(icount(n), time=Inf))
  expected <- as.list(1:n)
  checkEquals(actual, expected)
}
