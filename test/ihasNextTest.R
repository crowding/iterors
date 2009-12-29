test01 <- function() {
  x <- rnorm(100)
  it <- ihasNext(x)
  actual <- unlist(as.list(it))

  checkEquals(actual, x)
}

test02 <- function() {
  x <- 1:10
  it <- ihasNext(x)
  i <- 0
  actual <- integer(0)  # inefficient, but good test
  while (hasNext(it)) {
    i <- i + 1
    actual[i] <- nextElem(it)
  }

  checkTrue(i == length(x))
  checkEquals(actual, x)
}
