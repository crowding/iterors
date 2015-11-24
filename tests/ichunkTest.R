test01 <- function() {
  x <- 1:10
  actual <- as.list(ichunk(x, 3))
  expected <- list(as.list(1:3), as.list(4:6), as.list(7:9), as.list(10))
  checkEquals(actual, expected)
}

test02 <- function() {
  n <- 13
  chunkSize <- 7
  x <- rnorm(n * chunkSize)
  actual <- as.list(ichunk(x, chunkSize))
  checkTrue(length(actual) == n)
  for (chunk in actual) {
    checkTrue(length(chunk) == chunkSize)
  }

  y <- unlist(actual)
  checkEquals(x, y)
}
