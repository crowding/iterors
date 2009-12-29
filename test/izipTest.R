test01 <- function() {
  actual <- as.list(izip(a=1, b=2, c=rep(3, 100)))
  expected <- list(list(a=1, b=2, c=3))
  checkEquals(actual, expected)
}

test02 <- function() {
  actual <- as.list(izip())
  expected <- list()
  checkEquals(actual, expected)
}
