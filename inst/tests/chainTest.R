test01 <- function() {
  actual <- as.list(chain(a=1, b=2, c=rep(3.14159, 100)))
  expected <- c(list(1, 2), as.list(rep(3.14159, 100)))
  checkEquals(actual, expected)
}

test02 <- function() {
  actual <- as.list(chain())
  expected <- list()
  checkEquals(actual, expected)
}
