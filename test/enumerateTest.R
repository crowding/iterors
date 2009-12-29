test01 <- function() {
  actual <- as.list(enumerate(icount(3)))
  index <- 1:3
  expected <- lapply(index, function(i) list(index=i, value=i))
  checkEquals(actual, expected)
}

test02 <- function() {
  actual <- as.list(enumerate(c()))
  expected <- list()
  checkEquals(actual, expected)
}

test03 <- function() {
  x <- rnorm(100)
  actual <- as.list(enumerate(x))
  expected <- as.list(izip(index=icount(), value=x))
  checkEquals(actual, expected)
}
