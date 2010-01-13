# Test the irep "each" argument
test01 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x, each=2))
  expected <- rep(x, each=2)
  checkEquals(actual, expected)
}

# Test the irep "times" argument
test02 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x, times=2))
  expected <- rep(x, times=2)
  checkEquals(actual, expected)

  actual <- as.list(irep(x, times=rep(2, length(x))))
  expected <- rep(x, each=2)
  checkEquals(actual, expected)
}

# Test the irep "length.out" argument
test03 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x, length.out=33))
  expected <- rep(x, length.out=33)
  checkEquals(actual, expected)
}

# Test the irep "each" and "length.out" arguments together
test04 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x, each=2, length.out=33))
  expected <- rep(x, each=2, length.out=33)
  checkEquals(actual, expected)
}

# Test the irep "each" and "times" arguments together
test05 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x, each=2, times=3))
  expected <- rep(x, each=2, times=3)
  checkEquals(actual, expected)
}

# Test that a zero length "times" raises an error
test06 <- function() {
  checkException(irep(1:3, times=integer()))
}

# Test that a negative "times" value raises an error
test07 <- function() {
  checkException(irep(1:3, times=-1))
}

# Test that if any values in "times" are negative, it raises an error
test08 <- function() {
  checkException(irep(1:3, times=c(1,1,-1)))
}
