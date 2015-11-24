# Test the irep "each" argument
test01 <- function() {
  x <- as.list(rnorm(10))
  each <- 2
  actual <- as.list(irep(x, each=each))
  expected <- rep(x, each=each)
  checkEquals(actual, expected)
}

# Test the irep "times" argument
test02 <- function() {
  x <- as.list(rnorm(10))
  times <- 2
  actual <- as.list(irep(x, times=times))
  expected <- rep(x, times=times)
  checkEquals(actual, expected)

  times <- rep(2, length(x))
  actual <- as.list(irep(x, times=times))
  expected <- rep(x, times=times)
  checkEquals(actual, expected)
}

# Same as test02 but with an unnamed "times" argument
test03 <- function() {
  x <- as.list(rnorm(10))
  times <- 2
  actual <- as.list(irep(x, times))
  expected <- rep(x, times)
  checkEquals(actual, expected)

  times <- rep(2, length(x))
  actual <- as.list(irep(x, times))
  expected <- rep(x, times)
  checkEquals(actual, expected)
}

# Test the irep "length.out" argument
test04 <- function() {
  x <- as.list(rnorm(10))
  length.out <- 33
  actual <- as.list(irep(x, length.out=length.out))
  expected <- rep(x, length.out=length.out)
  checkEquals(actual, expected)
}

# Test the irep "each" and "length.out" arguments together
test05 <- function() {
  x <- as.list(rnorm(10))
  length.out <- 33
  actual <- as.list(irep(x, each=2, length.out=length.out))
  expected <- rep(x, each=2, length.out=length.out)
  checkEquals(actual, expected)
}

# Test the irep "each" and "times" arguments together
test06 <- function() {
  x <- as.list(rnorm(10))
  each <- 2
  times <- 3
  actual <- as.list(irep(x, each=each, times=times))
  expected <- rep(x, each=each, times=times)
  checkEquals(actual, expected)
}

# Test the irep "each" and a vector "times" together.
# I test this in a special way to avoid what appears to
# be a bug in "rep".
test07 <- function() {
  x <- as.list(rnorm(10))
  each <- 2
  times <- seq(length=length(x) * each)
  actual <- as.list(irep(x, each=each, times=times))
  expected <- rep(rep(x, each=each), times=times)
  checkEquals(actual, expected)
}

# Test that a zero length "times" raises an error
test08 <- function() {
  checkException(irep(1:3, times=integer()))
}

# Test that a negative "times" value raises an error
test09 <- function() {
  checkException(irep(1:3, times=-1))
}

# Test that if any values in "times" are negative, it raises an error
test10 <- function() {
  checkException(irep(1:3, times=c(1,1,-1)))
}

# Test with just one argument
test11 <- function() {
  x <- as.list(rnorm(10))
  actual <- as.list(irep(x))
  expected <- rep(x)
  checkEquals(actual, expected)
}

# Test with an empty list
test12 <- function() {
  x <- list()
  times <- 100
  actual <- as.list(irep(x, times=times))
  expected <- rep(x, times=times)
  checkEquals(actual, expected)

  each <- 10
  actual <- as.list(irep(x, each=each))
  expected <- rep(x, each=each)
  checkEquals(actual, expected)

  actual <- as.list(irep(x, times=times, each=each))
  expected <- rep(x, times=times, each=each)
  checkEquals(actual, expected)
}

# Same as test12 but with an empty iterator
test13 <- function() {
  x <- list()
  times <- 100
  actual <- as.list(irep(iter(x), times=times))
  expected <- rep(x, times=times)
  checkEquals(actual, expected)

  each <- 10
  actual <- as.list(irep(iter(x), each=each))
  expected <- rep(x, each=each)
  checkEquals(actual, expected)

  actual <- as.list(irep(iter(x), times=times, each=each))
  expected <- rep(x, times=times, each=each)
  checkEquals(actual, expected)
}
