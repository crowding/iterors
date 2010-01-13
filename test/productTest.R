test01 <- function() {
  actual <- as.list(product(x=1:3, a=letters[1:2], b=1))
  expected <- list(list(x=1L, a='a', b=1),
                   list(x=1L, a='b', b=1),
                   list(x=2L, a='a', b=1),
                   list(x=2L, a='b', b=1),
                   list(x=3L, a='a', b=1),
                   list(x=3L, a='b', b=1))

  checkEquals(actual, expected)
}

test02 <- function() {
  it <- product(a=LETTERS[1:10], b=1, x=1:3)

  results <- 
    foreach(a=LETTERS[1:10], .combine='c') %:%
      foreach(b=1, .combine='c') %:%
        foreach(x=1:3, actual=it, .combine='c') %do%
          identical(list(a=a, b=b, x=x), actual)

  checkTrue(length(results) == 30)
  checkTrue(all(results))
}

# Same as test02, but using explicit "iter" calls when calling "product"
# to test that substitute/eval are working properly
test03 <- function() {
  it <- product(a=iter(LETTERS[1:10]), b=iter(1), x=iter(1:3))

  results <- 
    foreach(a=LETTERS[1:10], .combine='c') %:%
      foreach(b=1, .combine='c') %:%
        foreach(x=1:3, actual=it, .combine='c') %do%
          identical(list(a=a, b=b, x=x), actual)

  checkTrue(length(results) == 30)
  checkTrue(all(results))
}

# Test product with no arguments
test04 <- function() {
  actual <- as.list(product())
  expected <- list()

  checkEquals(actual, expected)
}

# Test product with one argument
test05 <- function() {
  x <- rnorm(100)
  actual <- unlist(as.list(product(x)), use.names=FALSE)

  checkEquals(actual, x)
}

# Test product with NULL argument
test06 <- function() {
  actual <- as.list(product(NULL))

  checkTrue(is.list(actual))
  checkTrue(length(actual) == 0)
}

# Test against expand.grid
test07 <- function() {
  x <- 1:3
  y <- rnorm(5)
  z <- letters[1:4]
  it <- product(a=x, b=y, c=z)
  actual <- foreach(d=it, .combine='rbind') %do% {
    as.data.frame(d, stringsAsFactors=FALSE)
  }
  expected <- expand.grid(c=z, b=y, a=x, stringsAsFactors=FALSE)[3:1]
  checkEquals(actual, expected)
}
