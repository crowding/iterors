test01 <- function() {
  n <- 1000
  zz <- file("testbin", "wb")
  expected <- foreach(1:1000) %do% {
    x <- rnorm(n)
    writeBin(x, zz)
    mean(x)
  }
  close(zz)

  it <- ireadBin("testbin", "double", n=n)
  actual <- foreach(x=it) %do% {
    mean(x)
  }

  checkEquals(actual, expected)
}

# Test the ipos argument
test02 <- function() {
  n <- 1000
  zz <- file("testbin", "wb")
  expected <- foreach(1:1000) %do% {
    x <- rnorm(n)
    writeBin(x, zz)
    mean(x)
  }
  close(zz)

  it <- irep(0, n)  # Always seek to the beginning of the file
  it <- ireadBin("testbin", "double", n=n, ipos=it)
  actual <- foreach(x=it) %do% {
    mean(x)
  }

  # Modify expected to contain the first value "n" times
  expected <- rep(expected[1], n)

  checkEquals(actual, expected)
}

test03 <- function() {
  x <- rnorm(100)
  expected <- matrix(x, 10)
  zz <- file("testbin", "wb")
  writeBin(x, zz)
  close(zz)

  it <- ireadBin("testbin", "double", nrow(expected))
  actual <- foreach(col=it, .combine='cbind') %do% col
  colnames(actual) <- NULL
  dimnames(actual) <- NULL
  checkEquals(actual, expected)

  # Do it again, but with as.list instead
  it <- ireadBin("testbin", double(), nrow(expected))
  actual <- do.call('cbind', as.list(it))
  checkEquals(actual, expected)
}

test99 <- function() {
  unlink("testbin")
}
