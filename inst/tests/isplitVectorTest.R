# Split a vector of random numbers using various values for chunks and chunkSize
test01 <- function() {
  x <- rnorm(1000)

  for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunks=chunks)))
    checkEquals(x, y)
  }

  for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunkSize=chunkSize)))
    checkEquals(x, y)
  }
}

# Split a list of random numbers using various values for chunks and chunkSize
test02 <- function() {
  x <- as.list(rnorm(1000))

  for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunks=chunks)))
    checkEquals(x, y)
  }

  for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunkSize=chunkSize)))
    checkEquals(x, y)
  }
}

# Split a string vector of random numbers using various values for chunks and chunkSize
test03 <- function() {
  x <- as.character(rnorm(1000))

  for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunks=chunks)))
    checkEquals(x, y)
  }

  for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999, 1000, 1001)) {
    y <- do.call('c', as.list(isplitVector(x, chunkSize=chunkSize)))
    checkEquals(x, y)
  }
}
