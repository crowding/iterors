#' @exportS3Method as.list iteror
as.list.iteror <- function(x, n=as.integer(2^31-1), ...) {
  icollect(x, "list", n=n, ...)
}

#' @exportS3Method as.double iteror
as.double.iteror <- function(x, n=as.integer(2^31-1), ...) {
  icollect(x, mode="numeric", n=n, ...)
}

#' @exportS3Method as.logical iteror
as.logical.iteror <- function(x, n=as.integer(2^31-1), ...) {
  icollect(x, mode="logical", n=n, ...)
}

#' @exportS3Method as.vector iteror
as.vector.iteror <- function(x, mode) {
  icollect(x, mode)
}

icollect <- function(x, mode="any", n=as.integer(2^31-1), ...) {
  if (mode == "list")
    wrap <- list
  else wrap <- identity

  size <- 64
  a <- vector(mode, length=size)
  i <- 0
  while (i < n) {
    item <- nextOr(x, break)
    i <- i + 1
    if (i >= size) {
      size <- min(2 * size, n)
      length(a) <- size
    }
    a[i] <- wrap(item)
  }
  length(a) <- i
  a
}
