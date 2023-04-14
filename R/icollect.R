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

  size <- min(64, n)
  a <- vector(mode, length=size)
  i <- 0
  while (i < n) {
    length(a) <- size
    i <- i + 1
    for (i in i:size) {
      a[i] <- wrap(nextOr(x, {length(a) <- i-1; return(a)}))
    }
    size <- min(2 * size, n)
  }
  a
}
