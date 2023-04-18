#' Collect all values from an iterable object
#' @rdname collect
#' @export
collect <- function(x, ...) UseMethod("collect")

#' @rdname collect
#' @param ... passed along to [iteror] constructor.
#' @export
collect.default <- function(x, mode="list", n=as.integer(2^31-1), collapse=FALSE, ...) {
  collect(iteror(x, ...), n=n, mode=mode, collapse=collapse)
}

#' `collect` collects all (or some given number of) values from
#' an iteror and returns them in a vector of the given type.
#'
#' @param x an iterable object
#' @param n the number of elements to return.
#' @param mode What mode to use for the output vector.
#' @param collapse If `FALSE`, the iteror should return a single
#'   element per call, and the returned vector will have as many
#'   elments as there were iterations. If `TRUE` the iteror may emit
#'   a varying-length vector per iteration, and the returned value
#'   will be the concatenation of those vectors.
#'
#' @return The returned value will be `n` elements long if the
#'   iterator did not stop. If `collapse=TRUE` the returned value may
#'   be longer than `n` depending on how long the last chunk was.
#' @exportS3Method collect iteror
#' @rdname collect
collect.iteror <- function(x, mode="list", n=as.integer(2^31-1), collapse=FALSE, ...) {
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("n must be a positive integer of length 1")
  }
  size <- min(64, n)
  a <- vector(mode, length=size)
  i <- 0
  if (collapse) {
    while (i < n) {
      chunk <- x(or = break)
      l <- length(chunk)
      to <- i + l
      if (to > size) {
        size <- min(2 * size, to)
        length(a) <- size
      }
      a[i + seq_len(l)] <- chunk
      i <- i + l
    }
    length(a) <- i;
    a
  } else {
    if (mode == "list") {
      wrap <- list
    } else {
      wrap <- identity
    }
    while (i < n) {
      length(a) <- size
      i <- i + 1
      for (i in i:size) {
        a[i] <- wrap(x(or = {length(a) <- i-1; return(a)}))
      }
      size <- min(2 * size, n)
    }
    a
  }
}


#' @exportS3Method as.list iteror
#' @rdname collect
as.list.iteror <- function(x, n=as.integer(2^31-1), ..., collapse=FALSE) {
  collect(x, mode="list", n=n, collapse=collapse, ...)
}

#' @exportS3Method as.double iteror
#' @rdname collect
as.double.iteror <- function(x, n=as.integer(2^31-1), ..., collapse=FALSE) {
  collect(x, mode="numeric", n=n, collapse=collapse, ...)
}

#' @exportS3Method as.numeric iteror
#' @rdname collect
as.numeric.iteror <- function(x, n=as.integer(2^31-1), ..., collapse=FALSE) {
  collect(x, mode="numeric", n=n, collapse=collapse, ...)
}

#' @exportS3Method as.logical iteror
#' @rdname collect
as.logical.iteror <- function(x, n=as.integer(2^31-1), ..., collapse=FALSE) {
  collect(x, mode="logical", n=n, collapse=collapse, ...)
}

#' @exportS3Method as.character iteror
#' @rdname collect
as.character.iteror <- function(x, n=as.integer(2^31-1), ..., collapse=FALSE) {
  collect(x, mode="character", n=n, collapse=collapse, ...)
}

#' @exportS3Method as.vector iteror
#' @rdname collect
as.vector.iteror <- function(x, mode) {
  collect(x, mode)
}

#' Return the first n elements of an iterable object as a list
#'
#' Returns the first \code{n} elements of an iterable \code{object} as a list.
#' If \code{n} is larger than the number of elements in \code{object}, the
#' entire iterator is consumed.
#'
#' @export
#' @param obj an iterable object
#' @param n the number of elements to return.
#' @param mode What mode to use for the output vector.
#' @return a list of the first \code{n} items of the iterable
#'   \code{obj}
#' @details A function `take` first appeared in package `itertools2`.
#'   It is basically an alias for [collect] but defaults to taking
#'   one element.
#' @param ... passed along to [iteror] constructor.
#' @examples
#' take(1:10, 3) # 1 2 3
#' take(icount(), 10) # 1:10
#' take(icount(5), 10) # 1 2 3 4 5
take <- function(obj, n=1, mode="list", ...) {
  obj <- iteror(obj, ...)
  collect(obj, n=n, mode=mode)
}
