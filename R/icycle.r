#' Iterator that cycles indefinitely through an iterable object
#'
#' Constructs an iterator that returns an iterable object in sequence over and
#' over again.
#'
#' Runs indefinitely unless the \code{times} argument is specified.
#'
#' @export
#' @param object object to cycle indefinitely.
#' @param times the number of times \code{object} is returned. If \code{NULL}
#' (default), \code{object} is returned indefinitely.
#' @return iterator that returns \code{object} in sequence
#'
#' @examples
#' it <- icycle(1:3)
#' nextOr(it, NA) # 1
#' nextOr(it, NA) # 2
#' nextOr(it, NA) # 3
#' nextOr(it, NA) # 1
#' nextOr(it, NA) # 2
#' nextOr(it, NA) # 3
#' nextOr(it, NA) # 1
#'
#' it2 <- icycle(1:3, times=2)
#' as.list(it2)
#'
#' # Can return the results from a function.
#' it3 <- icycle(function() rnorm(1))
#' nextOr(it, NA)
#' nextOr(it, NA)
#' nextOr(it, NA)
#' nextOr(it, NA)
icycle <- function(object, times=NULL) {
  if (!is.null(times)) {
    times <- as.numeric(times)
    if (length(times) != 1) {
      stop("'times' must be a numeric value of length 1")
    }
  }

  # The recycle argument enables this function to be pretty easy when `times` is
  # NULL. However, we lose the ability to keep track of when the iterator has
  # been exhausted. Fortunately, there is a `length` added for fixed-length
  # objects. When there's not a length, we assume it to be 1.
  if (!is_iteror(object)) {
    if (is.function(object)) {
      iter_obj <- iteror(function(or) object())
    } else {
      iter_obj <- iteror(object, recycle=TRUE)
    }
  }
  iter_len <- iter_length(iter_obj)

  # Counter on number of times `object` has been exhausted
  num_exhausted <- 0
  i <- 0

  nextOr_ <- function(or) {
    i <<- i + 1
    if (i > iter_len) {
      num_exhausted <<- num_exhausted + 1
      i <<- 1
    }
    if (!is.null(times) && num_exhausted >= times) {
      or
    } else {
      nextOr(iter_obj, or)
    }
  }

  it <- iteror(nextOr_)
}
