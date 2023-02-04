#' Consumes the first n elements of an iterator
#'
#' Advances the iterator n-steps ahead without returning anything.
#'
#' If \code{n} is 0, the iterator is consumed entirely. Similarly, if \code{n}
#' is larger than the length of the iterator, the iterator is consumed entirely.
#'
#' @export
#' @param iteror an iteror object
#' @param n The number of elements to consume.
#' @return Nothing, i.e., \code{invisible(NULL)}
#'
#' @examples
#' it <- iteror(1:10)
#' # Skips the first 5 elements
#' consume(it, n=5)
#' # Returns 6
#' nextElemOr(it, NA)
#'
#' it2 <- iteror(letters)
#' # Skips the first 4 elements
#' consume(it2, 4)
#' # Returns 'e'
#' nextElemOr(it2, NA)
#'
consume <- function(iteror, n=0) {
  if (!is_iteror(iteror)) {
    stop("'iterator' must be of class 'iteror'")
  }

  if (n < 0 || !is.numeric(n) || length(n) != 1) {
    stop("n must be a non-negative integer of length 1")
  }
  n <- as.integer(n)
  if (n == 0) {
    repeat nextElemOr(iteror, break)
  } else {
    for (i in seq_len(n)) {
      nextElemOr(iteror, break)
    }
  }

  invisible(NULL)
}

#' Returns the nth item of an iteror
#'
#' Returns the \code{n}th item of an \code{iteror} after advancing the
#' iteror \code{n} steps ahead. If the \code{iteror} is entirely consumed,
#' the \code{default} value is returned instead. That is, if either \code{n >
#' length(iteror)} or \code{n} is 0, then the \code{iteror} is consumed.
#'
#' @export
#' @param iteror an iteror object
#' @param n The location of the desired element to return
#' @param or An argument to force and return if the iteror is consumed.
#' @return The nth element of the iteror or the result of forcing `or`
#'
#' @examples
#' it <- iteror(1:10)
#' # Returns 5
#' nth(it, 5, NA)
#'
#' it2 <- iteror(letters)
#' # Returns 'e'
#' nth(it2, 5, NA)
#'
#' it3 <- iteror(letters)
#' # Returns default value of NA
#' nth(it3, 42, NA)
#'
#' it4 <- iteror(letters)
#' # Returns default value of "foo"
#' nth(it4, 42, or="foo")
#'
nth <- function(iteror, n, or) {
  if (!is_iteror(iteror)) {
    stop("'iteror' must be of class 'iter'")
  }

  if (n < 0 | !is.numeric(n) | length(n) != 1) {
    stop("n must be a positive integer of length 1")
  }

  n <- as.integer(n)

  it <- islice(iteror, start=n)
  nextElemOr(it, or)
}
