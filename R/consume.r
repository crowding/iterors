#' Consumes the first n elements of an iterator
#'
#' Advances the iterator n-steps ahead without returning anything.
#'
#' @export
#' @param obj an iteror object
#' @param n The number of elements to consume.
#' @return `obj`, invisibly.
#'
#' @examples
#' it <- iteror(1:10)
#' # Skips the first 5 elements
#' consume(it, n=5)
#' # Returns 6
#' nextOr(it, NA)
#'
#' it2 <- iteror(letters)
#' # Skips the first 4 elements
#' consume(it2, 4)
#' # Returns 'e'
#' nextOr(it2, NA)
iconsume <- function(obj, n=Inf) {
  obj <- iteror(obj)

  if (n <= 0 | !is.numeric(n) | length(n) != 1) {
    stop("n must be a non-negative integer of length 1")
  }

  i <- 0L
  while (i < n) {
    nextOr(obj, break)
    i <- i + 1
  }
  invisible(obj)
}

#' Returns the nth item of an iteror
#'
#' Returns the \code{n}th item of an \code{iteror} after advancing the
#' iteror \code{n} steps ahead. If the \code{iteror} is entirely consumed,
#' the argument \code{or} is returned instead. That is, if either \code{n >
#' length(iteror)} or \code{n} is 0, then the \code{iteror} is consumed.
#'
#' @export
#' @param iteror an iteror object
#' @param n The location of the desired element to return
#' @param or An argument to force and return if the iteror is consumed.
#' @return The nth element of the iteror or the result of forcing `or`.
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
nth <- function(it, n, or) {
  it <- iteror(it)

  if (n <= 0 | !is.numeric(n) | length(n) != 1) {
    stop("n must be a positive integer of length 1")
  }

  n <- as.integer(n)
  i <- 0
  for (i in seq_len(n))
    last <- nextOr(it, return(or))
  last
}
