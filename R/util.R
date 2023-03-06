#' Utilities for writing iterators
#'
#' \code{is.iterator} indicates if an object is an iterator.
#'
#' @aliases is.iterator
#' @param x any object.
#' @keywords utilities
#' @examples
#'
#' it <- iteror(1:3)
#' stopifnot(is.iteror(it))
#' repeat {
#'   print(nextOr(it, break))
#' }
#'
#' @export is.iteror
is.iteror <- function(x) inherits(x, 'iteror') || inherits(x, 'iter')
