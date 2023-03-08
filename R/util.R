#' Utilities for writing iterators
#'
#' \code{is.iteror} indicates if an object is an iteror.
#'
#' @aliases is.iteror
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
is.iteror <- function(x) inherits(x, 'iteror')
