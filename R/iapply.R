#' Apply a function to each element of an iterator.
#'
#' `iapply(obj, f)` returns the iteror that applies `f` to
#' each element of the given iterable `obj`. It is an iterator
#' equivalent of `lapply`.
#'
#' @param obj an iterable.
#' @param f a function
#' @param ... Additional arguments will be passed along to `f`
#' @return An iteror.
#' @seealso To apply a function of multiple arguments to multiple
#'   iterators, see [imap]. To split an array over margins (like
#'   `iterators::iapply` use [`iteror(obj, by=MARGIN`][iteror.array]
#' @keywords utilities
#' @export iapply
iapply <- function(obj, f, ...) {
  obj <- iteror(obj, ...)
  nextOr_ <- function(or) f(nextOr(obj, return(or)), ...)
  iteror.internal(nextOr_)
}
