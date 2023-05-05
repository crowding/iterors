#' Iteror that chains multiple arguments together into a single iterator
#'
#' `ichain` for iterators is analogous to [c()] on vectors. `ichain`
#' constructs an [iteror] that returns elements from the first
#' argument until it is exhausted, then elements from the next
#' argument, and so on until all arguments have been exhausted.
#'
#' @export
#' @author Peter Meilstrup
#' @param ... multiple iterable arguments
#' @return iteror that iterates through each argument in sequence
#'
#' @examples
#' it <- ichain(1:3, 4:5, 6)
#' as.list(it)
#'
#' it2 <- ichain(1:3, levels(iris$Species))
#' as.list(it2)
ichain <- function(...) {
  L <- iteror(list(...))
  icollapse(L)
}
