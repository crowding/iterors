#' Iterator that returns elements while a predicate function returns TRUE
#'
#' Constructs an iterator that returns elements from an iterable \code{object}
#' as long as the given \code{predicate} function returns \code{TRUE}.
#'
#' @export
#' @param object an iterable object
#' @param predicate a function that determines whether an element is \code{TRUE}
#' or \code{FALSE}. The function is assumed to take only one argument.
#' @return iterator object
#'
#' @examples
#' # Filters out numbers exceeding 5
#' not_too_large <- function(x) {
#'   x <= 5
#' }
#' it <- ikeepwhile(1:100, not_too_large)
#' unlist(as.list(it)) == 1:5
#'
#' # Same approach but uses an anonymous function
#' it2 <- ikeepwhile(seq(2, 100, by=2), function(x) x <= 10)
#' unlist(as.list(it2)) == c(2, 4, 6, 8, 10)
ikeepwhile <- function(object, predicate) {
  iter_obj <- iteror(object)

  stop_iterating <- FALSE
  nextOr_ <- function(or) {
    if (stop_iterating) return(or)
    next_elem <- nextOr(iter_obj, return(or))
    if (predicate(next_elem)) {
      return(next_elem)
    } else {
      stop_iterating <<- TRUE
      return(or)
    }
  }

  iteror.function(nextOr_)
}
