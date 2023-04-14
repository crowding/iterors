#' Iteror that chains multiple arguments together into a single iterator
#'
#' `ichain` for iterators is analogous to [c()] on vectors. `ichain`
#' constructs an [iteror] that returns elements from the first argument
#' until it is exhausted. Then generates an iterator from the next
#' argument and returns elements from it. This process continues until
#' all arguments are exhausted.
#'
#' @export
#' @author Peter Meilstrup
#' @param ... multiple arguments to iterate through in sequence
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

#' @rdname ichain
#' @param obj an iterable.
#' @description `icollapse(obj)` takes an iterable that returns
#'   iterables, and chains together all inner values of iterables into
#'   one iterator. Analogous to `unlist(recursive=FALSE)`.
#' @export
icollapse <- function(obj, ...) {
  obj <- iteror(obj, ...)
  current <- NULL

  nextOr_ <- function(or) {
    if (is.null(current)) {
      current <<- iteror(nextOr(obj, return(or)))
    }
    repeat {
      return(nextOr(current, {
        current <<- iteror(nextOr(obj, return(or)))
        next
      }))
    }
  }

  iteror.internal(nextOr_)
}
