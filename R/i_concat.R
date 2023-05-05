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
      current <<- iteror(obj(or = return(or)))
    }
    repeat {
      return(current(or = {
        current <<- iteror(obj(or = return(or)))
        next
      }))
    }
  }

  iteror.internal(nextOr_)
}
