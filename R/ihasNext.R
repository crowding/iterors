# This is based on code that was contributed by Hadley Wickham


#' Create an iterator that supports the hasNext method
#'
#' \code{ihasNext} is a generic function that indicates if the iterator has
#' another element.
#'
#'
#' @param iterable an iterable object, which could be an iterator.
#' @return An \code{ihasNext} iterator that wraps the specified iterator and
#' supports the \code{hasNext} method.
#' @keywords utilities
#' @examples
#'
#'   it <- ihasNext(c('a', 'b', 'c'))
#'   while (hasNext(it))
#'     print(nextElem(it))
#'
#' @export ihasNext
ihasNext <- function(iterable) {
  it <- iter(iterable)

  if (inherits(it, 'ihasNext')) {
    it
  } else {
    cache <- NULL
    hasnext <- NA

    nextEl <- function() {
      if (! hasNx()) {
        stop('StopIteration', call.=FALSE)
      }

      hasnext <<- NA
      cache
    }

    hasNx <- function() {
      if (is.na(hasnext)) {
        tryCatch({
          cache <<- nextElem(it)
          hasnext <<- TRUE
        },
        error=function(e) {
          if (identical(conditionMessage(e), 'StopIteration')) {
            hasnext <<- FALSE
          } else {
            stop(e)
          }
        })
      }

      hasnext
    }

    obj <- list(nextElem=nextEl, hasNext=hasNx)
    class(obj) <- c('ihasNext', 'abstractiter', 'iter')
    obj
  }
}

#' @exportS3Method hasNext ihasNext
hasNext.ihasNext <- function(obj, ...) {
  obj$hasNext()
}
