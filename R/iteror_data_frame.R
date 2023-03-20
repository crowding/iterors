#' @exportS3Method iteror data.frame
#' @rdname iteror
iteror.data.frame <- function(obj, by=c('column', 'row'),
                              ..., recycle=FALSE) {
  by <- match.arg(by)
  i <- 0L
  n <- switch(by, column=length(obj), nrow(obj))

  nextOr_ <- function(or, ...) {
    if (i >= n) {
      if (n == 0) or
      else if (recycle) i <<- 0
      else or
    } else {
      i <<- i + 1L
      return(switch(by,
                    column=obj[, i],
                    obj[i, ]))
    }
  }

  iteror.function(nextOr_)
}
