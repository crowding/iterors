#' Iteror that returns selected elements from an iterable.
#'
#' Constructs an iteror that returns elements from an iterable following the
#' given sequence with starting value \code{start} and ending value \code{end}.
#' The sequence's step size is given by \code{step}.
#'
#' The iterable given in \code{object} is traversed beginning with element
#' having index specified in \code{start}. If \code{start} is greater than 1,
#' then elements from the \code{object} are skipped until \code{start} is
#' reached. By default, elements are returned consecutively. However, if the
#' \code{step} size is greater than 1, elements in \code{object} are skipped.
#'
#' If \code{stop} is \code{NULL} (default), the iteration continues until the
#' iteror is exhausted unless \code{end} is specified. In this case,
#' \code{end} specifies the sequence position to stop iteration.
#'
#' @export
#' @param object iterable object through which this function iterates
#' @param start the index of the first element to return from \code{object}
#' @param end the index of the last element to return from \code{object}
#' @param step the step size of the sequence
#' @return iteror that returns \code{object} in sequence
#'
#' @examples
#' it <- islice(1:5, start=2)
#' nextOr(it, NULL) # 2
#' nextOr(it, NULL) # 3
#' nextOr(it, NULL) # 4
#' nextOr(it, NULL) # 5
#'
#' it2 <- islice(1:10, start=2, end=5)
#' unlist(as.list(it2)) == 2:5
#'
#' it3 <- islice(1:10, start=2, end=9, step=2)
#' unlist(as.list(it3)) == c(2, 4, 6, 8)
islice <- function(object, start=1, end=NULL, step=1) {
  start <- as.integer(start)
  step <- as.integer(step)
  if (length(start) != 1 || start < 1) {
    stop("'start' must be positive integer of length 1")
  }
  if (length(step) != 1 || step < 1) {
    stop("'step' must be a positive integer of length 1")
  }
  if (!is.null(end)) {
    end <- as.integer(end)
    if (length(end) != 1) {
      stop("'end' must be a numeric value of length 1")
    }
  }

  iter_ienum <- ienumerate(object)
  iter_icount <- icount(start=start, step=step)

  nextElem <- function(or) {
    i <- nextOr(iter_icount, return(or))
    if (!is.null(end) && i > end) return(or)

    repeat {
      next_ienum <- nextOr(iter_ienum, return(or))
      if (i == next_ienum$index) {
        return(next_ienum$value)
      }
    }
  }

  iteror(nextElem)
}
