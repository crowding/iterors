nullish <- function(x) length(x) == 0

#' Iterators for sequence generation
#'
#' Constructs iterators that generate regular sequences that follow the
#' \code{\link[base]{seq}} family.
#'
#' The \code{iseq} function generates a sequence of values beginning with
#' \code{from} and ending with \code{to}. The sequence of values between are
#' determined by the \code{by}, \code{length_out}, and \code{along_with}
#' arguments. The \code{by} argument determines the step size of the sequence,
#' whereas \code{length_out} and \code{along_with} determine the length of the
#' sequence. If \code{by} is not given, then it is determined by either
#' \code{length_out} or \code{along_with}. By default, neither are given, in
#' which case \code{by} is set to 1 or -1, depending on whether \code{to >
#' from}.
#'
#' @export
#' @param from the starting value of the sequence
#' @param to the end value of the sequence
#' @param by increment of the sequence.
#' @param length_out desired length of the sequence. A non-negative number,
#' which for \code{seq} will be rounded up if fractional.
#' @param along_with the length of the sequence will match the length of this
#' @param recycle Whether to restart the sequence after it reaches `to`.
#' argument
#' @return sequence's iterator
#'
#' @examples
#' it <- iseq(from=2, to=5)
#' unlist(as.list(it)) == 2:5
#'
#' it2 <- iseq_len(4)
#' unlist(as.list(it2)) == 1:4
#'
#' it3 <- iseq_along(iris)
#' unlist(as.list(it3)) == 1:length(iris)
iseq <- function(from=1,
                 to=NULL,
                 by=NULL,
                 length_out=NULL, along_with=NULL,
                 recycle=FALSE) {
  mode(from) <- "numeric" # keeps names
  to <- as.numeric(to)
  by <- as.numeric(by)

  if (length(from) != 1) {
    stop("'from' must be a numeric value of length 1")
  }
  if (length(by) > 1 || (length(by) == 1 && by == 0)) {
    stop("'by' must be a nonzero numeric value of length 1")
  }
  if (nullish(to)) {
    if (nullish(by) || by > 0) to <- Inf
    else to <- -Inf
  }
  if (length(to) != 1) {
    stop("'to' must be a numeric value of length 1")
  }

  # If 'by' is not given, then it is determined by either 'length_out' or
  # 'along_with'.  # By default, neither are given, in which case 'by' is set to
  # 1 or -1, depending on whether to > from.
  if (nullish(by)) {
    if (!nullish(along_with)) {
      length_out <- length(along_with)
      by <- (to - from) / (length_out - 1)
    } else if (!nullish(length_out)) {
      by <- (to - from) / (length_out - 1)
    } else if (to >= from) {
      by <- 1
    } else if (to < from) {
      by <- -1
    }
  }

  current_val <- from - by
  if (by > 0L) {
    iteror.internal(function(or) {
      current_val <<- current_val + by
      if (current_val > to) {
        if (recycle)
          current_val <<- from
        else or
      } else {
        current_val
      }
    })
  } else {
    iteror.internal(function(or) {
      current_val <<- current_val + by
      if (current_val < to) {
        if (recycle)
          current_val <<- from
        else or
      } else {
        current_val
      }
    })
  }
}

#' @export
#' @rdname iseq
iseq_len <- function(length_out=NULL) {
  length_out <- suppressWarnings(try(as.integer(length_out), silent=TRUE))

  if (inherits(length_out, "try-error") || length(length_out) != 1
      || length_out < 0 || is.na(length_out)) {
    stop("'length_out' must be coercible to non-negative integer")
  }

  i <- 0
  nextOr_ <- function(or) {
    if (i >= length_out) or else (i <<- i + 1)
  }

  iteror.internal(nextOr_)
}

#' @export
#' @rdname iseq
iseq_along <- function(along_with=NULL) {
  length_out <- length(along_with)

  i <- 0
  nextOr_ <- function(or) {
    if (i >= length_out) or else i <<- i + 1
  }

  iteror.internal(nextOr_)
}
