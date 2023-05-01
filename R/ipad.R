#' Iterator that returns an object followed indefinitely by a fill value
#'
#' Constructs an iterator that returns an iterable \code{object} before padding
#' the iterator with the given \code{fill} value indefinitely.
#'
#' @export
#' @param object an iterable object
#' @param fill the value to pad the indefinite iterator after the initial
#' \code{object} is consumed. Default: \code{NA}
#' @param ... Passed along to [iteror] constructor.
#' @return iterator that returns \code{object} followed indefinitely by the
#' \code{fill} value
#' @examples
#'
#' it <- iteror(1:9)
#' it_ipad <- ipad(it)
#' as.list(islice(it_ipad, end=9)) # Same as as.list(1:9)
#'
#' it2 <- iteror(1:9)
#' it2_ipad <- ipad(it2)
#' as.list(islice(it2_ipad, end=10)) # Same as as.list(c(1:9, NA))
#'
#' it3 <- iteror(1:9)
#' it3_ipad <- ipad(it3, fill=TRUE)
#' as.list(islice(it3_ipad, end=10)) # Same as as.list(c(1:9, TRUE))
#'
ipad <- function(object, fill=NA, ...) {
  it <- iteror(object, ...)
  ichain(it, irepeat(fill))
}
