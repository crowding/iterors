#' Count the number of times an iterable object is TRUE
#'
#' Returns the number of elements from an iterable object evaluate to
#' \code{TRUE}.
#'
#' @export
#' @param object an iterable object
#' @return the number of \code{TRUE} elements
#'
#' @seealso ireduce
#'
#' @examples
#' it <- iteror(c(TRUE, FALSE, TRUE))
#' quantify(it) # 2
#'
#' set.seed(42)
#' x <- sample(c(TRUE, FALSE), size=10, replace=TRUE)
#' quantify(x) # Equivalent to sum(x)
#'
quantify <- function(object) {
  it <- iteror(object)
  i <- 0
  repeat{
    next_elem <- nextOr(it, break)
    if (next_elem) {
      i <- i + 1
    }
  }
  i
}
