#' Return the first n elements of an iterable object as a list
#'
#' Returns the first \code{n} elements of an iterable \code{object} as a list.
#' If \code{n} is larger than the number of elements in \code{object}, the
#' entire iterator is consumed.
#'
#' @export
#' @param obj an iterable object
#' @param n the number of elements to return.
#' @param mode What mode to use for the output vector.
#' @return a list of the first \code{n} items of the iterable
#'   \code{obj}
#' @details A function `take` appeared in package `itertools2`. It is
#'   essentially synonymous with `as.vector.iteror(obj, n=n)` but has
#'   a different order of arguments.
#' @examples
#' take(1:10, 3) # 1 2 3
#'
#' take(1:5, 10) # 1 2 3 4 5
take <- function(obj, n=1, mode="list") {
  obj <- iteror(obj)
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("n must be a positive integer of length 1")
  }
  as.vector.iteror(obj, n=n, mode=mode)
}
