# This code was contributed by Hadley Wickham

end_iteration <- function() stop('StopIteration', call.=FALSE)

iteration_has_ended <- function(e) {
  identical(conditionMessage(e), 'StopIteration')
}

new_iterator <- function(nextElem, ...) {
  structure(list(nextElem=nextElem, ...), class=c('abstractiter', 'iter'))
}



#' Utilities for writing iterators
#'
#' \code{is.iterator} indicates if an object is an iterator.
#' \code{end_iteration} throws an exception to signal that there are no more
#' values available in an iterator.  \code{iteration_has_ended} tests an
#' exception to see if it indicates that iteration has ended.
#' \code{new_iterator} returns an iterator object.
#'
#'
#' @aliases is.iterator end_iteration iteration_has_ended new_iterator
#' @param x any object.
#' @param e a condition object.
#' @param nextElem a function object that takes no arguments.
#' @param \dots not currently used.
#' @keywords utilities
#' @examples
#'
#' # Manually iterate using the iteration_has_ended function to help
#' it <- iter(1:3)
#' tryCatch({
#'   stopifnot(is.iterator(it))
#'   repeat {
#'     print(nextElem(it))
#'   }
#' },
#' error=function(e) {
#'   if (!iteration_has_ended(e)) {
#'     stop(e)
#'   }
#' })
#'
#' @export is.iterator
is.iterator <- function(x) inherits(x, 'iter')
