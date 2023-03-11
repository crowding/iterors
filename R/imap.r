#' Iterator that applies a given function to several iterables concurrently.
#'
#' Constructs an iterator that computes the given function \code{f} using the
#' arguments from each of the iterables given in \code{...}.
#'
#' The iterator returned is exhausted when the shortest iterable in \code{...}
#' is exhausted. Note that \code{imap} does not recycle arguments as
#' \code{\link[base]{Map}} does.
#'
#' The primary difference between \code{istarmap} and
#' \code{\link[itertools2]{imap}} is that the former expects an iterable object
#' whose elements are already grouped together, while the latter case groups the
#' arguments together before applying the given function. The choice is a matter
#' of style and convenience.
#'
#' @export
#' @param f a function
#' @param ... multiple arguments to iterate through in sequence
#' @return iterator that returns the values of \code{object} along with the
#' index of the object.
#'
#' @examples
#' pow <- function(x, y) {
#'   x^y
#' }
#' it <- imap(pow, c(2, 3, 10), c(5, 2, 3))
#' as.list(it)
#'
#' # Similar to the above, but because the second vector is exhausted after two
#' # calls to `nextElem`, the iterator is exhausted.
#' it2 <- imap(pow, c(2, 3, 10), c(5, 2))
#' as.list(it2)
#'
#' # Another similar example but with lists instead of vectors
#' it3 <- imap(pow, list(2, 3, 10), list(5, 2, 3))
#' nextOr(it3, NA) # 32
#' nextOr(it3, NA) # 9
#' nextOr(it3, NA) # 1000
imap <- function(f, ...) {
  iter_obj <- izip(...)

  wrap <- function(l) {
    lapply(l, function(x) if (is.language(x)) call("quote", x) else x)
  }

  nextOr_ <- function(or) {
    elem <- nextOr(iter_obj, return(or))
    do.call(f, wrap(elem))
  }

  iteror(nextOr_)
}
