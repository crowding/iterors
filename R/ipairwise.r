#' Iterator that returns elements of an object in pairs
#'
#' Constructs an iterator of an iterable \code{object} that returns its elements
#' in pairs.
#'
#' @export
#' @param object an iterable object
#' @return an iterator that returns pairwise elements
#'
#' @examples
#' it <- ipairwise(iterators::iter(letters[1:4]))
#' nextElemOr(it, NA) # list("a", "b")
#' nextElemOr(it, NA) # list("b", "c")
#' nextElemOr(it, NA) # list("c", "d")
#'
#' it2 <- ipairwise(1:5)
#' nextElemOr(it2, NA) # list(1, 2)
#' nextElemOr(it2, NA) # list(2, 3)
#' nextElemOr(it2, NA) # list(3, 4)
#' nextElemOr(it2, NA) # list(4, 5)
#'
ipairwise <- function(object) {
  it_tee <- itee(object, n=2)
  dev_null <- nextElemOr(it_tee[[2]], NA)

  nextElemOr_ <- function(or) {
    lapply(it_tee, nextElemOr, or=return(or))
  }

  iteror(nextElemOr_)
}
