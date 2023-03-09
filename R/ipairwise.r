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
#' nextOr(it, NA) # list("a", "b")
#' nextOr(it, NA) # list("b", "c")
#' nextOr(it, NA) # list("c", "d")
#'
#' it2 <- ipairwise(1:5)
#' nextOr(it2, NA) # list(1, 2)
#' nextOr(it2, NA) # list(2, 3)
#' nextOr(it2, NA) # list(3, 4)
#' nextOr(it2, NA) # list(4, 5)
#'
ipairwise <- function(object) {
  it_tee <- itee(object, n=2)
  dev_null <- nextOr(it_tee[[2]], NA)

  nextOr_ <- function(or) {
    lapply(it_tee, nextOr, or=return(or))
  }

  iteror(nextOr_)
}
