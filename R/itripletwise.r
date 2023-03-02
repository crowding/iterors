#' Iterator that returns adjacent triplets from an iterator
#'
#' Constructs an iterator of an iterable \code{object} that returns its elements
#' in a sliding window three elements wide.
#'
#' @importFrom iterators nextElem iter
#' @export
#' @param object an iterable object
#' @return an iterator that returns tripletwise elements
#'
#' @examples
#' it <- itripletwise(iteror(letters[1:4]))
#' nextOr(it) # list("a", "b", "c")
#' nextOr(it) # list("b", "c", "d")
#'
#' it2 <- itripletwise(1:5)
#' nextOr(it2) # list(1, 2, 3)
#' nextOr(it2) # list(2, 3, 4)
#' nextOr(it2) # list(3, 4, 5)
#'
itripletwise <- function(object) {
  object <- iteror(object)

  last_2 <- nextOr(object, NULL)
  last_1 <- nextOr(object, NULL)
  nextOr_ <- function(or) {
    value <- list(last_2,
                  last_1,
                  nextOr(object, return(or)))
    last_2 <<- last_1
    last_1 <<- value[[3]]
    value
  }

  iteror(nextOr_)
}
