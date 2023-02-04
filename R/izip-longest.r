#' Iterator that iterates through several iterables concurrently.
#'
#' The resulting iterator aggregates elements from each of the iterables into a
#' list from each iteration. Used for lock-step iteration over several iterables
#' at a time.
#'
#' Although similar to \code{\link[itertools2]{izip}}, missing values are
#' replaced with \code{fill} if the iterables are of uneven length, and
#' Iteration continues until the longest iterable is exhausted.
#'
#' @importFrom iterators iter nextElem
#' @export
#' @param ... multiple arguments to iterate through in sequence
#' @param fill the value used to replace missing values when the iterables in
#' \code{...} are of uneven length
#' @return iterator that iterates through each argument in sequence
#'
#' @examples
#' it <- izip_longest(x=1:3, y=4:6, z=7:9)
#' nextElemOr(it, NA) # list(x=1, y=4, z=7)
#' nextElemOr(it, NA) # list(x=2, y=5, z=8)
#' nextElemOr(it, NA) # list(x=3, y=6, z=9)
#'
#' it2 <- izip_longest(1:3, 4:8)
#' nextElemOr(it2, NA) # list(1, 4)
#' nextElemOr(it2, NA) # list(2, 5)
#' nextElemOr(it2, NA) # list(3, 6)
#' nextElemOr(it2, NA) # list(NA, 7)
#' nextElemOr(it2, NA) # list(NA, 8)
#'
#' it3 <- izip_longest(1:2, 4:7, levels(iris$Species), fill="w00t")
#' nextElemOr(it3, NA) # list(1, 4, "setosa")
#' nextElemOr(it3, NA) # list(2, 5, "versicolor")
#' nextElemOr(it3, NA) # list("w00t", 6, "virginica")
#' nextElemOr(it3, NA) # list("w00t", 7, "w00t")
izip_longest <- function(..., fill=NA) {
  iter_list <- lapply(list(...), iteror)
  if (length(iter_list) == 0) {
    stop("At least one argument must be supplied.")
  }
  running <- rep(TRUE, length(iter_list))

  nextElemOr_ <- function(or) {
    if (all(!running)) return(or)
    out <- rep(list(fill), length(iter_list))
    for (i in seq_along(iter_list)) {
      if (running[i]) {
        out[i] <- list(nextElemOr(iter_list[[i]], {
          running[i] <<- FALSE
          fill
        }))
      }
    }
    if (all(!running)) return(or)
    out
  }

  iteror.function(nextElemOr_)
}
