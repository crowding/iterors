#' Iterator that iterates through several iterables concurrently.
#'
#' The resulting iterator aggregates elements from each of the iterables into a
#' list from each iteration. Used for lock-step iteration over several iterables
#' at a time.
#'
#' @importFrom iterators iter nextElem
#' @export
#' @param ... multiple arguments to iterate through in sequence
#' @return iteror that iterates through each argument in sequence
#'
#' @examples
#' it <- izip(x=1:3, y=4:6, z=7:9)
#' nextOr(it, NULL) # list(x=1, y=4, z=7)
#' nextOr(it, NULL) # list(x=2, y=5, z=8)
#' nextOr(it, NULL) # list(x=3, y=6, z=9)
#'
#' # Sums the zip'd elements. 1 + 4 + 7, and so on.
#' it2 <- izip(1:3, 4:6, 7:9)
#' sum_zip <- sapply(it2, function(x) sum(unlist(x)))
#' sum_zip == c(12, 15, 18)
#'
#' it3 <- izip(a=1:3, b=4:42, class=levels(iris$Species))
#' nextOr(it3, NULL) # list(a=1, b=4, class="setosa")
#' nextOr(it3, NULL) # list(a=2, b=5, class="versicolor")
#' nextOr(it3, NULL) # list(a=3, b=6, class="virginica")
izip <- function(...) {
  iter_list <- lapply(list(...), iteror)
  if (length(iter_list) == 0) {
    stop("At least one argument must be supplied.")
  }

  nextOr_ <- function(or) {
    out <- iter_list
    for (i in seq_along(out)) {
      out[i] <- list(nextOr(out[[i]], return(or)))
    }
    out
  }

  iteror(nextOr_)
}
