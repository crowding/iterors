#' Iterator that returns elements in fixed-length chunks
#'
#' Constructs an iterator that returns elements of an iterable \code{object} in
#' fixed-length chunks. If the length of the iterator is not divisible by
#' \code{chunk_size}, the remainder of the last block is filled with the value
#' specified in \code{fill}.
#'
#' This function corresponds to Python's \code{grouper} function. We chose the
#' name \code{ichunk} because it more explicitly defines the function's purpose.
#'
#' @export
#' @param object an iterable object
#' @param chunk_size the number of elements returned per chunk
#' @param fill the value with which to fill the last chunk if the length of the
#' iterator is not divisble by \code{chunk_size}
#' @return each call to \code{nextElem} results in a list of length
#' \code{chunk_size}
#'
#' @examples
#' it <- ichunk(iterators::iter(1:5), chunk_size=2)
#' # List: list(1, 2, 3)
#' nextOr(it, NA)
#' # List: list(4, 5, NA)
#' nextOr(it, NA)
#'
#' it2 <- ichunk(levels(iris$Species), chunk_size=4, "weeee")
#' # Returns: list("setosa", "versicolor", "virginica", "weeee")
#' nextOr(it2, NA)
#'
ichunk <- function(object, chunk_size=1, fill=NA) {
  if (chunk_size <= 0 | !is.numeric(chunk_size) | length(chunk_size) != 1) {
    stop("'chunk_size' must be a positive integer of length 1")
  }
  chunk_size <- as.integer(chunk_size)

  it <- iteror(object)
  it_replicate <- replicate(n=chunk_size, it, simplify=FALSE)

  nextOr_ <- function(or) {
    out <- vector("list", chunk_size)
    for (i in seq_len(chunk_size)) {
      out[i] <- list(nextOr(it, if (i == 1) return(or) else fill))
    }
    out
  }

  iteror(nextOr_)
}
