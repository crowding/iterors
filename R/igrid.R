#' Iterator that returns the Cartesian product of the arguments.
#'
#' Given a number of iterables as arguments, constructs an iterator that
#' is the Cartesian product of all arguments.
#'
#' Although they share the same end goal, \code{igrid} can yield drastic
#' memory savings compared to \code{\link[base]{expand.grid}}.
#'
#' @export igrid
#' @param \dots Named iterables to iterate over. The right-most
#'   iterables change more quickly, like an odometer.
#' @param rowMajor If TRUE, the left-most indices change fastest.
#' @param simplify If TRUE, results are returned in a vector (or
#'   matrix if chunking). If FALSE, results are returned as a list.
#' @return iterator that iterates through each element from the
#'   Cartesian product of its arguments
#' @details Based on [itertools2::iproduct] and [itertools::product].
#' @aliases iproduct
#' @examples
#' # Simulate a doubly-nested loop with a single while loop
#' it <- igrid(a=1:3, b=1:2)
#' repeat {
#'   x <- nextOr(it, break)
#'   cat(sprintf('a = %d, b = %d\n', x$a, x$b))
#' }
#'
#' it <- igrid(x=1:3, y=4:5)
#' nextOr(it, NA) # list(x=1, y=4)
#' nextOr(it, NA) # list(x=1, y=5)
#' nextOr(it, NA) # list(x=2, y=4)
#' nextOr(it, NA) # list(x=2, y=5)
#' nextOr(it, NA) # list(x=3, y=4)
#' nextOr(it, NA) # list(x=3, y=5)
#'
#' # Second Cartesian product
#' nextOr(it, NA) # list(x=1, y=4)
#' nextOr(it, NA) # list(x=1, y=5)
#' nextOr(it, NA) # list(x=2, y=4)
#' nextOr(it, NA) # list(x=2, y=5)
#' nextOr(it, NA) # list(x=3, y=4)
#' nextOr(it, NA) # list(x=3, y=5)
#'
#' # igrid is an iterator equivalent to base::expand.grid()
#' # Large data.frames are not created unless the iterator is manually consumed
#' a <- 1:2
#' b <- 3:4
#' c <- 5:6
#' it3 <- igrid(a=a, b=b, c=c)
#' df_igrid <- do.call(rbind, as.list(it3))
#' df_igrid <- data.frame(df_igrid)
#'
#' # Compare df_igrid with the results from base::expand.grid()
#' base::expand.grid(a=a, b=b, c=c)
igrid <- function(..., simplify=FALSE, rowMajor=FALSE) {
  args <- list(...)
  args_table <- c(if (simplify) NULL else list(),
                  ..., use.names=FALSE)
  dim <- vapply(args, length, 0)
  names(args_table) <- rep(names(args), dim)
  indexer <- arrayIndexer(dim, rowMajor=rowMajor, offset=TRUE)
  count <- prod(dim)

  ix <- 0
  nextOr_ <- function(or) {
    if (ix < count) {
      ix <<- ix + 1
      args_table[indexer(ix)]
    } else or
  }

  iteror.internal(nextOr_, "basicIteror")
}
