#' Iterator that returns the Cartesian product of the arguments.
#'
#' Given a number of iterables as arguments, constructs an iterator that
#' is the Cartesian product of all arguments.
#'
#' Although they share the same end goal, \code{igrid} can yield drastic
#' memory savings compared to \code{\link[base]{expand.grid}}.
#'
#' @export
#' @param \dots Named iterables to iterate over. The right-most
#'   iterables change more quickly, like an odometer.
#' @return iterator that iterates through each element from the
#'   Cartesian product.
#' @details Based on [itertools2::iproduct] and
#'   [itertools::product]
#' @aliases iproduct
#' @examples
#' # Simulate a doubly-nested loop with a single while loop
#' it <- product(a=1:3, b=1:2)
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
#' # igrid is a replacement for base::expand.grid()
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
igrid <- function(...) {
  args <- list(...)
  n <- length(args)
  anames <- names(args)
  it <- igrid.internal(n, args)
  iteror.function(function(or) {
    val <- nextOr(it, return(or))
    names(val) <- anames
    val
  })
}

igrid.internal <- function(n, args) {
  if (n <= 1) {
    if (n == 1) {
      icar <- iteror(args[[1]])

      nextOr_ <- function(or) {
        list(nextOr(icar, return(or)))
      }
    } else {
      nextOr_ <- function(or) or
    }
  } else {
    icdr <- igrid.internal(n - 1, args[-n])
    cdrval <- NULL
    needval <- TRUE
    vals <- as.list(args[[n]])
    icar <- NULL

    nextOr_ <- function(or) {
      repeat {
        if (needval) {
          cdrval <<- nextOr(icdr, return(or))
          needval <<- FALSE
          icar <<- iteror.default(vals)
        }

        carval <- list(nextOr(icar, {
          needval <<- TRUE
          next
        }))
        break
      }
      c(cdrval, carval)
    }
  }

  iteror.function(nextOr_)
}

igrid2 <- function(..., times=1L) {
  args_list <- list(...)
  if (length(args_list) == 0) {
    stop("At least one argument must be supplied.")
  }

  # Determines the frequency and number of replicates each element in args_list
  # must be repeated
  args_lengths <- as.integer(vapply(args_list, length, 0))
  period <- prod(args_lengths)
  ix <- icount(period*times)

  nextOr_ <- function(or) {
    i <- nextOr(ix, return(or))
    vi <- arrayIndex(i, args_lengths)
    mapply(`[[`, args_list, vi, SIMPLIFY=FALSE)
  }

  iteror.function(nextOr_)
}

