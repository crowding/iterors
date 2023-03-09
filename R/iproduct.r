#' Iterator that returns the Cartesian product of the arguments.
#'
#' Given a number of vectors as arguments, constructs an iterator that
#' is the Cartesian product of all arguments.
#'
#' Although they share the same end goal, \code{iproduct} can yield drastic
#' memory savings compared to \code{\link[base]{expand.grid}}.
#'
#' @export
#' @param ... multiple vectors.
#' @param times the number of times the Cartesian product is repeated. By
#' default, repeated only once.
#' @return iterator that iterates through each element from the Cartesian
#' product
#'
#' @examples
#' it <- iproduct(x=1:3, y=4:5)
#' nextOr(it, NA) # list(x=1, y=4)
#' nextOr(it, NA) # list(x=1, y=5)
#' nextOr(it, NA) # list(x=2, y=4)
#' nextOr(it, NA) # list(x=2, y=5)
#' nextOr(it, NA) # list(x=3, y=4)
#' nextOr(it, NA) # list(x=3, y=5)
#'
#' # Repeats the Cartesian product twice
#' it <- iproduct(x=1:3, y=4:5, times=2)
#' # First Cartesian product
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
#' # iproduct is a replacement for base::expand.grid()
#' # Large data.frames are not created unless the iterator is manually consumed
#' a <- 1:2
#' b <- 3:4
#' c <- 5:6
#' it3 <- iproduct(a=a, b=b, c=c)
#' df_iproduct <- do.call(rbind, as.list(it3))
#' df_iproduct <- data.frame(df_iproduct)
#'
#' # Compare df_iproduct with the results from base::expand.grid()
#' base::expand.grid(a=a, b=b, c=c)
#'
iproduct <- function(..., times=1) {
  args_list <- list(...)
  if (length(args_list) == 0) {
    stop("At least one argument must be supplied.")
  }

  # Determines the frequency and number of replicates each element in args_list
  # must be repeated
  args_lengths <- vapply(args_list, length, 0)
  freq <- cumprod(c(1, rev(args_lengths[-1])))
  freq <- rev(unname(freq))
  rep_times <- unname(prod(args_lengths) / freq / args_lengths)

  # To repeat the Cartesian product multiple times, the 'rep_times' is simply
  # scaled by 'times'
  rep_times <- rep_times * times

  # For each element in args_list, an iterator is constructed that repeats each
  # of its values with the proper frequency and cadence.
  args_iters <- mapply(irep,
                       args_list,
                       times=rep_times,
                       each=freq,
                       SIMPLIFY=FALSE)

  # Finally, izip's the list of iterators
  do.call(izip, args=args_iters)
}
