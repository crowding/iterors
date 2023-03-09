#' Iteror that chains multiple arguments together into a single iterator
#'
#' Generates an [iteror] that returns elements from the first argument until it
#' is exhausted. Then generates an iterator from the next argument and returns
#' elements from it. This process continues until all arguments are exhausted
#' Chaining is useful for treating consecutive sequences as a single sequence.
#'
#' @export
#' @param ... multiple arguments to iterate through in sequence
#' @return iteror that iterates through each argument in sequence
#'
#' @examples
#' it <- ichain(1:3, 4:5, 6)
#' as.list(it)
#'
#' it2 <- ichain(1:3, levels(iris$Species))
#' as.list(it2)
ichain <- function(...) {
  iter_list <- lapply(list(...), iteror)
  num_args <- length(iter_list)

  if (num_args == 0) {
    stop("At least one argument must be supplied.")
  }

  arg_i <- 1

  nextOr_ <- function(or) {
    repeat {
      if (arg_i > num_args) return(or)
      return(nextOr(iter_list[[arg_i]], {
        arg_i <<- arg_i + 1
        next
      }))
    }
    elem
  }

  iteror(nextOr_)
}
