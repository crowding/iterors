#' Iterator that filters elements not satisfying a predicate function
#'
#' `ikeep(iterable, predicate)` constructs an iterator that filters
#' elements from iterable returning only those for which the predicate
#' is \code{TRUE}.
#'
#' @export
#' @aliases ifilter
#' @seealso idrop ikeepwhile idropwhile
#' @param iterable an iterable object.
#' @param predicate a function that determines whether an element is
#'   \code{TRUE} or \code{FALSE}. The function is assumed to take only
#'   one argument.
#' @details Originally called 'ifilter' from package
#'   `itertools`. Renamed because the order of arguments has changed
#'   to put the iterable in the first argument, the better to be used
#'   with the `|>` operator.
#' @return iterator object
#'
#' @examples
#' # Filters out odd numbers and retains only even numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- ikeep(1:10, is_even)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out even
#' # numbers
#' it2 <- ikeep(1:10, function(x) x %% 2 == 1)
#' nextOr(it2, NA) # 1
#' nextOr(it2, NA) # 3
#' nextOr(it2, NA) # 5
#' nextOr(it2, NA) # 7
#' nextOr(it2, NA) # 9
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- ikeep(letters, is_vowel)
#' as.list(it3)
ikeep <- function(iterable, predicate) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must be a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- nextOr(iter_obj, return(or))
      if (predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror(nextOr_)
}

#' `idrop(iterable, predicate)` constructs an iterator that filters
#' elements from iterable returning only those for which the predicate
#' is \code{FALSE}.
#'
#' @export
#' @aliases ifilterfalse
#' @examples
#' # Filters out even numbers and retains only odd numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- idrop(1:10, is_even)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out odd
#' # numbers
#' it2 <- ikeep(1:10, function(x) x %% 2 == 1)
#' as.list(it2)
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- idrop(letters, is_vowel)
#' nextOr(it3, NA) # b
#' nextOr(it3, NA) # c
#' nextOr(it3, NA) # d
#' nextOr(it3, NA) # f
#' nextOr(it3, NA) # g
#' # nextOr(it, NA) continues through the rest of the consonants
#'
#' @rdname ikeep
idrop <- function(iterable, predicate) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- iter_obj(or = return(or))
      if (!predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror(nextOr_)
}
