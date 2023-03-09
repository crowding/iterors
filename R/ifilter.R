#' Iterator that filters elements not satisfying a predicate function
#'
#' Constructs an iterator that filters elements from iterable returning only
#' those for which the predicate is \code{TRUE}.
#'
#' @export
#' @param predicate a function that determines whether an element is
#'   \code{TRUE} or \code{FALSE}. The function is assumed to take only
#'   one argument.
#' @param iterable an iterable object
#' @details Originally from package `itertools2`, however the order of
#'   arguments has been reversed (so that the pre-filtering iterator appears on teh left side of a pipe. 
#' @return iterator object
#'
#' @examples
#' # Filters out odd numbers and retains only even numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- ifilter(is_even, 1:10)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out even
#' # numbers
#' it2 <- ifilter(1:10, function(x) x %% 2 == 1)
#' nextOr(it2, NA) # 1
#' nextOr(it2, NA) # 3
#' nextOr(it2, NA) # 5
#' nextOr(it2, NA) # 7
#' nextOr(it2, NA) # 9
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- ifilter(letters, is_vowel)
#' as.list(it3)
ifilter <- function(iterable, predicate) {
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

#' Iterator that filters elements not satisfying a predicate function
#'
#' Constructs an iterator that filters elements from iterable returning only
#' those for which the predicate is \code{FALSE}.
#'
#' @export
#' @examples
#' # Filters out even numbers and retains only odd numbers
#' is_even <- function(x) {
#'   x %% 2 == 0
#' }
#' it <- ifilterfalse(1:10, is_even)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out odd
#' # numbers
#' it2 <- ifilter(1:10, function(x) x %% 2 == 1)
#' as.list(it2)
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- ifilterfalse(letters, is_vowel)
#' nextOr(it3, NA) # b
#' nextOr(it3, NA) # c
#' nextOr(it3, NA) # d
#' nextOr(it3, NA) # f
#' nextOr(it3, NA) # g
#' # nextOr(it, NA) continues through the rest of the consonants
#'
#' @rdname ifilter
ifilterfalse <- function(iterable, predicate) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable)

  nextOr_ <- function(or) {
    repeat {
      next_elem <- nextOr(iter_obj, return(or))
      if (!predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror(nextOr_)
}
