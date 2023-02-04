#' Iterator that filters elements not satisfying a predicate function
#'
#' Constructs an iterator that filters elements from iterable returning only
#' those for which the predicate is \code{TRUE}.
#'
#' @export
#' @param predicate a function that determines whether an element is \code{TRUE}
#' or \code{FALSE}. The function is assumed to take only one argument.
#' @param iterable an iterable object
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
#' it2 <- ifilter(function(x) x %% 2 == 1, 1:10)
#' nextElemOr(it2, NA) # 1
#' nextElemOr(it2, NA) # 3
#' nextElemOr(it2, NA) # 5
#' nextElemOr(it2, NA) # 7
#' nextElemOr(it2, NA) # 9
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- ifilter(is_vowel, letters)
#' as.list(it3)
ifilter <- function(predicate, iterable) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must be a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable)

  nextElemOr_ <- function(or) {
    repeat {
      next_elem <- nextElemOr(iter_obj, return(or))
      if (predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror(nextElemOr_)
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
#' it <- ifilterfalse(is_even, 1:10)
#' as.list(it)
#'
#' # Similar idea here but anonymous function is used to filter out odd
#' # numbers
#' it2 <- ifilter(function(x) x %% 2 == 1, 1:10)
#' as.list(it2)
#'
#' is_vowel <- function(x) {
#'   x %in% c('a', 'e', 'i', 'o', 'u')
#' }
#' it3 <- ifilterfalse(is_vowel, letters)
#' nextElemOr(it3, NA) # b
#' nextElemOr(it3, NA) # c
#' nextElemOr(it3, NA) # d
#' nextElemOr(it3, NA) # f
#' nextElemOr(it3, NA) # g
#' # nextElemOr(it, NA) continues through the rest of the consonants
#'
#' @rdname ifilter
ifilterfalse <- function(predicate, iterable) {
  if (!is.function(predicate)) {
    stop("The 'predicate' must a function that returns TRUE or FALSE.")
  }

  iter_obj <- iteror(iterable)

  nextElemOr_ <- function(or) {
    repeat {
      next_elem <- nextElemOr(iter_obj, return(or))
      if (!predicate(next_elem)) {
        return(next_elem)
      }
    }
  }

  iteror(nextElemOr_)
}
