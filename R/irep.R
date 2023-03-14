#
# Copyright (c) 2010, Stephen B. Weston
# Updated 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA



#' Create a repeating iterator
#'
#' Create an iterator version of the \code{rep} function.
#'
#'
#' @param iterable The iterable to iterate over repeatedly.
#' @param times A vector giving the number of times to repeat each element if
#' the length is greater than one, or to repeat all the elements if the length
#' is one.  This behavior is less strict than \code{rep} since the length of an
#' iterable isn't generally known.
#' @param length.out non-negative integer.  The desired length of the output
#' iterator.
#' @param each non-negative integer.  Each element of the iterable is repeated
#' \code{each} times.
#' @seealso \code{\link[base]{rep}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' unlist(as.list(irep(1:4, 2)))
#' unlist(as.list(irep(1:4, each=2)))
#' unlist(as.list(irep(1:4, c(2,2,2,2))))
#' unlist(as.list(irep(1:4, c(2,1,2,1))))
#' unlist(as.list(irep(1:4, each=2, len=4)))
#' unlist(as.list(irep(1:4, each=2, len=10)))
#' unlist(as.list(irep(1:4, each=2, times=3)))
#'
#' @export irep
irep <- function(iterable, times=NULL, length.out=NULL, each=NULL) {
  # Apply "each" first
  it <- if (!is.null(each)) {
    irep.each(iteror(iterable), each)
  } else {
    iteror(iterable)
  }

  if (!is.null(length.out)) {
    # Ignore "times" if "length.out" is specified
    ilimit(recycle(it), length.out)
  } else if (!is.null(times)) {
    if (length(times) == 1) {
      # If "times" has a single value, recycle that many times
      recycle(it, times)
    } else {
      # If "times" has multiple values, it's kind of like "each"
      irep.times(it, times)
    }
  } else {
    # Neither "length.out" or "times" was specified
    it
  }
}

# Internal function used to handle the irep "each" argument
irep.each <- function(it, each) {
  each <- as.integer(each[1])

  if (is.na(each)) {
    each <- 1L
  } else if (each < 0) {
    stop("invalid 'each' argument")
  }

  n <- 0L
  value <- NULL

  nextOr_ <- if (each == 0) {
    function(or) or
  } else if (each == 1) {
    function(or) nextOr(it, or)
  } else {
    function(or) {
      if (n <= 0) {
        value <<- nextOr(it, or)
        n <<- each
      }
      n <<- n - 1L
      value
    }
  }

  iteror(nextOr_)
}

# Internal function used to handle the irep "times" argument
irep.times <- function(it, times) {
  it <- iteror(it)
  times <- as.integer(times)
  if (length(times) == 0 || any(is.na(times) | times < 0)) {
    stop("invalid 'times' argument")
  }

  i <- 0L
  n <- 0L
  value <- NULL

  nextOr_ <- function(or) {
    while (n <= 0 && i < length(times)) {
      i <<- i + 1L
      n <<- times[i]
      value <<- nextOr(it, return(or))
    }
    if (n <= 0) {
      or
    } else {
      n <<- n - 1L
      value
    }
  }

  iteror(nextOr_)
}
