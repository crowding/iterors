#
# Copyright (c) 2009-2010, Stephen B. Weston
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



#' Create a recycling iterator
#'
#' Create an iterator that recycles a specified iterable.
#'
#'
#' @param iterable The iterable to recycle.
#' @param times integer.  Number of times to recycle the values in the
#' iterator.  Default value of \code{NA_integer_} means to recycle forever.
#' @keywords utilities
#' @examples
#'
#' # Recycle over 'a', 'b', and 'c' three times
#' recycle(letters[1:3], 3)
#'
#' @details Originally from the `itertools` package.
#' @export irecycle
irecycle <- function(iterable, times=NA_integer_) {
  # Manually check for a missing argument since "inherits" issues
  # a cryptic error message in that case
  if (missing(iterable)) {
    stop('argument "iterable" is missing, with no default')
  }

  if (!is.numeric(times) || length(times) != 1 || (!is.na(times) && times < 0)) {
    stop('argument "times" must be a non-negative numeric value')
  }

  times <- as.integer(times)

  if (is.na(times) || times > 1) {
    if (! inherits(iterable, 'iter')) {
      buffer <- iterable
      buffer.iter <- iteror(buffer)
    } else {
      iterable.iter <- iteror(iterable)
      bsize <- 256  # allocated size of buffer
      bsize.max <- 2 ^ 31 - 1  # maximum allowable allocated size of buffer
      buffer <- vector('list', length=bsize)
      blen <- 0  # number of values currently in buffer
      buffer.iter <- NULL  # will become an iterator over buffer
    }
  } else if (times > 0) {
    iterable.iter <- iteror(iterable)
  }

  # This is used until the underlying iterator runs out
  nextOr.buffering <- function(or) {
    # Check if buffer is full
    if (blen >= bsize) {
      # Don't attempt to create a list with more than 2^31-1 elements
      if (blen == bsize.max) {
        stop('underlying iterator has too many values to buffer')
      }
      # Double the size of buffer
      bsize <<- min(2 * bsize, bsize.max)
      length(buffer) <<- bsize
    }
    e <- nextOr(iterable.iter, {
      times <<- times - 1L  # will still be greater than zero
      length(buffer) <<- blen
      iterable <<- NULL
      iterable.iter <<- NULL
      buffer.iter <<- iteror(buffer)
      nextOr.pointer <<- nextOr.cycling
      return(nextOr.pointer(or))
    })
    blen <<- blen + 1
    buffer[blen] <<- list(e)
    e

  }

  # This will be used once we've run through the underlying iterator
  nextOr.cycling <- function(or) {
    nextOr(buffer.iter, {
      if (!is.na(times) && times <= 1) {
        times <<- 0L
        return(or)
      }
      times <<- times - 1L
      buffer.iter <<- iteror(buffer)
      # If this throws 'StopIteration', we're done
      nextOr(buffer.iter, or)
    })
  }

  # This handles the case when "times" is one (pretty useless case)
  nextOr.one <- function(or) {
    nextOr(iterable.iter, or)
  }

  # This handles the case when "times" is zero
  nextOr.zero <- function(or) {
    or
  }

  # Set the initial value of nextOr.pointer
  if (is.na(times) || times > 1) {
    nextOr.pointer <- if (is.null(buffer.iter)) nextOr.buffering else nextOr.cycling
  } else if (times == 1) {
    nextOr.pointer <- nextOr.one
  } else {
    nextOr.pointer <- nextOr.zero
  }

  # This is the function that will be stored in the iterator object,
  # which will call either nextOr.buffering of nextOr.cycling, depending
  # on the value of nextOr.pointer variable
  nextOr_ <- function(or) {
    nextOr.pointer(or)
  }

  iteror(nextOr_)
}
