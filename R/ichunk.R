#
# Copyright (c) 2009-2013, Stephen B. Weston
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



#' Create a chunking iterator
#'
#' Create an iterator that issues lists of values from the underlying iterable.
#' This is useful for manually \dQuote{chunking} values from an iterable.
#'
#'
#' @param iterable Iterable to iterate over.
#' @param chunkSize Maximum number of values from \code{iterable} to return in
#' each value issued by the resulting iterator.
#' @param mode Mode of the objects returned by the iterator.
#' @seealso \code{\link{isplitVector}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Split the vector 1:10 into "chunks" with a maximum length of three
#' it <- ichunk(1:10, 3)
#' repeat print(unlist(nextOr(it, break)))
#'
#' # Same as previous, but return integer vectors rather than lists
#' it <- ichunk(1:10, 3, mode='integer')
#' repeat print(unlist(nextOr(it, break)))
#'
#' @export ichunk
ichunk <- function(iterable, chunkSize, mode='list') {
  force(iterable)
  force(chunkSize)
  it <- iteror(iterable)

  legal.modes <- c('list', 'logical', 'integer', 'numeric', 'double',
                   'complex', 'character', 'raw')
  if (! mode %in% legal.modes)
    stop(sprintf("cannot make a vector of mode '%s'", mode))

  nextOr.list <- function(or) {
    r <- vector('list', chunkSize)
    i <- 0L

    while (i < chunkSize) {
      r[i + 1L] <- list(nextOr(it, {
        if (i == 0L) return(or)
        length(r) <- i
        break
      }))
      i <- i + 1L
    }

    r
  }

  nextOr.vector <- function(or) {
    r <- vector(mode, chunkSize)
    i <- 0L

    while (i < chunkSize) {
      r[i + 1L] <- nextOr(it, {
        if (i == 0L) return(or)
        length(r) <- i
        break
      })
      i <- i + 1L
    }

    r
  }

  object <- if (mode == 'list')
    iteror.function(nextOr.list)
  else
    iteror.function(nextOr.vector)
}
