#
# Copyright (c) 2010, Stephen B. Weston
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



#' Create an iterator of indices
#'
#' Create an iterator of chunks of indices from 1 to \code{n}. You can specify
#' either the number of pieces, using the \code{chunks} argument, or the
#' maximum size of the pieces, using the \code{chunkSize} argument.
#'
#'
#' @param n Maximum index to generate.
#' @param \dots Passed as the second and subsequent arguments to \code{idiv}
#' function.  Currently, \code{idiv} accepts either a value for \code{chunks}
#' or \code{chunkSize}.
#' @return An iterator that returns vectors of indices from 1 to \code{n}.
#' @seealso \code{\link[iterators]{idiv}, \link{isplitVector}}
#' @keywords utilities
#' @examples
#'
#' # Return indices from 1 to 17 in vectors no longer than five
#' it <- ihasNext(isplitIndices(17, chunkSize=5))
#' while (hasNext(it)) {
#'   print(nextElem(it))
#' }
#'
#' # Return indices from 1 to 7 in four vectors
#' it <- ihasNext(isplitIndices(7, chunks=4))
#' while (hasNext(it)) {
#'   print(nextElem(it))
#' }
#'
#' @export isplitIndices
isplitIndices <- function(n, ...) {
  it <- idiv(n, ...)
  i <- 1L

  nextEl <- function() {
    m <- as.integer(nextElem(it))
    j <- i
    i <<- i + m
    seq(j, length=m)
  }

  object <- list(nextElem=nextEl)
  class(object) <- c('abstractiter', 'iter')
  object
}
