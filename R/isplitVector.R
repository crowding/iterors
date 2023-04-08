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



#' Create an iterator that splits a vector
#'
#' Create an iterator that splits a vector into smaller pieces. You can specify
#' either the number of pieces, using the \code{chunks} argument, or the
#' maximum size of the pieces, using the \code{chunkSize} argument.
#'
#'
#' @param x Vector to iterate over.  Note that it doesn't need to be an atomic
#' vector, so a list is acceptable.
#' @param \dots Passed as the second and subsequent arguments to \code{idiv}
#' function.  Currently, \code{idiv} accepts either a value for \code{chunks}
#' or \code{chunkSize}.
#' @return An iterator that returns vectors of the same type as \code{x} with
#' one or more elements from \code{x}.
#' @seealso \code{\link[iterators]{idiv}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Split the vector 1:10 into "chunks" with a maximum length of three
#' it <- ihasNext(isplitVector(1:10, chunkSize=3))
#' while (hasNext(it)) {
#'   print(nextOr(it))
#' }
#'
#' # Split the vector "letters" into four chunks
#' it <- ihasNext(isplitVector(letters, chunks=4))
#' while (hasNext(it)) {
#'   print(nextOr(it))
#' }
#'
#' # Get the first five elements of a list as a list
#' nextOr(isplitVector(as.list(letters), chunkSize=5))
#' @details Originally appeared in package `itertools`.
#' @export isplitVector
isplitVector <- function(x, ...) {
  it <- idiv(length(x), ...)
  i <- 1L

  nextOr_ <- function(or) {
    n <- as.integer(nextOr(it, return(or)))
    j <- i
    i <<- i + n
    x[seq(j, length=n)]
  }

  iteror.internal(nextOr_)
}
