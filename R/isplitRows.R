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



#' Create an iterator that splits a matrix into block rows
#'
#' Create an iterator that splits a matrix into block rows. You can specify
#' either the number of blocks, using the \code{chunks} argument, or the
#' maximum size of the blocks, using the \code{chunkSize} argument.
#'
#'
#' @param x Matrix to iterate over.
#' @param \dots Passed as the second and subsequent arguments to \code{idiv}
#' function.  Currently, \code{idiv} accepts either a value for \code{chunks}
#' or \code{chunkSize}.
#' @return An iterator that returns submatrices of \code{x}.
#' @seealso \code{\link[iterators]{idiv}, \link{isplitCols}}
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Split a matrix into submatrices with a maximum of three rows
#' x <- matrix(1:100, 10)
#' it <- isplitRows(x, chunkSize=3)
#' repeat print(nextOr(it, break))
#'
#' # Split the same matrix into five submatrices
#' it <- isplitRows(x, chunks=5)
#' repeat print(nextOr(it, break))
#'
#' @export isplitRows
isplitRows <- function(x, ...) {
  it <- idiv(nrow(x), ...)
  i <- 1L

  nextOr_ <- function(or) {
    n <- as.integer(nextOr(it, return(or)))
    j <- i
    i <<- i + n
    x[seq(j, length=n),, drop=FALSE]
  }

  iteror.internal(nextOr_)
}
