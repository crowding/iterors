#
# Copyright (c) 2009-2010, Stephen B. Weston
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



#' Create a limited iterator
#'
#' Create an iterator that wraps a specified iterable a limited number of
#' times.
#'
#'
#' @param iterable Iterable to iterate over.
#' @param n Maximum number of values to return.
#' @keywords utilities
#' @examples
#'
#' # Limit icount to only return three values
#' as.list(ilimit(icount(), 3))
#'
#' @export ilimit
ilimit <- function(iterable, n) {
  it <- iter(iterable)
  n <- as.integer(n)

  nextEl <- function() {
    if (n <= 0) {
      stop('StopIteration', call.=FALSE)
    }
    n <<- n - 1L
    nextElem(it)
  }

  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}
