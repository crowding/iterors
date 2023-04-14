#
# Copyright (c) 2009-2010, Stephen B. Weston
# Translated 2013 by Peter Meilstrup
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



#' Create an iterator over multiple iterables
#'
#' Create an iterator that iterates over multiple iterables, returning the
#' values as a list.
#'
#'
#' @param \dots The iterables to iterate over.
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Iterate over two iterables of different sizes
#' as.list(izip(a=1:2, b=letters[1:3]))
#'
#' @export izip
izip <- function(...) {
  iterators <- lapply(list(...), iteror)

  if (length(iterators) == 0) {
    nextOr_ <- function(or) or
  } else {
    nextOr_ <- function(or) {
      lapply(iterators, nextOr, return(or))
    }
  }

  iteror.internal(nextOr_)
}
