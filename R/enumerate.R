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



#' Create an enumeration object
#'
#' Create an iterator that iterates over an iterable, returning the value in a
#' list that includes an index.
#'
#'
#' @param iterable Iterable to iterate over.
#' @keywords utilities
#' @details Originally from the `itertools` package.
#' @examples
#'
#' # Create an enumeration of five random numbers
#' as.list(enumerate(rnorm(5)))
#'
#' @export enumerate
enumerate <- function(iterable) {
  it <- iteror(iterable)
  e <- icount()
  izip(index=e, value=it)
}

icount <- function(n) {
  x <- 0
  if (missing(n))
    iteror(function(or) x <<- x + 1)
  else
    iteror(function(or) if (x < n) x <<- x + 1 else or)
}
