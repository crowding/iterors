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



#' Create a chaining iterator
#'
#' Create an iterator that chains multiple iterables together.
#'
#'
#' @param \dots The iterables to iterate over.
#' @keywords utilities
#' @examples
#'
#' # Iterate over two iterables
#' as.list(chain(1:2, letters[1:3]))
#'
#' @export chain
chain <- function(...) {
  iterators <- lapply(list(...), iteror)

  nextOr_ <- function(or) {
    repeat {
      if (length(iterators) == 0) {
        return(or)
      } else {
        return(nextOr(iterators[[1]], {
          iterators <<- iterators[-1]
          next
        }))
      }
    }
  }

  iteror.function(nextOr_)
}
