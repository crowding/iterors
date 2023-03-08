#
# Copyright (c) 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3 of the License, or
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


#' Does This Iterator Have A Next Element
#'
#' \code{hasNext} is a generic function that indicates if the iterator
#' has another element.
#'
#'
#' @aliases hasNext hasNext.ihasNext
#' @param obj an iterator object.
#' @param \dots additional arguments that are ignored.
#' @return Logical value indicating whether the iterator has a next element.
#' @keywords methods
#' @examples
#'
#'   it <- ihasNext(iteror(c('a', 'b', 'c')))
#'   while (hasNext(it))
#'     print(nextOr(it))
#'
#' @export hasNext
hasNext <- function(obj, ...) {
  UseMethod("hasNext")
}

#' @exportS3Method
hasNext.ihasNext <- function(obj, ...) {
  obj$hasNext()
}

#' @exportS3Method
ihasNext.iteror <- function(iter, ...) {
  noValue <- new.env() #sigil values
  endIter <- new.env()
  last <- noValue
  nextOr_ <- function(or, ...) {
    if (identical(last, noValue))
      last <<- nextOr(iter, endIter)
    if (identical(last, endIter))
      or
    else {
      tmp <- last
      last <<- noValue
      tmp
    }
  }

  hasNext_ <- function() {
    if (identical(last, noValue))
      last <<- nextOr(iter, endIter)
    !identical(last, endIter)
  }

  it <- iteror.function(nextOr_)
  it$hasNext <- hasNext_
  structure(it, class=c("ihasNext", class(it)))
}
