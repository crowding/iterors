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



#' Does This Iterator Have A Next Element
#'
#' \code{hasNext} is a generic function that indicates if the iterator has
#' another element.
#'
#'
#' @aliases hasNext hasNext.ihasNext
#' @param obj an iterator object.
#' @param \dots additional arguments that are ignored.
#' @return Logical value indicating whether the iterator has a next element.
#' @keywords methods
#' @examples
#'
#'   it <- ihasNext(iter(c('a', 'b', 'c')))
#'   while (hasNext(it))
#'     print(nextElem(it))
#'
#' @export hasNext
hasNext <- function(obj, ...) {
  UseMethod('hasNext')
}
