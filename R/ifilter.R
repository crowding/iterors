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



#' Create a filtering iterator
#'
#' The \code{ifilter} and \code{ifilterfalse} functions create iterators that
#' return a subset of the values of the specified iterable. \code{ifilter}
#' returns the values for which the \code{pred} function returns \code{TRUE},
#' and \code{ifilterfalse} returns the values for which the \code{pred}
#' function returns \code{FALSE}.
#'
#'
#' @aliases ifilter ifilterfalse
#' @param pred A function that takes one argument and returns \code{TRUE} or
#' \code{FALSE}.
#' @param iterable The iterable to iterate over.
#' @keywords utilities
#' @examples
#'
#' # Return the odd numbers between 1 and 10
#' as.list(ifilter(function(x) x %% 2 == 1, icount(10)))
#'
#' # Return the even numbers between 1 and 10
#' as.list(ifilterfalse(function(x) x %% 2 == 1, icount(10)))
#'
#' @export ifilter
ifilter <- function(pred, iterable) {
  it <- iteror(iterable)

  nextOr_ <- function(or) {
    repeat {
      val <- nextOr(it, return(or))
      if (pred(val)) {
        return(val)
      }
    }
  }

  iteror.function(nextOr_)
}

ifilterfalse <- function(pred, iterable) {
  it <- iteror(iterable)

  nextOr_ <- function(or) {
    repeat {
      val <- nextOr(it, return(or))
      if (! pred(val)) {
        return(val)
      }
    }
  }

  iteror.function(nextOr_)
}
