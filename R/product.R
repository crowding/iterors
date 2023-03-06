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



#' Create a cartesian product iterator
#'
#' Create an iterator that returns values from multiple iterators in cartesian
#' product fashion.  That is, they are combined the manner of nested \code{for}
#' loops.
#'
#'
#' @param \dots Named iterables to iterate over.  The right-most iterables
#' change more quickly, like an odometer.
#' @keywords utilities
#' @examples
#'
#' # Simulate a doubly-nested loop with a single while loop
#' it <- product(a=1:3, b=1:2)
#' repeat {
#'   x <- nextOr(it, break)
#'   cat(sprintf('a = %d, b = %d\n', x$a, x$b))
#' }
#'
#' @export product
product <- function(...) {  # XXX: this use of substitute is goofy af.
                            # why not use recycle?
  args <- substitute(list(...))[-1]
  n <- length(args)
  anames <- names(args)
  if (is.null(anames)) {
    anames <- rep('', n)
  }
  env <- parent.frame()
  product.internal(n, args, anames, env)
}

product.internal <- function(n, args, anames, env) {
  if (n <= 1) {
    if (n == 1) {
      icar <- iteror(eval(args[[1]], envir=env))

      nextOr_ <- function(or) {
        carval <- list(nextOr(icar, return(or)))
        names(carval) <- anames[1]
        carval
      }
    } else {
      nextOr_ <- function(or) or
    }
  } else {
    icdr <- product.internal(n - 1, args[-n], anames[-n], env)
    cdrval <- NULL
    needval <- TRUE
    icar <- NULL

    nextOr_ <- function(or) {
      repeat {
        if (needval) {
          cdrval <<- nextOr(icdr, return(or))
          needval <<- FALSE
          icar <<- iteror(eval(args[[n]], envir=env))
        }

        carval <- list(nextOr(icar, {
          needval <<- TRUE
          next
        }))
        break
     }

      names(carval) <- anames[n]
      c(cdrval, carval)
    }
  }

  iteror.function(nextOr_)
}
